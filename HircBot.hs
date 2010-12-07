{-
 - HircBot - Simple IRC bot in haskell.
 - Copyright (C) 2008  Madis Janson
 -
 - This file is part of HircBot.
 - 
 - HircBot is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 - 
 - HircBot is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 - 
 - You should have received a copy of the GNU General Public License
 - along with HircBot.  If not, see <http://www.gnu.org/licenses/>.
 -}
import Hirc
import Utf8Conv
import Calculator
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.HashTable as T
import qualified Data.ByteString.Char8 as C
import Control.Monad
import Control.Concurrent
import Text.Regex
import System.Environment
import System.Time
import System.Random
import System.Posix.Signals
import System.Process
import System.IO
import qualified Network.HTTP as H
import Network.URI

data EncodingSpec = Utf8 | Latin1 | Raw
    deriving Read

data EventSpec =
    Send String [String] | Say String | SayTo String String |
    Join String | Quit String | Perm String | RandLine String |
    Exec String [String] | Plugin [String] String |
    Http String String Int String [EventSpec] | Calc String |
    Append String String | Rehash
    deriving Read

data AllowSpec = Client Regex | Group String

data Config = Config {
    host :: String,
    port :: Integer,
    nick :: String,
    encoding :: EncodingSpec,
    messages :: [(String, [EventSpec])],
    commands :: [(String, [String], [EventSpec])],
    permits  :: [(String, [String])],
    nopermit :: [EventSpec]
} deriving (Read)

data PluginId = ExecPlugin [String]
    deriving (Show, Eq, Ord)

data PluginCmd = PluginMsg String String | KillPlugin

type Bot a = Irc ConfigSt a
type ConfigPatterns = M.Map String [([Regex], [EventSpec])]

data User = User {
    rank :: Int,
    spoke :: C.ByteString
} deriving Eq

data ConfigSt = ConfigSt {
    raw :: Config,
    encodeInput :: String -> String,
    patterns :: ConfigPatterns,
    perms :: M.Map String [AllowSpec],
    users :: T.HashTable String User,
    plugins :: M.Map PluginId (PluginCmd -> Bot ())
}

bindArg :: String -> [String] -> String -> String
bindArg prefix bindings str =
    let (start, rest) = span (/= '$') str in
    case rest of
    "" -> start
    '$':':':r -> start ++ prefix ++ bindArg prefix bindings r
    _ -> let (numStr, rest') = span isNumber (tail rest) in
         let num = read numStr in
         start ++ (if num < length bindings then bindings!!num else '$':numStr)
               ++ bindArg prefix bindings rest'

randLine :: String -> IO String
randLine fn =
     do l <- fmap C.lines (C.readFile fn)
        n <- randomRIO (0, length l - 1)
        format $ C.unpack $ l !! n
  where format ('{':t) = snippet t "" []
        format (c:t) = fmap (c:) (format t)
        format "" = return ""
        snippet ('|':t) a r = snippet t "" (reverse a : r)
        snippet ('}':t) a r =
             do let l = reverse a : r
                n <- randomRIO (0, length l - 1)
                rest <- format t
                return $ (l !! n) ++ rest
        snippet (c:t) a r = snippet t (c:a) r
        snippet "" a r = snippet "}" a r -- someone forget to add '}'?

dropPath p = if s == "" then p else dropPath (tail s)
  where s = dropWhile (/= '/') p

putLog = liftIO . putStrLn
lower = map toLower

{-
 - HTTP
 -}
httpGet from uriStr body maxb re action =
     do uri <- maybe (fail $ "Bad URI: " ++ uriStr) return (parseURI uriStr)
        unlift <- escape
        let hdr = [H.Header H.HdrRange ("bytes=0-" ++ show maxb)]
            rq = if body == "" then H.Request uri H.GET hdr ""
                 else H.Request uri H.POST (H.Header H.HdrContentLength
                                                (show $ length body):hdr) body
        liftIO $ forkIO $ catch
            (do rsp <- H.simpleHTTP rq >>= either (fail . show) return
                let code = H.rspCode rsp
                when (code /= (2, 0, 0) && code /= (2,0,6)) (fail $ show $ code)
                unlift $ maybe (putLog $ "HTTP NOMATCH: " ++ uriStr)
                               (action . bindArg "" . (from:))
                               (matchRegex re $ take maxb $ H.rspBody rsp))
            (\e -> print $ "HTTP " ++ uriStr ++ ": " ++ show e)
        return ()

{-
 - EXEC
 -}
execSys :: String -> String -> [String] -> Bot ()
execSys to prog argv =
     do (inp, out, err, pid) <-
            liftIO $ runInteractiveProcess prog argv Nothing Nothing
        liftIO (do hClose inp
                   hSetEncoding out latin1
                   hSetEncoding err latin1)
        sayTo <- fmap (. say to) escape
        let copy h = liftIO $ forkIO $
             do l <- fmap lines (hGetContents h)
                mapM_ sayTo (take 50 l)
                when (drop 50 l /= []) (sayTo "...")
                hClose h
        copy out
        copy err
        liftIO $ forkIO $ -- termination guard
            do threadDelay 30000000
               dead <- getProcessExitCode pid
               when (dead == Nothing) $
                     do terminateProcess pid
                        sayTo ("Terminated " ++ dropPath prog)
                        waitForProcess pid
                        return ()
        return ()

{-
 - SEEN
 -}
getUser :: String -> Bot (Maybe User)
getUser nick = ircConfig >>= liftIO . (`T.lookup` (lower nick)) . users

updateUser :: (Maybe User -> Maybe User) -> String -> Bot (Maybe User)
updateUser f nick = ircConfig >>= liftIO . update . users
  where key = lower nick
        update t = do
            old <- T.lookup t key
            let new = f old
            when (old /= new) (maybe (T.delete t key) (set t) new)
            return new
        set t v = T.update t key v >> return ()

updateRank :: (Int -> Int) -> String -> Bot (Maybe User)
updateRank f nick = updateUser update nick
  where update u = let r = f (maybe 0 rank u)
                       s = maybe C.empty spoke u in
                   if r == 0 && C.null s then Nothing
                                         else Just (User {rank = r, spoke = s})

seenMsg nick said =
    updateUser (\u -> Just (User {rank = maybe 0 rank u, spoke = said})) nick
        >> return ()

appendSeen :: [(String, Bool)] -> Bot ()
appendSeen nicks =
     do TOD t _ <- liftIO getClockTime
        mapM (format (show t)) nicks >>=
            liftIO . appendFile "seen.dat" . unlines
  where clear = (>>= \u -> return $ u {spoke = C.empty})
        format :: String -> (String, Bool) -> Bot String
        format t (nick, alive) =
         do user <- updateUser (if alive then const Nothing else clear) nick
            let said = maybe "" (C.unpack . spoke) user
            return (nick ++ '\t':(if alive then '+':t else t) ++ '\t':said)

updateSeen what nick _ =
     appendSeen [(nick, what)]
seenEvent "JOIN" = updateSeen True
seenEvent "PART" = updateSeen False
seenEvent "QUIT" = updateSeen False
seenEvent "NICK" = \old (new:_) ->
                     do user <- getUser old
                        appendSeen [(old, False), (new, True)]
                        updateUser (const user) new
                        return ()
seenEvent "KICK" = \_ args -> case args of
                              (_:nick:_) -> updateSeen False nick []
                              _ -> return ()
seenEvent "353" = const $ register . words . last
  where stripTag s@(c:t) =
           (if c == ' ' || c == '+' || c == '@' || c == '%' then t else s, True)
        stripTag s = (s, True)
        modeRank '@' = 3
        modeRank '%' = 2
        modeRank '+' = 1
        modeRank _ = 0
        checkMode (c:t) = updateRank (const (modeRank c)) t >> return ()
        checkMode _ = return ()
        register names =
             do appendSeen (map stripTag names)
                mapM_ checkMode names

-- track mode changes for maintaining ranks
seenEvent "MODE" = const atMode
  where atMode (_:m:args) = modes False m args
        atMode _ = return ()
        modes _ ('+':m) args = modes True m args
        modes _ ('-':m) args = modes False m args
        modes set (c:m) args = mode set c args >>= modes set m
        modes _ _ _ = return ()
        mode _ c (_:args) | elem c "belkIR" = return args
        mode set c args'@(who:args) =
            case elemIndex c "vho" of
            Just rank -> do setRank who (if set then rank + 1 else 0)
                            return args
            Nothing -> return args'
        mode _ _ _ = return []
        setRank who rank' =
             do user <- updateRank (if rank' == 0 then const 0
                                                  else max rank') who
                putLog ("setRank " ++ who ++ " = " ++ show (maybe 0 rank user))
seenEvent _ = \_ _ -> return ()

{-
 - PLUGIN
 -}
removePlugin id =
     do cfg <- ircConfig
        ircSetConfig cfg { plugins = M.delete id (plugins cfg) }

killPlugins = ircConfig >>= mapM_ ($ KillPlugin) . M.elems . plugins

startPlugin :: PluginId -> String -> Bot (PluginCmd -> Bot ())
startPlugin id@(ExecPlugin (prog:argv)) replyTo =
     do to <- liftIO $ newMVar replyTo
        unlift <- escape
        (inp, out, err, pid) <-
            liftIO $ runInteractiveProcess prog argv Nothing Nothing
        let sayTo s = readMVar to >>= unlift . (`say` s)
            output = do catch (hGetContents out >>= mapM_ sayTo . lines) print
                        unlift $ removePlugin id
                        hClose inp
                        waitForProcess pid
                        return ()
            kill = removePlugin id >> liftIO (terminateProcess pid)
            handler KillPlugin = kill
            handler (PluginMsg replyTo msg) = liftIO $ catch
                (swapMVar to replyTo >> hPutStrLn inp msg)
                (\e -> putStrLn (show id ++ ": " ++ show e) >>
                       unlift (kill >>
                                say replyTo "\^AACTION has a ghost plugin\^A"))
        liftIO $ do hSetEncoding inp latin1
                    hSetEncoding out latin1
                    hSetBuffering inp LineBuffering
                    forkIO $ output
                    forkIO $ hGetContents err >>= putStr
        return handler

startPlugin id _ = fail ("Illegal plugin id: " ++ show id)

invokePlugin :: PluginId -> String -> String -> Bot ()
invokePlugin id to msg =
    ircConfig >>= maybe start ($ PluginMsg to msg) . M.lookup id . plugins
  where start = do p <- startPlugin id to
                   cfg <- ircConfig
                   ircSetConfig cfg { plugins = M.insert id p (plugins cfg) }
                   p (PluginMsg to msg)
{-
 - CORE
 -}
requirePerm :: String -> String -> String -> Bot ()
requirePerm nick prefix perm =
     do cfg <- ircConfig
        let hasPerm "+" = hasRank 1
            hasPerm "%" = hasRank 2
            hasPerm "@" = hasRank 3
            hasPerm perm =
                anyPerm $ concat $ maybeToList $ M.lookup perm (perms cfg)
            anyPerm (perm:rest) =
             do ok <- case perm of
                      Client re -> return $ (matchRegex re prefix) /= Nothing
                      Group group -> hasPerm group
                if ok then return True else anyPerm rest
            anyPerm [] = return False
            hasRank expectRank =
                getUser nick >>= return . maybe False ((>= expectRank) . rank)
        ok <- hasPerm perm
        unless ok (fail "NOPERM")

-- wrapper that encodes irc input into desired charset
bot :: (String, String, [String]) -> Bot ()
bot (prefix, cmd, args) =
     do cfg <- ircConfig
        bot' (prefix, cmd, map (encodeInput cfg) args)

bot' :: (String, String, [String]) -> Bot ()
bot' msg@(prefix, cmd, args) =
    ircCatch (seenEvent cmd from args)
             (putLog . (("seenEvent " ++ cmd ++ ": ") ++) . show) >>
    ircCatch (ircConfig >>= maybe (liftIO $ print msg) doMatch
                            . M.lookup cmd . patterns) atErr
  where doMatch ((argPattern, events):rest) =
            if length args < length argPattern then doMatch rest
            else maybe (doMatch rest)
                  (\p -> mapM_ (execute $ bindArg prefix . (from:) . (++ [from])
                                        $ concat p) events)
                  (sequence $ zipWith matchRegex argPattern args)
        doMatch [] = do let what = last args
                        when (cmd == "PRIVMSG") (seenMsg from $ C.pack what)
                        when (args /= [] && isPrefixOf "!" what)
                             (putLog $ "NOMATCH " ++ showMsg msg)
        execute param event =
            case event of
            Send evCmd evArg -> ircSend "" evCmd (map param evArg)
            Say text      -> mapM_ reply (lines $ param text)
            SayTo to text -> mapM_ (say $ param to) (lines $ param text)
            Perm perm     -> requirePerm from prefix perm
            Join channel  -> ircSend "" "JOIN" [param channel]
            Quit msg      -> quitIrc (param msg)
            RandLine fn   -> liftIO (randLine fn) >>= reply . param
            Calc text     -> ircCatch (reply $ calc $ param text) reply
            Rehash        -> killPlugins >>
                ircConfig >>= liftIO . getConfig . users >>= ircSetConfig
            Exec prg args -> execSys replyTo prg (map param args)
            Plugin prg cmd -> invokePlugin (ExecPlugin prg) replyTo (param cmd)
            Http uri body maxb pattern events ->
                httpGet from (param uri) (param body) maxb
                        (mkRegexWithOpts pattern False False)
                        (\param -> mapM_ (execute param) events)
            Append file str -> liftIO $ appendFile file (param str)
        atErr "NOPERM" = ircConfig >>=
                mapM_ (execute $ bindArg prefix [from, from]) . nopermit . raw
        atErr str = putLog str >> ircSend from "NOTICE" [from, str]
        replyTo = case args of
                  (s@('#':_)):_ -> s
                  _ -> from
        reply = say replyTo
        from = takeWhile (/= '!') prefix

createPatterns :: Config -> ConfigPatterns
createPatterns cfg = foldr addCmd M.empty
    (commands cfg ++
        map (\(pattern, event) -> ("PRIVMSG", ["", pattern], event))
            (messages cfg))
  where addCmd (cmd, args, event) = let bind = (map mkRegex args, event) in
                                    M.alter (Just . maybe [bind] (bind:)) cmd

preparePermPattern = subst "\\*" ".*" . subst "\\." "\\."
    where subst pattern text = (flip $ subRegex (mkRegex pattern)) text

getConfig users =
     do args <- getArgs
        s <- fmap C.unpack $ C.readFile (fromMaybe "hircrc" $ listToMaybe args)
        cfg <- return $! read (rmComments s)
        return $! ConfigSt {
            raw = cfg,
            encodeInput = case encoding cfg of
                          Utf8 -> utf8Encode
                          Latin1 -> utf8Decode
                          Raw -> id,
            patterns = createPatterns cfg,
            perms = foldr addPerm M.empty (permits cfg),
            plugins = M.empty,
            users = users
        }
  where addPerm (perm, users) = let perms = map getPerm users in
                                M.alter (Just . maybe perms (perms ++)) perm
        getPerm (':':group) = Group group
        getPerm user = Client (mkRegex ('^':preparePermPattern user ++ "$"))
        rmComments = (flip $ subRegex (mkRegex "(^|\n)\\s*#[^\n]*")) ""

reconnect connect =
    appendFile "seen.dat" "\n" >>
    catch connect (\ex -> do putStrLn ("Reconnect after 1 min: Error occured: "
                                       ++ show ex)
                             threadDelay 60000000
                             putStrLn "Reconnecting..."
                             reconnect connect)
main = 
     do installHandler sigPIPE Ignore Nothing -- stupid ghc runtime
        users <- T.new (==) T.hashString
        config <- getConfig users
        let cfg = raw config
        reconnect $ connectIrc (host cfg) (port cfg) (nick cfg) bot config
