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
import Data.Bits
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.HashTable as T
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
    Http String String Int String [EventSpec] |
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

data ConfigSt = ConfigSt {
    raw :: Config,
    encodeInput :: String -> String,
    patterns :: ConfigPatterns,
    perms :: M.Map String [AllowSpec],
    spoke :: T.HashTable String String,
    ranks :: T.HashTable String Int,
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
     do l <- fmap lines (readFile fn)
        n <- randomRIO (0, length l - 1)
        format (l !! n)
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

infixl 7 &
(&) :: Char -> Int -> Int
c & i = ord c .&. i

utf8Decode :: String -> String
utf8Decode s@(a:t'@(b:t)) =
    if ac .&. 0xfc == 0xc0 && bc .&. 0xc0 == 0x80 then
        chr (shiftL (ac .&. 3) 6 .|. (bc .&. 0x3f)):utf8Decode t
    else if ac .&. 0x80 == 0 then a:utf8Decode t' else s
  where ac = ord a
        bc = ord b
utf8Decode s = s

{-
 - Encodes non-utf-8 bytes as utf-8.
 - Utf-8 sequences are left untouched.
 -}
utf8Encode :: String -> String

utf8Encode "" = ""
-- 7bit ASCII - don't modify
utf8Encode (a:t) | a & 0x80 == 0
    = a : utf8Encode t
-- 2-byte utf-8 sequence
utf8Encode (a:b:t) | a & 0xe0 == 0xc0 && b & 0xc0 == 0x80
    = a : b : utf8Encode t
-- 3-byte utf-8 sequence
utf8Encode (a:b:c:t) | a & 0xf0 == 0xe0 && b & 0xc0 == 0x80 &&
                       c & 0xc0 == 0x80
    = a : b : c : utf8Encode t
-- 4-byte utf-8 sequence
utf8Encode (a:b:c:d:t) | a & 0xf8 == 0xf0 && b & 0xc0 == 0x80 &&
                        c & 0xc0 == 0x80 && d & 0xc0 == 0x80
    = a : b : c : d : utf8Encode t
-- Invalid byte, treat as latin-1 and encode into 2-byte utf-8
utf8Encode (a:t) =
    chr (0xc0 .|. shiftR v 6) : chr (0x80 .|. v .&. 0x3f) : utf8Encode t
  where v = ord a

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
        liftIO $ hClose inp
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
seenMsg nick said =
     do cfg <- ircConfig
        liftIO (T.update (spoke cfg) (lower nick) said >> return ())

appendSeen :: [(String, Bool)] -> Bot ()
appendSeen nicks =
     do st <- fmap spoke ircConfig
        TOD t' _ <- liftIO getClockTime
        let t = show t'
            format (nick, alive) =
             let key = lower nick in
             do said <- fmap (fromMaybe "") (T.lookup st key)
                T.delete st key
                return (nick ++ '\t':(if alive then '+':t else t) ++ '\t':said)
        liftIO (mapM format nicks >>= appendFile "seen.dat" . unlines)

updateSeen what nick _ =
     do appendSeen [(nick, what)]
        unless what (ircConfig >>= liftIO . (`T.delete` nick) . ranks)
seenEvent "JOIN" = updateSeen True
seenEvent "PART" = updateSeen False
seenEvent "QUIT" = updateSeen False
seenEvent "NICK" = \old (new:_) ->
                     do appendSeen [(old, False), (new, True)]
                        t <- fmap ranks ircConfig
                        liftIO $ do rank <- T.lookup t old
                                    T.delete t old
                                    T.update t new (fromMaybe 0 rank)
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
        checkMode ranks (c:t) =
            T.update ranks t (modeRank c) >> return ()
        checkMode _ _ = return ()
        register names =
             do appendSeen (map stripTag names)
                ircConfig >>= liftIO . (`mapM_` names) . checkMode . ranks
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
        setRank who rank =
             do ranks <- fmap ranks ircConfig
                old <- fmap (fromMaybe 0) $ liftIO $ T.lookup ranks who
                let rank' = if rank == 0 || old < rank then rank else old
                putLog ("setRank " ++ who ++ " = " ++ show rank')
                liftIO $ T.update ranks who rank'
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
        liftIO $ do hSetBuffering inp LineBuffering
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
             do rank <- liftIO $ T.lookup (ranks cfg) nick
                return $ maybe False (>= expectRank) rank
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
                        when (cmd == "PRIVMSG") (seenMsg from what)
                        when (args /= [] && isPrefixOf "!" what)
                             (putLog $ "NOMATCH " ++ showMsg msg)
        execute param event =
            case event of
            Send evCmd evArg -> ircSend "" evCmd (map param evArg)
            Say text      -> mapM_ (say replyTo) (lines $ param text)
            SayTo to text -> mapM_ (say $ param to) (lines $ param text)
            Perm perm     -> requirePerm from prefix perm
            Join channel  -> ircSend "" "JOIN" [param channel]
            Quit msg      -> quitIrc (param msg)
            RandLine fn   -> liftIO (randLine fn) >>= say replyTo . param
            Rehash        -> killPlugins >>
                ircConfig >>= liftIO . getConfig . ranks >>= ircSetConfig
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

getConfig ranks =
     do args <- getArgs
        s <- readFile (fromMaybe "hircrc" $ listToMaybe args)
        spoke <- T.new (==) T.hashString
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
            spoke = spoke,
            ranks = ranks
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
        ranks <- T.new (==) T.hashString
        config <- getConfig ranks
        let cfg = raw config
        reconnect $ connectIrc (host cfg) (port cfg) (nick cfg) bot config
