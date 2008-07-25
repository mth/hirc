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
import qualified Data.ByteString.Char8 as C
import Data.Bits
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.HashTable as T
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Concurrent
import qualified Control.Exception as E
import Text.Regex
import System.Locale
import System.Time
import System.Random
import System.Posix.Signals
import System.Process
import System.IO
import qualified Network.HTTP as H
import Network.URI
import Debug.Trace

data EventSpec =
    Send String [String] | Say String | SayTo String String |
    Join String | Quit String | Perm String | RandLine String |
    Seen String String [String] | Exec String [String] |
    Plugin [String] String | Http String String Int String [EventSpec] |
    Append String String | Rehash
    deriving Read

data AllowSpec = Client Regex | Group String

data Config = Config {
    host :: String,
    port :: Integer,
    nick :: String,
    messages :: [(String, [EventSpec])],
    commands :: [(String, [String], [EventSpec])],
    permits  :: [(String, [String])],
    nopermit :: [EventSpec]
} deriving (Read)

data PluginId = ExecPlugin [String]
    deriving (Show, Eq, Ord)

data PluginCmd = PluginMsg String String | KillPlugin

type Bot a = Irc ConfigSt a

data ConfigSt = ConfigSt {
    raw :: Config,
    patterns :: M.Map String [([Regex], [EventSpec])],
    perms :: M.Map String [AllowSpec],
    seen  :: T.HashTable String (Integer, Bool, String, C.ByteString),
    -- plugin is just a synchronized IO output, where to send matching commands
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

randLine fn =
     do l <- fmap lines (readFile fn)
        n <- randomRIO (0, length l - 1)
        return $ l !! n

dropPath p = if s == "" then p else dropPath (tail s)
  where s = dropWhile (/= '/') p

putLog = liftIO . putStrLn
lower = map toLower

utfDecode s@(a:t'@(b:t)) =
    if ac .&. 0xfc == 0xc0 && bc .&. 0xc0 == 0x80 then
        chr (shiftL (ac .&. 3) 6 .|. (bc .&. 0x3f)):utfDecode t
    else if ac .&. 0x80 == 0 then a:utfDecode t' else s
  where ac = ord a
        bc = ord b
utfDecode s = s

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
splitEntry line =
  case span (/= '\t') line of
  (key, _:rest) -> (case span (/= '\t') rest of
                    (value, _:hist) -> (key, value, hist)
                    (value, _) -> (key, value, ""))
  (key, _) -> (key, "", "")

clockSeconds = getClockTime >>= \(TOD t _) -> return t

readSeen =
     do st <- T.new (==) T.hashString
        let add l =
                let l' = C.unpack l
                    (nick, a, said) = splitEntry l' in
                E.catch (do let t = read a
                            t `seq` T.update st (lower nick)
                                                (t, False, nick, C.pack said)
                            return ())
                        (\e -> putStrLn (show e ++ " (" ++ l' ++ ")"))
        dat <- catch (C.readFile "seen.dat")
                     (\e -> print e >> return C.empty)
        mapM_ add (C.lines dat)
        return st

writeSeen alive nicks =
     do st <- fmap seen ircConfig
        liftIO $ do t <- clockSeconds
                    let getSaid (_, _, _, said) = said
                        add nick =
                         do let key = lower nick
                            said <- fmap (maybe C.empty getSaid)
                                         (T.lookup st key)
                            T.update st key (t, alive, nick, said)
                    mapM_ add nicks
                    T.toList st >>= writeFile "seen.dat" . foldl' line ""
  where line r (_, (t, _, k, s)) = k ++ '\t':(show t) ++
                                     '\t':(C.unpack s) ++ '\n':r

seenMsg nick said =
    do st <- fmap seen ircConfig
       liftIO $ do let getTime (t, _, _, _) = return t
                   t <- T.lookup st key >>= maybe clockSeconds getTime
                   T.update st key (t, True, nick, C.pack said)
                   return ()
  where key = lower nick

updateSeen what nick _ = writeSeen what [nick]
seenEvent "JOIN" = updateSeen True
seenEvent "PART" = updateSeen False
seenEvent "QUIT" = updateSeen False
seenEvent "NICK" = \old new -> writeSeen False [old] >> writeSeen True new
seenEvent "353" = const $ writeSeen True . map stripTag . words . last
    where stripTag s@(c:t) =
            if c == ' ' || c == '+' || c == '@' || c == '%' then t else s
          stripTag s = s
seenEvent _ = \_ _ -> return ()

seenQuery :: String -> String -> String -> String -> [String] -> Bot ()
seenQuery from to q format [unknown, hasBeen, here, mySelf, yourSelf] =
    if q == "" then fail "NOPERM" else
    case splitAt (length q - 1) (lower q) of
    ('*':s, "*") -> find (isInfixOf s)
    (s, "*") -> find (isPrefixOf s)
    _ -> case lower q of
         '*':q -> find (isSuffixOf q)
         q -> ircConfig >>= liftIO . (`T.lookup` q) . seen
                        >>= msg . fromMaybe none
  where none = (0, False, "", C.empty)
        find f = ircConfig >>= liftIO . T.toList . seen
                           >>= msg . foldl' (chk f) none
        chk f r@(t', _, _, _) (k, v@(t, _, _, _)) =
            if t > t' && f k then v else r
        msg (0, False, "", _) = say to (bindArg "" [from, q] unknown)
        msg (t, a, k, said) =
            do now <- liftIO clockSeconds
               me <- myIrcNick
               let said' = C.unpack said
               say to (bindArg "" [from, k, ts (now - t),
                                   if said' == "" then "N/A" else said']
                                  (if not a then hasBeen
                                   else if k == from then yourSelf
                                   else if k == me then mySelf else here))
        ts t = let f ('$':'d':s) = show (t `div` 86400) ++ f s
                   f ('$':'h':s) = show (t `div` 3600 `mod` 24) ++ f s
                   f ('$':'m':s) = show (t `div` 60 `mod` 60) ++ f s
                   f ('$':'s':s) = show (t `mod` 60) ++ f s
                   f (c:s) = c:f s
                   f "" = "" in f format
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
requirePerm :: String -> String -> Bot ()
requirePerm prefix perm =
     do cfg <- ircConfig
        let hasPerm = maybe False (any checkPerm) . (`M.lookup` perms cfg)
            checkPerm (Client re) = (matchRegex re prefix) /= Nothing
            checkPerm (Group group) = hasPerm group
        unless (hasPerm perm) (fail "NOPERM")

bot :: (String, String, [String]) -> Bot ()
bot msg@(prefix, cmd, args') =
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
            Perm perm     -> requirePerm prefix perm
            Join channel  -> ircSend "" "JOIN" [param channel]
            Quit msg      -> quitIrc (param msg)
            RandLine fn   -> liftIO (randLine fn) >>= say replyTo . param
            Rehash        -> do oldCfg <- ircConfig
                                killPlugins
                                cfg <- liftIO getConfig
                                ircSetConfig cfg { seen = seen oldCfg }
            Exec prg args -> execSys replyTo prg (map param args)
            Plugin prg cmd -> invokePlugin (ExecPlugin prg) replyTo (param cmd)
            Http uri body maxb pattern events ->
                httpGet from (param uri) (param body) maxb
                        (mkRegexWithOpts pattern False False)
                        (\param -> mapM_ (execute param) events)
            Seen nick format args ->
                seenQuery from replyTo (param nick) format args
            Append file str -> liftIO $ appendFile file (param str)
        atErr "NOPERM" = ircConfig >>=
                mapM_ (execute $ bindArg prefix [from, from]) . nopermit . raw
        atErr str = putLog str >> ircSend from "NOTICE" [from, str]
        replyTo = case args of
                  (s@('#':_)):_ -> s
                  _ -> from
        from = takeWhile (/= '!') prefix
        args = map utfDecode args'

getConfig = do s <- readFile "hircrc"
               seen <- readSeen
               return $! prepareConfig seen $! read (rmComments s)

rmComments = (flip $ subRegex (mkRegex "(^|\n)\\s*#[^\n]*")) ""

preparePattern = subst "\\*" ".*" . subst "\\." "\\."
    where subst pattern text = (flip $ subRegex (mkRegex pattern)) text

prepareConfig :: T.HashTable String (Integer, Bool, String, C.ByteString)
                    -> Config -> ConfigSt
prepareConfig seen cfg = ConfigSt {
    raw = cfg,
    patterns = foldr addCmd M.empty (commands cfg ++
                 map (\(pattern, event) -> ("PRIVMSG", ["", pattern], event))
                     (messages cfg)),
    perms = foldr addPerm M.empty (permits cfg),
    plugins = M.empty,
    seen = seen
} where addCmd (cmd, args, event) = let bind = (map mkRegex args, event) in
                                    M.alter (Just . maybe [bind] (bind:)) cmd
        addPerm (perm, users) = let perms = map getPerm users in
                                M.alter (Just . maybe perms (perms ++)) perm
        getPerm (':':group) = Group group
        getPerm user = Client (mkRegex ('^':preparePattern user ++ "$"))

reconnect connect =
    catch connect (\ex -> do putStrLn ("Error occured: " ++ show ex)
                             threadDelay 60000000
                             putStrLn "Reconnecting..."
                             reconnect connect)
main = 
     do installHandler sigPIPE Ignore Nothing -- stupid ghc runtime
        config <- getConfig
        let cfg = raw config
        reconnect $ connectIrc (host cfg) (port cfg) (nick cfg) bot config
