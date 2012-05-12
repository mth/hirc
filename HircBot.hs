{-
 - HircBot - Simple IRC bot in haskell.
 - Copyright (C) 2008-2011  Madis Janson
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
import Data.Array (elems)
import Data.Char
import Data.IORef
import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.HashTable as H
import qualified Data.ByteString.Char8 as C
import Control.Monad
import Control.Concurrent
import Text.Regex.Posix
import System.Environment
import System.Exit
import System.Time
import System.Random
import System.Posix.IO
import System.Posix.Signals
import System.Posix.Process
import System.Posix.Types
import System.IO
import qualified Network.HTTP as H
import Network.URI

data EncodingSpec = Utf8 | Latin1 | Raw
    deriving Read

data EventSpec =
    Send String [C.ByteString] |
    Say C.ByteString | SayTo C.ByteString C.ByteString |
    Join C.ByteString | Quit C.ByteString | Perm String |
    IfPerm String [EventSpec] [EventSpec] | RandLine String |
    Exec String [C.ByteString] | Plugin [String] C.ByteString |
    ExecMaxLines Int String [C.ByteString] |
    Http C.ByteString C.ByteString Int Regex [EventSpec] |
    Calc C.ByteString | Append String C.ByteString | Rehash |
    Call C.ByteString [C.ByteString]
    deriving Read

data AllowSpec = Client Regex | Group String

instance Read Regex where
    readsPrec _ ('/':(!s)) =
        let parse ('\\':'x':a:b:cs) acc =
                parse cs $! chr ((ord a - 48) * 16 + ord b - 48) : acc
            parse ('\\':'/':cs) acc = parse cs $! '/':acc
            parse ('/':'i':cs) acc = [(regex compIgnoreCase acc, cs)]
            parse ('/':cs) acc = [(regex 0 acc, cs)]
            parse (c:cs) acc = parse cs $! c:acc
            parse "" _ = []
            regex opt s = makeRegexOpts (opt + compExtended) execBlank
                            $! C.reverse $! C.pack s in
        parse s ""
    readsPrec x (c:cs) | isSpace c = readsPrec x cs
    readsPrec _ _ = []

data Config = Config {
    servers  :: [(String, Integer)],
    nick     :: String,
    encoding :: EncodingSpec,
    define   :: [(C.ByteString, [EventSpec])],
    messages :: [(Regex, [EventSpec])],
    commands :: [(String, [Regex], [EventSpec])],
    permits  :: [(String, [String])],
    nopermit :: [EventSpec]
} deriving Read

data PluginId = ExecPlugin [String]
    deriving (Show, Eq, Ord)

data PluginCmd = PluginMsg C.ByteString C.ByteString | KillPlugin

type Bot a = Irc ConfigSt a
type ConfigPatterns = M.Map String [([Regex], [EventSpec])]

data User = User {
    rank :: Int,
    spoke :: C.ByteString
}

data ConfigSt = ConfigSt {
    raw :: Config,
    encodeInput :: String -> String,
    patterns :: ConfigPatterns,
    aliasMap :: H.HashTable C.ByteString [EventSpec],
    perms :: M.Map String [AllowSpec],
    -- Map nick (Map channel User)
    users :: H.HashTable C.ByteString (M.Map C.ByteString User),
    plugins :: M.Map PluginId (PluginCmd -> Bot ())
}

hashByteString s = H.hashInt (C.foldl' (\m c -> 31 * m + ord c) 0 s)

matchRegex :: Regex -> C.ByteString -> Maybe [C.ByteString]
matchRegex re value =
    fmap (collect . drop 1 . elems) (matchOnce re value)
  where collect ((start, len) : rest) =
            C.take len (C.drop start value) : collect rest
        collect [] = []

bindArg :: C.ByteString -> [C.ByteString] -> C.ByteString -> C.ByteString
bindArg prefix bindings str = C.concat $! format str
  where format str =
            let (start, rest) = C.span (/= '$') str in
            if C.null rest then
                [start]
            else let !rest' = C.tail rest in
                if not (C.null rest') && C.head rest' == ':' then
                    start : prefix : format (C.tail rest')
                else case C.readInt rest' of
                    Just (i, r) | i >= 0 && i < length bindings ->
                        start : (bindings!!i) : format r
                    _ -> start : C.singleton '$' : format rest'

randLine :: String -> IO C.ByteString
randLine fn =
     do l <- fmap C.lines (C.readFile fn)
        n <- randomRIO (0, length l - 1)
        parts <- format (C.copy (l !! n))
        return $! C.concat parts
  where format t = let (before, after) = C.span (/= '{') t
                       (choice, rest) = C.span (/= '}') after in
                   if C.null after then return [t]
                        else do !s <- snippet (C.tail choice) []
                                !r <- format (C.drop 1 rest)
                                return $ before : s : r
        snippet t a =
            let (before, after) = C.span (/= '|') t in
            if C.null after then
                 do n <- randomRIO (0, length a)
                    return $! (before : a) !! n
                else snippet (C.tail after) (before : a)

dropPath p = if s == "" then p else dropPath (tail s)
  where s = dropWhile (/= '/') p

putLog = liftIO . putStrLn
cPutLog s l = liftIO $ C.putStrLn $ C.concat (C.pack s : l)
lower = C.map toLower

{-
 - HTTP
 -}
httpGet uriStr body maxb re action =
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
                unlift $ maybe (putLog $! "HTTP NOMATCH: " ++ uriStr) action
                               (matchRegex re $! C.take maxb $ C.pack
                                                $ H.rspBody rsp))
            (\e -> print $! "HTTP " ++ uriStr ++ ": " ++ show e)
        return ()

{-
 - EXEC
 -}
sysProcess :: Maybe Fd -> String -> [String] -> IO (ProcessID, Handle)
sysProcess input prog argv =
     do (rd, wd) <- createPipe
        pid <- forkProcess (closeFd rd >> child wd)
        closeFd wd
        h <- fdToHandle rd
        hSetEncoding h latin1
        return (pid, h)
  where tryClose fd = catch (closeFd fd) (const (return ()))
        child wd =
         do dupTo wd stdOutput
            case input of
                Just inp -> do dupTo inp stdInput
                               closeFd inp
                Nothing ->  do dupTo wd stdError
                               closeFd stdInput
            closeFd wd
            mapM_ (tryClose . toEnum) [3 .. 255]
            catch (executeFile prog False argv Nothing)
                  (\e -> when (input /= Nothing)
                              (fdWrite stdError (show e ++ "\n") >> return ()))
            fdWrite stdOutput "dead plugin walking"
            exitImmediately (ExitFailure 127)

readInput :: Handle -> (C.ByteString -> IO Bool) -> IO () -> IO ()
readInput h f cleanup = catch copy (\_ -> hClose h >> cleanup)
    where copy = C.hGetLine h >>= f >>= (`when` copy)

execSys :: C.ByteString -> Int -> String -> [C.ByteString] -> Bot ()
execSys to maxLines prog argv =
     do unlift <- escape
        let filter l n = let r = take n l in return (n - length r, r)
        liftIO $ do (pid, h) <- sysProcess Nothing prog (map C.unpack argv)
                    v <- newMVar maxLines
                    let sayN s = do
                         unlift $ say' (liftIO . modifyMVar v . filter) to s
                         withMVar v (return . (> 0))
                    forkIO $ readInput h sayN (return ())
                    forkIO $ guard (unlift . say to) pid
                    return ()
  where kill sig pid next =
         do dead <- getProcessStatus False False pid
            when (dead == Nothing) $
                 do signalProcess sig pid
                    next
        guard sayTo pid =
         do threadDelay 30000000
            kill softwareTermination pid $
                 do sayTo (C.pack $ "Terminated " ++ dropPath prog)
                    threadDelay 1000000
                    kill killProcess pid
                         (getProcessStatus True False pid >> return ())

{-
 - SEEN
 -}
getUserMap :: C.ByteString -> Bot (M.Map C.ByteString User)
getUserMap nick =
     do cfg <- ircConfig
        res <- liftIO $ H.lookup (users cfg) (lower nick)
        return $! fromMaybe M.empty res

getUser :: C.ByteString -> C.ByteString -> Bot (Maybe User)
getUser channel nick =
    fmap (M.lookup channel) (getUserMap nick)

updateUserMap :: (M.Map C.ByteString User -> M.Map C.ByteString User)
                    -> C.ByteString -> Bot ()
updateUserMap f nick =
     do t <- fmap users ircConfig
        !m <- fmap (f . fromMaybe M.empty) $ liftIO $ H.lookup t k
        liftIO (if M.null m then H.delete t k
                            else H.update t k m >> return ())
 where k = lower nick

updateUser :: (Maybe User -> Maybe User)
                -> C.ByteString -> C.ByteString -> Bot ()
updateUser f !channel nick = updateUserMap (M.alter f channel) nick

updateRank :: (Int -> Int) -> C.ByteString -> C.ByteString -> Bot ()
updateRank f channel nick = updateUser update channel nick
  where update u = let !r = f (maybe 0 rank u)
                       !s = maybe C.empty spoke u in
                   if r == 0 && C.null s then emptyUser
                                         else Just (User {rank = r, spoke = s})

seenMsg (Just channel) nick !said = updateUser update channel nick
  where update u = let !r = maybe 0 rank u in
                   Just (User {rank = r, spoke = said})
seenMsg Nothing _ _ = return () -- private message

emptyUser = Just (User { rank = 0, spoke = C.empty })

-- XXX sharing seen.dat between channels is probably stupid, but whatever
appendSeen :: [(C.ByteString, Bool)] -> C.ByteString -> Bot ()
appendSeen nicks channel =
     do TOD t _ <- liftIO getClockTime
        mapM (format (show t)) nicks >>=
            liftIO . C.appendFile "seen.dat" . C.unlines
  where clear True (Just u) | rank u /= 0 = Just $ u {spoke = C.empty}
        clear True _ = emptyUser -- QUIT needs it
        clear _ _ = Nothing
        format t (nick, alive) =
         do user <- getUser channel nick
            updateUser (clear alive) channel nick
            let said = maybe C.empty spoke user
            return $! C.concat
                [nick, C.pack ('\t':(if alive then '+':t else t)), tab, said]
        tab = C.singleton '\t'

appendSeen' :: C.ByteString -> Bool -> C.ByteString -> Bot ()
appendSeen' nick alive channel = appendSeen [(nick, alive)] channel

seenEvent :: String -> C.ByteString -> [C.ByteString] -> Bot ()
seenEvent "JOIN" nick (channel:_)   = appendSeen' nick True channel
seenEvent "PART" nick (channel:_)   = appendSeen' nick False channel
seenEvent "KICK" _ (channel:nick:_) = appendSeen' nick False channel

seenEvent "QUIT" nick _ =
    getUserMap nick >>= mapM_ (appendSeen [(nick, False)]) . M.keys

seenEvent "NICK" old (new:_) =
     do user <- getUserMap old
        mapM_ (appendSeen [(old, False), (new, True)]) (M.keys user)
        updateUserMap (const user) new

seenEvent "353" _ args =
     do appendSeen (map stripTag nameList) channel
        mapM_ checkMode nameList
  where stripTag s =
            (if not (C.null s) && C.head s `elem` " +@%" then
                C.tail s else s, True)
        modeRank '@' = 3
        modeRank '%' = 2
        modeRank '+' = 1
        modeRank _ = 0
        checkMode s | C.null s = return ()
        checkMode s = updateRank (\_ -> modeRank $ C.head s) channel (C.tail s)
        (names:channel:_) = reverse args
        nameList = C.words names

-- track mode changes for maintaining ranks
seenEvent "MODE" _ (channel:m:args') = modes False (C.unpack m) args'
  where modes _ ('+':m) args = modes True m args
        modes _ ('-':m) args = modes False m args
        modes set (c:m) args = mode set c args >>= modes set m
        modes _ _ _ = return ()
        mode _ c (_:args) | elem c "belkIR" = return $! args
        mode set c args'@(who:args) =
            case elemIndex c "vho" of
            Just rank -> do setRank who (if set then rank + 1 else 0)
                            return $! args
            Nothing -> return $! args'
        mode _ _ _ = return []
        setRank who rank' =
             do updateRank (if rank' == 0 then const 0 else max rank')
                           channel who
                user <- getUser channel who
                cPutLog "setRank" [channel, C.singleton ' ', who,
                                   C.pack $ " = " ++ show (maybe 0 rank user)]

seenEvent "TERMINATE" _ _ = killPlugins

seenEvent _ _ _ = return ()

{-
 - PLUGIN
 -}
removePlugin id =
     do cfg <- ircConfig
        ircSetConfig cfg { plugins = M.delete id (plugins cfg) }

killPlugins = ircConfig >>= mapM_ ($! KillPlugin) . M.elems . plugins

startPlugin :: PluginId -> C.ByteString -> Bot (PluginCmd -> Bot ())
startPlugin id@(ExecPlugin (prog:argv)) replyTo =
     do to <- liftIO $ newMVar replyTo
        unlift <- escape
        (pid, inp, out) <- liftIO $
             do (inputRd, inputWd) <- createPipe
                (pid, fd) <- sysProcess (Just inputRd) prog argv
                closeFd inputRd
                h <- fdToHandle inputWd
                hSetEncoding h latin1
                hSetBuffering h LineBuffering
                return (pid, h, fd)
        let sayTo s = readMVar to >>= unlift . (`say` s) >> return True
            output = readInput out sayTo $
                         do unlift (removePlugin id)
                            hClose inp
                            getProcessStatus True False pid
                            return ()
            kill = removePlugin id >>
                    liftIO (signalProcess softwareTermination pid)
            handler KillPlugin = kill
            handler (PluginMsg replyTo msg) = liftIO $ catch
                (swapMVar to replyTo >> C.hPutStrLn inp msg)
                (\e -> putStrLn (show id ++ ": " ++ show e) >>
                       unlift (kill >> say replyTo ghost))
        liftIO $ forkIO $ output
        return handler
  where ghost = C.pack "\^AACTION has a ghost plugin\^A"

startPlugin id _ = fail ("Illegal plugin id: " ++ show id)

invokePlugin :: PluginId -> C.ByteString -> C.ByteString -> Bot ()
invokePlugin id to msg =
    ircConfig >>= maybe start ($! PluginMsg to msg) . M.lookup id . plugins
  where start = do p <- startPlugin id to
                   cfg <- ircConfig
                   ircSetConfig cfg { plugins = M.insert id p (plugins cfg) }
                   p (PluginMsg to msg)
{-
 - CORE
 -}
checkPerm :: Maybe C.ByteString -> C.ByteString -> C.ByteString
             -> String -> Bot Bool
checkPerm channel nick prefix perm =
     do cfg <- ircConfig
        let hasPerm "+" = hasRank 1
            hasPerm "%" = hasRank 2
            hasPerm "@" = hasRank 3
            hasPerm perm =
                anyPerm $! concat $! maybeToList $! M.lookup perm (perms cfg)
            anyPerm (perm:rest) =
             do ok <- case perm of
                      Client re -> return $! (matchOnce re prefix) /= Nothing
                      Group group -> hasPerm group
                if ok then return True else anyPerm rest
            anyPerm [] = return False
            hasRank expectRank =
                -- It's idiotic to give perm based on rank on any channel,
                -- but I don't know a better solution now for private chat
                case channel of
                Nothing -> fmap (any (\u -> rank u >= expectRank) . M.elems)
                                (getUserMap nick)
                Just ch -> fmap (maybe False ((>= expectRank) . rank))
                                (getUser ch nick)
        hasPerm perm

-- wrapper that encodes irc input into desired charset
bot :: (C.ByteString, String, [C.ByteString]) -> Bot ()
bot (prefix, cmd, args) =
     do cfg <- ircConfig
        bot' (prefix, cmd, map (C.pack . encodeInput cfg . C.unpack) args)

bot' :: (C.ByteString, String, [C.ByteString]) -> Bot ()
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
                        when (cmd == "PRIVMSG")
                             (seenMsg channel from $! what)
                        when (args /= [] && C.isPrefixOf (C.singleton '!') what)
                             (cPutLog "NOMATCH " [showMsg msg])
        execute param event =
            case event of
            Send evCmd evArg -> ircSend C.empty evCmd (map param evArg)
            Say text      -> mapM_ reply (C.lines $ param text)
            SayTo to text -> mapM_ (say $ param to) (C.lines $ param text)
            Call alias args ->
                let exec = execute (if null args then param else
                                        bindArg prefix (from:map param args)) in
                ircConfig >>= liftIO . (`H.lookup` alias) . aliasMap >>=
                maybe (error (C.unpack alias ++ " undefined")) (mapM_ exec)
            Perm perm     -> do ok <- checkPerm channel from prefix perm
                                unless ok (fail "NOPERM")
            IfPerm perm events evElse ->
                 do ok <- checkPerm channel from prefix perm
                    mapM_ (execute param) (if ok then events else evElse)
            Join channel  -> ircSend C.empty "JOIN" [param channel]
            Quit msg      -> quitIrc (param msg)
            RandLine fn   -> liftIO (randLine fn) >>= reply . param
            Calc text     ->
                let reply' = reply . C.pack in
                ircCatch (reply' $ calc $ C.unpack $ param text) reply'
            Rehash        -> killPlugins >>
                ircConfig >>= liftIO . getConfig . users >>= ircSetConfig
            Exec prg args -> execSys replyTo 50 prg (map param args)
            ExecMaxLines limit prg args ->
                execSys replyTo limit prg (map param args)
            Plugin prg cmd -> invokePlugin (ExecPlugin prg) replyTo (param cmd)
            Http uri body maxb pattern events ->
                httpGet (C.unpack $ param uri) (C.unpack $ param body) maxb
                        pattern (\param ->
                           mapM_ (execute $ bindArg prefix $ from:param) events)
            Append file str -> liftIO $ C.appendFile file (param str)
        atErr "NOPERM" = ircConfig >>=
                mapM_ (execute $! bindArg prefix [from, from]) . nopermit . raw
        atErr str = putLog str >> ircSend from "NOTICE" [from, C.pack str]
        replyTo = fromMaybe from channel
        channel = case args of
                  s : _ | not (C.null s) && C.head s == '#' -> Just s
                  _ -> Nothing
        reply = say replyTo
        from = C.takeWhile (/= '!') prefix

createPatterns :: Config -> ConfigPatterns
createPatterns cfg = foldr addCmd M.empty
    (commands cfg ++
        map (\(pattern, event) -> ("PRIVMSG", [emptyRegex, pattern], event))
            (messages cfg))
  where addCmd (cmd, args, event) = let bind = (args, event) in
                                    M.alter (Just . maybe [bind] (bind:)) cmd
        emptyRegex = makeRegex ""

readConfig =
     do args <- getArgs
        s <- C.readFile (fromMaybe "hircrc" $ listToMaybe args)
        return $! read $ C.unpack $! rmComments s
  where rmComments = C.unlines . filter notComment . C.lines
        notComment s = let s' = C.dropWhile isSpace s in
                       C.null s' || C.head s' /= '#'

initConfig !users !cfg =
     do aliasHash <- H.new (==) hashByteString
        mapM_ (\(k, v) -> H.update aliasHash k v) (define cfg)
        return $! ConfigSt {
            raw = cfg,
            encodeInput = case encoding cfg of
                          Utf8 -> utf8Encode
                          Latin1 -> utf8Decode
                          Raw -> id,
            patterns = createPatterns cfg,
            aliasMap = aliasHash,
            perms = foldr addPerm M.empty (permits cfg),
            plugins = M.empty,
            users = users
        }
  where addPerm (perm, users) = let perms = map getPerm users in
                                M.alter (Just . maybe perms (perms ++)) perm
        getPerm (':':group) = Group group
        getPerm user = Client (makeRegex ('^':permPattern user ++ "$"))
        permPattern (c:s) = case c of
                            '*' -> '.':'*':permPattern s
                            '.' -> '\\':'.':permPattern s
                            _ -> c:permPattern s
        permPattern "" = ""

getConfig users = readConfig >>= initConfig users

main = 
     do initEnv
        hSetBuffering stdout LineBuffering
        readConfig >>= connect 0
  where connect !nth !cfg =
             do let servers' = servers cfg
                    (host, port) = servers' !! (nth `mod` length servers')
                users <- H.new (==) hashByteString
                config <- initConfig users cfg >>= newIORef
                appendFile "seen.dat" "\n"
                catch (connectIrc host port (nick cfg) bot config)
                      (failed nth config)
        failed !nth !config ex =
             do putStrLn ("Reconnect after 1 min: Error occured: " ++ show ex)
                threadDelay 60000000
                putStrLn "Reconnecting..."
                readIORef config >>= connect (nth + 1) . raw
