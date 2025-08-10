{-
 - HircBot - Simple IRC bot in haskell.
 - Copyright (C) 2008-2025  Madis Janson
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
import Data.Maybe
import Data.List
import Data.UnixTime
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C
import Control.Monad
import Control.Concurrent
import Control.Exception as E
import Text.Regex.Posix
import System.Environment
import System.Exit
import System.Random
import System.Posix.IO
import System.Posix.Signals
import System.Posix.Process.ByteString
import System.Posix.Types
import System.IO

data EncodingSpec = Utf8 | Latin1 | Raw
    deriving Read

data EventSpec =
    Send !String [C.ByteString] |
    Say !C.ByteString | SayTo !C.ByteString !C.ByteString |
    Join !C.ByteString | Quit !C.ByteString | Perm !C.ByteString |
    IfPerm !C.ByteString [EventSpec] [EventSpec] | RandLine !String |
    Exec !C.ByteString [C.ByteString] | Plugin [C.ByteString] !C.ByteString |
    ExecMaxLines !Int !C.ByteString [C.ByteString] |
    ExecTopic !C.ByteString !C.ByteString [C.ByteString] |
    Calc !C.ByteString | Append !String !C.ByteString | Rehash |
    LinesStarting !Int !String !Int !C.ByteString [EventSpec] |
    Call !C.ByteString [C.ByteString] | Next
    deriving Read

data AllowSpec = Client !Regex | Group C.ByteString

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
    servers  :: [(String, Int, Bool)],
    nick     :: String,
    encoding :: !EncodingSpec,
    define   :: [(C.ByteString, [EventSpec])],
    messages :: [(Regex, [EventSpec])],
    commands :: [(String, [Regex], [EventSpec])],
    times    :: [(Regex, String, [EventSpec])],
    permits  :: [(C.ByteString, [C.ByteString])],
    nopermit :: [EventSpec]
} deriving Read

type ConfigPatterns = M.Map String [([Regex], [EventSpec])]

data ConfigItem =
    Server String !Int |
    IPv6Server String !Int |
    Nick String |
    Encoding !EncodingSpec |
    On !Regex [EventSpec] |
    Time !Regex String [EventSpec] |
    Command String [Regex] [EventSpec] |
    Define !C.ByteString [EventSpec] |
    Permit !C.ByteString [C.ByteString] |
    NoPermit [EventSpec] deriving Read

data PluginId = ExecPlugin [C.ByteString]
    deriving (Show, Eq, Ord)

data PluginCmd = PluginMsg C.ByteString C.ByteString | KillPlugin

type Bot a = Irc ConfigSt a

data User = User {
    rank :: !Int,
    spoke :: !C.ByteString
}

data ConfigSt = ConfigSt {
    raw :: !Config,
    encodeInput :: String -> String,
    patterns :: !ConfigPatterns,
    aliasMap :: M.Map C.ByteString [EventSpec],
    perms :: M.Map C.ByteString [AllowSpec],
    -- Map nick (Map channel User)
    users :: M.Map C.ByteString (M.Map C.ByteString User),
    plugins :: M.Map PluginId (PluginCmd -> Bot ()),
    topics :: M.Map C.ByteString C.ByteString,
    timers :: [(Bool, Regex, C.ByteString, [EventSpec])]
}

data EventSrc = EventSrc {
    channel :: Maybe C.ByteString,
    from    :: !C.ByteString,
    prefix  :: !C.ByteString
}

ioCatch :: IO a -> (IOError -> IO a) -> IO a
ioCatch = E.catch

matchRegex :: Regex -> C.ByteString -> Maybe [C.ByteString]
matchRegex re value = collect . drop 1 . elems <$> matchOnce re value
  where collect ((start, len) : rest) =
            C.take len (C.drop start value) : collect rest
        collect [] = []

bindArg :: EventSrc -> [C.ByteString] -> C.ByteString -> Bot C.ByteString
bindArg src bindings str = C.concat <$> mapM id (format str)
  where format str =
            let (start, rest) = C.span (/= '$') str in
            if C.null rest then
                [return start]
            else let !rest' = C.tail rest in
                if C.null rest' then
                    [return start, return rest]
                else case (C.head rest', C.span (/= '}') (C.tail rest')) of
                    (':', _) ->
                        return start : return (prefix src) : format (C.tail rest')
                    ('{', (v, t)) | not (C.null t) ->
                        let ftail = format (C.drop 1 t)
                            topic channel = (fromMaybe C.empty . M.lookup channel
                                             . topics <$> ircConfig) : ftail in
                        case map C.unpack $ C.words v of
                            ["topic"] -> maybe ftail topic (channel src)
                            ["topic", channel] -> topic (C.pack channel)
                            "time" : _ ->
                                liftIO (getUnixTime >>=
                                            formatUnixTime (C.drop 5 v)) : ftail
                            _ -> return start : dollar : format rest'
                    _ -> case C.readInt rest' of
                        Just (i, r) | i >= 0 && i < length bindings ->
                            return start : return (bindings!!i) : format r
                        _ -> return start : dollar : format rest'
        dollar = return $! C.singleton '$'

randLine :: String -> IO C.ByteString
randLine fn =
     do l <- C.lines <$> C.readFile fn
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

dropPath p = if C.null s then p else dropPath (C.tail s)
  where s = C.dropWhile (/= '/') p

putLog = liftIO . putStrLn
cPutLog s l = liftIO $ C.putStrLn $ C.concat (C.pack s : l)
lower = C.map toLower

findLinesStarting :: String -> Int -> C.ByteString -> IO [C.ByteString]
findLinesStarting fn sepLen str =
    if strLen <= sepLen then return []
    else ioCatch (mapMaybe checkLine . C.lines <$> C.readFile fn)
                 (\err -> return [C.pack $ show err])
  where lowStr = lower str
        strLen = C.length lowStr
        checkLine line =
            if lower (C.take strLen line) == lowStr then
                Just (C.concat [C.take (strLen - sepLen) line, C.pack " - ",
                                C.drop strLen line])
            else Nothing

{-
 - EXEC
 -}
sysProcess :: Maybe Fd -> C.ByteString -> [C.ByteString]
                       -> IO (ProcessID, Handle)
sysProcess input prog argv =
     do (rd, wd) <- createPipe
        pid <- forkProcess (finally (closeFd rd >> child wd)
                                    (exitImmediately (ExitFailure 127)))
        closeFd wd
        h <- fdToHandle rd
        hSetEncoding h latin1
        return (pid, h)
  where tryClose fd = ioCatch (closeFd fd) (const (return ()))
        child wd =
         do dupTo wd stdOutput
            case input of
                Just inp -> do dupTo inp stdInput
                               closeFd inp
                Nothing ->  do dupTo wd stdError
                               closeFd stdInput
            closeFd wd
            mapM_ (tryClose . toEnum) [3 .. 255]
            ioCatch (executeFile prog False argv Nothing)
                    (\e -> when (input /= Nothing)
                               (fdWrite stdError (show e ++ "\n") >> return ()))
            fdWrite stdOutput "dead plugin walking"
            return ()

readInput :: Handle -> (IO () -> C.ByteString -> IO ()) -> IO () -> IO ()
readInput h f cleanup = ioCatch copy (\_ -> hClose h >> cleanup)
    where copy = C.hGetLine h >>= f copy

execSys :: C.ByteString -> (IO () -> C.ByteString -> Bot ())
                        -> C.ByteString -> [C.ByteString] -> Bot ()
execSys to handler prog argv =
     do unlift <- escape
        liftIO $ do (pid, h) <- sysProcess Nothing prog argv
                    let reader cont line = unlift $ handler cont line
                    forkIO $ readInput h reader (return ())
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
                 do sayTo (C.append (C.pack "Terminated ") (dropPath prog))
                    threadDelay 1000000
                    kill killProcess pid
                         (getProcessStatus True False pid >> return ())

execToSay :: C.ByteString -> Int -> C.ByteString -> [C.ByteString] -> Bot ()
execToSay to maxLines prog argv =
     do v <- liftIO $ newMVar maxLines
        let filter l n = let r = take n l in return (n - length r, r)
            sayN continue s =
                 do say' (liftIO . modifyMVar v . filter) to s
                    liftIO $ withMVar v (return . (> 0)) >>= (`when` continue)
        execSys to sayN prog argv

{-
 - SEEN
 -}
getUserMap :: C.ByteString -> Bot (M.Map C.ByteString User)
getUserMap nick = fromMaybe M.empty . M.lookup (lower nick) . users <$> ircConfig

getUser :: C.ByteString -> C.ByteString -> Bot (Maybe User)
getUser channel nick = M.lookup channel <$> getUserMap nick

updateUserMap :: (M.Map C.ByteString User -> M.Map C.ByteString User)
                    -> C.ByteString -> Bot ()
updateUserMap f nick =
    ircModifyConfig $ \cfg ->
        let !users' = M.alter update (lower nick) (users cfg) in
        return cfg { users = users' }
  where update user = let !m = f (fromMaybe M.empty user) in
                      if M.null m then Nothing else Just m

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
     do UnixTime t _ <- liftIO getUnixTime
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

seenEvent "TOPIC" _ (channel:topic:_) = setTopic channel topic
seenEvent "332" _ (_:channel:topic:_) = setTopic channel topic
seenEvent "331" _ (_:channel:_) = setTopic channel C.empty
seenEvent "366" _ (_:channel:_) = ircCmd "TOPIC" channel -- End of NAMES list

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

setTopic !channel !topic =
    ircModifyConfig $ \cfg ->
        return cfg { topics = if C.null topic then M.delete channel (topics cfg)
                              else M.insert channel topic (topics cfg) }

{-
 - PLUGIN
 -}
removePlugin id =
    ircModifyConfig $ \cfg ->
        return cfg { plugins = M.delete id (plugins cfg) }

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
        let sayTo cont s = readMVar to >>= unlift . (`say` s) >> cont
            output = readInput out sayTo $
                         do unlift (removePlugin id)
                            hClose inp
                            getProcessStatus True False pid
                            return ()
            kill = removePlugin id >>
                    liftIO (signalProcess softwareTermination pid)
            handler KillPlugin = kill
            handler (PluginMsg replyTo msg) = liftIO $ ioCatch
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
                   ircModifyConfig $ \cfg ->
                        return cfg { plugins = M.insert id p (plugins cfg) }
                   p (PluginMsg to msg)
{-
 - CORE
 -}
checkPerm :: EventSrc -> C.ByteString -> Bot Bool
checkPerm src perm =
     do cfg <- ircConfig
        let hasPerm perm =
              case if C.length perm == 1 then C.head perm else ' ' of
              '+' -> hasRank 1
              '%' -> hasRank 2
              '@' -> hasRank 3
              _ -> anyPerm $! concat $! maybeToList $! M.lookup perm (perms cfg)
            anyPerm (perm:rest) =
             do ok <- case perm of
                      Client re -> return $! (matchOnce re (prefix src)) /= Nothing
                      Group group -> hasPerm group
                if ok then return True else anyPerm rest
            anyPerm [] = return False
            hasRank expectRank =
                -- It's idiotic to give perm based on rank on any channel,
                -- but I don't know a better solution now for private chat
                case channel src of
                Nothing -> any (\u -> rank u >= expectRank) . M.elems
                            <$> getUserMap (from src)
                Just ch -> maybe False ((>= expectRank) . rank)
                            <$> getUser ch (from src)
        hasPerm perm

executeEvent :: EventSrc -> (C.ByteString -> Bot C.ByteString) -> EventSpec -> Bot ()
executeEvent src param event =
    case event of
    Send evCmd evArg -> mapM param evArg >>= ircSend C.empty evCmd
    Say text      -> param text >>= mapM_ reply . C.lines
    SayTo to text -> do to' <- param to
                        param text >>= mapM_ (say to') . C.lines
    Call alias args ->
        let exec event =
              if null args then execute param event
              else do args' <- mapM param args
                      execute (bindArg src (from src : args')) event in
        ircConfig >>= maybe (error (C.unpack alias ++ " undefined"))
                            (mapM_ exec) . M.lookup alias . aliasMap
    Perm perm     -> do ok <- checkPerm src perm
                        unless ok (fail "NOPERM")
    IfPerm perm events evElse ->
         do ok <- checkPerm src perm
            mapM_ (execute param) (if ok then events else evElse)
    Join channel  -> do ch <- param channel
                        ircSend C.empty "JOIN" [ch]
    Quit msg      -> param msg >>= quitIrc
    RandLine fn   -> tryReply (liftIO (randLine fn) >>= param)
    LinesStarting limit fn sepLen find notFound ->
         do findText <- param find
            results <- liftIO (findLinesStarting fn sepLen findText)
            if null results then mapM_ (execute param) notFound
                            else mapM_ reply (take limit results)
    Calc text     -> tryReply (C.pack . calc . C.unpack <$> param text)
    Rehash        -> killPlugins >> ircModifyConfig getConfig
    Exec prg args -> mapM param args >>= execToSay replyTo 50 prg
    ExecMaxLines limit prg args ->
        mapM param args >>= execToSay replyTo limit prg
    ExecTopic channel prog args ->
        let setTopic _ topic =
             do current <- M.lookup channel . topics <$> ircConfig
                when (Just topic /= current)
                     (ircSend C.empty "TOPIC" [channel, topic]) in
        mapM param args >>= execSys channel setTopic prog
    Plugin prg cmd -> param cmd >>= invokePlugin (ExecPlugin prg) replyTo
    Append file str -> param str >>= liftIO . C.appendFile file
    Next -> return ()
  where replyTo = fromMaybe (from src) (channel src)
        reply = say replyTo
        tryReply action = ircCatch (action >>= reply) (reply . C.pack)
        execute = executeEvent src

-- wrapper that encodes irc input into desired charset
bot :: (C.ByteString, String, [C.ByteString]) -> Bot ()
bot (prefix, cmd, args) =
     do cfg <- ircConfig
        bot' (prefix, cmd, map (C.pack . encodeInput cfg . C.unpack) args)

bot' :: (C.ByteString, String, [C.ByteString]) -> Bot ()
bot' msg@(msgPrefix, cmd, args) =
    ircCatch (seenEvent cmd (from src) args)
             (putLog . (("seenEvent " ++ cmd ++ ": ") ++) . show) >>
    ircCatch (ircConfig >>= maybe (liftIO $ print msg) doMatch
                            . M.lookup cmd . patterns) atErr
  where doMatch ((argPattern, events):rest) =
            if length args < length argPattern then doMatch rest
            else maybe (doMatch rest) (\p -> do
                    let param = bindArg src (from src : (concat p) ++ [from src])
                    mapM_ (executeEvent src param) events
                    let isNext Next = True
                        isNext _ = False
                    when (any isNext events) (doMatch rest))
                  (sequence $ zipWith matchRegex argPattern args)
        doMatch [] = do let what = last args
                        when (cmd == "PRIVMSG")
                             (seenMsg (channel src) (from src) $! what)
                        when (args /= [] && C.isPrefixOf (C.singleton '!') what)
                             (cPutLog "NOMATCH " [showMsg msg])
        atErr "NOPERM" = ircConfig >>=
            mapM_ (executeEvent src $! bindArg src [from src, from src])
            . nopermit . raw
        atErr str = putLog str >> ircSend (from src) "NOTICE" [from src, C.pack str]
        src = EventSrc {
            prefix = msgPrefix,
            channel = case args of
                      s : _ | not (C.null s) && C.head s == '#' -> Just s
                      _ -> Nothing,
            from = C.takeWhile (/= '!') msgPrefix
        }

botTicker :: Bot ()
botTicker = do timeStr <- liftIO (getUnixTime >>= formatUnixTime timeFormat)
               activity <- ircConfig >>= mapM (run timeStr) . timers
               ircModifyConfig (\cfg ->
                    return cfg { timers = merge activity (timers cfg) })
  where merge (active : activity) ((_, re, from, events) : rest) =
            (active, re, from, events) : merge activity rest
        merge _ timers = timers
        run timeStr (wasActive, re, from, events) =
            case matchRegex re timeStr of
              Just param | not wasActive -> do
                let src = source from
                mapM_ (executeEvent src (bindArg src param)) events
                return True
              match -> return $ match /= Nothing
        timeFormat = C.pack "%FT%H:%M"
        source from =
          let channel = if C.head from == '#' then Just from else Nothing in
          EventSrc { prefix = C.empty, channel = channel, from = from }

parseConfigItems :: String -> [ConfigItem]
parseConfigItems str = skip parse 1 str
  where skip _ line ('\n':s) = skip parse (line + 1) s
        skip tr line (c:s) | isSpace c = skip tr line s
        skip _ _ "" = []
        skip tr line str = tr line str
        parse line str = case reads str of
            ((result, tail):_) ->
                let len = length str - length tail in
                let !nl = line + length (filter (== '\n') (take len str)) in
                result : skip err nl tail
            _ -> error (show line ++ ": syntax error")
        err line _ = error (show line ++ ": expected newline after definition")

collectConfig :: [ConfigItem] -> Config -> Config
collectConfig (item : items) cfg =
    collectConfig items $ case item of
        (Server host port) -> cfg { servers = (host, port, False) : servers cfg }
        (IPv6Server host port) -> cfg { servers = (host, port, True) : servers cfg }
        (Nick nick)     -> cfg { nick = nick }
        (Encoding spec) -> cfg { encoding = spec }
        (On re events)  -> cfg { messages = (re, events) : messages cfg }
        (Time re from events) -> cfg { times = (re, from, events) : times cfg }
        (Command cmd re events) ->
            cfg { commands = (cmd, re, events) : commands cfg }
        (Define name events) -> cfg { define = (name, events) : define cfg }
        (Permit name masks) -> cfg { permits = (name, masks) : permits cfg }
        (NoPermit events) -> cfg { nopermit = events ++ nopermit cfg }

collectConfig [] cfg = cfg

createPatterns :: Config -> ConfigPatterns
createPatterns cfg = foldr addCmd M.empty
    (commands cfg ++
        map (\(pattern, event) -> ("PRIVMSG", [emptyRegex, pattern], event))
            (messages cfg))
  where addCmd (cmd, args, event) = let bind = (args, event) in
                                    M.alter (Just . maybe [bind] (bind:)) cmd
        emptyRegex = makeRegex ""

readConfig :: IO Config
readConfig =
     do args <- getArgs
        s <- C.readFile (fromMaybe "hircrc" $ listToMaybe args)
        let !cfg = parse $! C.unpack $! rmComments s
        if null (servers cfg)
            then fail "No servers defined in configuration"
            else return $! cfg
  where rmComments = C.unlines . filter notComment . C.lines
        notComment s = let s' = C.dropWhile isSpace s in
                       C.null s' || C.head s' /= '#'
        parse str | "Config " `isPrefixOf` dropWhile isSpace str = read str
        parse str = collectConfig (reverse $ parseConfigItems str) Config {
            servers = [], nick = "HircB0t", encoding = Utf8, define = [],
            messages = [], commands = [], times = [], permits = [], nopermit = []
        }

initConfig !users !topics !cfg =
     do return $! ConfigSt {
            raw = cfg,
            encodeInput = case encoding cfg of
                          Utf8 -> utf8Encode
                          Latin1 -> utf8Decode
                          Raw -> id,
            patterns = createPatterns cfg,
            aliasMap = M.fromList (define cfg),
            perms = foldr addPerm M.empty (permits cfg),
            plugins = M.empty,
            users = users,
            topics = topics,
            timers = map (\(re, from, events) ->
                            (False, re, C.pack from, events)) (times cfg)
        }
  where addPerm (perm, users) = let perms = map getPerm users in
                                M.alter (Just . maybe perms (perms ++)) perm
        getPerm user =
            if not (C.null user) && C.head user == ':' then Group (C.tail user)
                else Client (makeRegex (permPattern user))
        permPattern s = C.concat $! C.singleton '^' : escapePerm s
        escapePerm = tr . C.break (\c -> c == '*' || c == '.')
        tr (!h, t) | C.null t = [h, C.singleton '$']
        tr (h, t) = h : (case C.head t of
                           '*' -> aster
                           '.' -> dot
                           _ -> C.empty) : escapePerm (C.tail t)
        aster = C.pack ".*"
        dot   = C.pack "\\."

getConfig st = readConfig >>= initConfig (users st) (topics st)

main =
     do initEnv
        hSetBuffering stdout LineBuffering
        readConfig >>= connect 0
  where connect !nth !cfg =
             do let servers' = servers cfg
                    server = servers' !! (nth `mod` length servers')
                config <- initConfig M.empty M.empty cfg >>= newMVar
                appendFile "seen.dat" "\n"
                ioCatch (connectIrc server (nick cfg) bot botTicker config)
                        (failed nth config)
        failed !nth !config ex =
             do putStrLn ("Reconnect after 1 min: Error occured: " ++ show ex)
                threadDelay 60000000
                putStrLn "Reconnecting..."
                readMVar config >>= connect (nth + 1) . raw
