{-
 - Lowlevel IRC client library for HircBot.
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
module Hirc (
    Irc, showMsg, ircSend, ircCmd, say, say', quitIrc, connectIrc,
    escape, ircCatch, liftIO, ircConfig, ircModifyConfig, myIrcNick, splitN,
    initEnv
) where

import Control.Arrow
import Control.Concurrent
import Control.Exception as E
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as C
import Data.IORef
import Data.List
import Network
import System.IO
import System.IO.Error (ioeGetErrorString, ioeGetErrorType, isUserErrorType)
import System.Environment
import System.Mem
import System.Random

data IrcCtx c = IrcCtx { conn :: Handle, lastPong :: MVar Int,
                         sync :: MVar (), buffer :: Chan [C.ByteString],
                         config :: MVar c,
                         currentNick :: IORef C.ByteString,
                         isQuit :: IORef Bool }
type Irc c a = ReaderT (IrcCtx c) IO a

foreign import ccall "hirc_init_env" initEnv :: IO ()

space = C.singleton ' '
colon = C.singleton ':'
spaceColon = C.pack " :"

showMsg :: (C.ByteString, String, [C.ByteString]) -> C.ByteString
showMsg (prefix, cmd, arg) =
    if C.null prefix then
        C.pack cmd `C.append` showArg arg
    else
        C.concat [colon, prefix, C.pack (' ':cmd), showArg arg]

showArg args = C.concat (format args)
  where format [] = []
        format [arg] = [spaceColon, arg]
        format (arg : rest) = space : arg : format rest

stripCR str =
    if not (C.null str) && C.last str == '\r' then
        C.take (C.length str - 1) str
    else
        str

readMsg :: (Monad m) => C.ByteString -> m (C.ByteString, String, [C.ByteString])
readMsg message =
    if args == [] then fail ("Invalid irc message: " ++ show message)
                  else return $! (prefix, C.unpack (head args),
                                  tail args ++ [stripCR $ C.drop 1 final])
  where (args, final) = first C.words (C.span (/= ':') msg)
        (prefix, msg) =
            if not (C.null message) && C.head message == ':' then
                C.span (/= ' ') (C.tail message)
            else
                (C.empty, message)

ircSend :: C.ByteString -> String -> [C.ByteString] -> Irc c ()
ircSend prefix cmd arg =
     do h <- fmap conn ask
        m <- fmap sync ask
        liftIO $ withMVar m $ \_ ->
             do C.hPutStr h $ showMsg (prefix, cmd, arg)
                hPutStr h "\r\n"
                hFlush h

ircCmd cmd what = ircSend C.empty cmd [what]

smartSplit at s =
    if C.null c || C.null rb' then (a, c)
                              else (C.reverse rb', C.append (C.reverse ra) c)
  where (a, c) = C.splitAt at s
        (ra, rb) = C.span (/= ' ') (C.reverse a)
        rb' = C.dropWhile (== ' ') rb

splitN n = takeWhile (not . C.null) . unfoldr (Just . smartSplit n)

say' limit !to text = limit (splitN 400 text) >>= mapM_ msg
  where msg !line = ask >>= liftIO . (`writeChan` [to, line]) . buffer

say to text = say' return to text

quitIrc :: C.ByteString -> Irc c ()
quitIrc quitMsg =
     do ask >>= liftIO . (`writeIORef` True) . isQuit
        ircCmd "QUIT" quitMsg
        fail "QUIT"

alive = C.pack "alive"

pinger ctx = run
  where run = do threadDelay (1000 * 1000 * 120)
                 runReaderT (ircCmd "PING" alive) ctx
                 run

pingChecker ctx th = run
  where run = do threadDelay 10000000
                 performGC -- just force GC on every 10 seconds
                 n <- modifyMVar (lastPong ctx) update
                 if n >= 300 then throwTo th (ErrorCall "ping timeout") else run
        update x = let y = x + 10 in return $! (y, y)

processIrc handler = run wait
  where run p = ask >>= liftIO . C.hGetLine . conn >>= readMsg >>= p
        process _ (_, "PING", param) = ircSend C.empty "PONG" param
        process _ (_, "PONG", _) =
            ask >>= liftIO . (`swapMVar` 0) . lastPong >> return ()
        process h msg@(_, cmd, !nick:_) | cmd == "NICK" || cmd == "001" =
            ask >>= liftIO . (`writeIORef` nick) . currentNick >> h msg
        process h msg  = h msg
        wait (_, "376", _) = handler (C.empty, "CONNECTED", []) >> run ready
        wait (_, "432", _) = liftIO $ fail "Invalid nick"
        wait (_, "433", _) = 
             do n <- liftIO $ randomRIO (10 :: Int, 9999)
                oldnick <- myIrcNick
                ircSend C.empty "NICK"
                        [C.take 8 oldnick `C.append` C.pack (show n)]
                run wait
        wait msg = process (const (return ())) msg >> run wait
        ready msg = process handler msg >> run ready

connectIrc :: Integral a => String -> a -> String
                         -> ((C.ByteString, String, [C.ByteString]) -> Irc c ())
                         -> MVar c -> IO ()
connectIrc host port nick handler cfgRef =
     do h <- connectTo host (PortNumber $ fromIntegral port)
        hSetEncoding h latin1
        lastPong <- newMVar 0
        sync <- newMVar ()
        buf <- newChan
        nick' <- newIORef cnick
        quit <- newIORef False
        let ctx = IrcCtx h lastPong sync buf cfgRef nick' quit
            writer t = do threadDelay t
                          msg <- readChan buf
                          runReaderT (ircSend C.empty "PRIVMSG" msg) ctx
                          writer (sum (120 : map C.length msg) * 9000)
            ex (ErrorCall e) = do putStrLn ("ex: " ++ show e)
                                  fail e
            ioEx e | ioeGetErrorString e == "QUIT" = putStrLn "ioEx QUIT"
            ioEx e = do q <- readIORef quit
                        if q then putStrLn "ioEx with quit"
                             else putStrLn ("ioEx: " ++ show e) >> ioError e
        mainThread <- myThreadId
        threads <- sequence $ map forkIO [
            pinger ctx, pingChecker ctx mainThread, writer 1]
        finally (E.catch (E.catch (runReaderT run ctx) ioEx) ex)
                (finally (runReaderT (handler (C.empty, "TERMINATE", [])) ctx)
                         (mapM_ killThread threads >> hClose h))
        putStrLn "Normal shutdown."
  where run = do user <- liftIO $ getEnv "USER"
                 ircCmd "NICK" cnick
                 ircSend C.empty "USER"
                         (map C.pack [user, "localhost", "unknown"] ++ [cnick])
                 processIrc handler
        cnick = C.pack nick

escape :: Irc c (Irc c a -> IO a)
escape =
     do ctx <- ask
        return $! \action -> runReaderT action ctx

ircCatch :: Irc c a -> (String -> Irc c a) -> Irc c a
ircCatch action handler =
     do liftIrc <- escape
        let ex (ErrorCall e) = fail e
            ioEx e | isUserErrorType (ioeGetErrorType e) =
                liftIrc $ handler (ioeGetErrorString e)
            ioEx e = ioError e
        liftIO $ E.catch (E.catch (liftIrc action) ex) ioEx

ircConfig :: Irc c c
ircConfig = ask >>= liftIO . readMVar . config

ircModifyConfig :: (c -> IO c) -> Irc c ()
ircModifyConfig f = ask >>= liftIO . (`modifyMVar_` f) . config

myIrcNick :: Irc c C.ByteString
myIrcNick = ask >>= liftIO . readIORef . currentNick
