{-
 - Lowlevel IRC client library for HircBot.
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
module Hirc (
    Irc, showMsg, ircSend, ircCmd, say, quitIrc, connectIrc,
    escape, ircCatch, liftIO, ircConfig, ircSetConfig, myIrcNick, splitN
) where

import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Data.IORef
import Data.List
import Network
import System.IO
import System.IO.Error (ioeGetErrorString, ioeGetErrorType, isUserErrorType)
import System.Environment
import Prelude hiding (catch)

data IrcCtx c = IrcCtx { conn :: Handle, lastPong :: IORef Bool,
                         sync :: MVar (), buffer :: Chan [String],
                         config :: IORef c,
                         currentNick :: IORef String,
                         isQuit :: IORef Bool }
type Irc c a = ReaderT (IrcCtx c) IO a

showMsg :: (String, String, [String]) -> String
showMsg ("", cmd, arg) = cmd ++ showArg arg
showMsg (prefix, cmd, arg) = ':':prefix ++ ' ':cmd ++ showArg arg

showArg [] = ""
showArg [arg] = ' ':':':arg
showArg (arg : rest) = ' ':arg ++ showArg rest

stripCR str = reverse (case reverse str of
                       '\r':s -> s
                       s -> s)

readMsg :: (Monad m) => String -> m (String, String, [String])
readMsg message =
    if args == [] then fail ("Invalid irc message: " ++ show message)
                  else return (prefix, head args,
                               tail args ++ [stripCR $ drop 1 final])
  where (args, final) = first words (span (/= ':') msg)
        (prefix, msg) = case message of
                        ':' : s -> span (/= ' ') s
                        s -> ("", s)

ircSend :: String -> String -> [String] -> Irc c ()
ircSend prefix cmd arg =
     do h <- fmap conn ask
        m <- fmap sync ask
        liftIO $ withMVar m $ \_ ->
             do hPutStr h $ showMsg (prefix, cmd, arg)
                hPutStr h "\r\n"
                hFlush h

ircCmd cmd what = ircSend "" cmd [what]

smartSplit at s =
    if c == "" || rb' == "" then (a, c) else (reverse rb', reverse ra ++ c)
  where (a, c) = splitAt at s
        (ra, rb) = span (/= ' ') (reverse a)
        rb' = dropWhile (== ' ') rb

splitN n = takeWhile (not . null) . unfoldr (Just . smartSplit n)

say to text = mapM_ msg (splitN 400 text)
  where msg line = ask >>= liftIO . (`writeChan` [to, line]) . buffer

quitIrc :: String -> Irc c ()
quitIrc quitMsg =
     do ask >>= liftIO . (`writeIORef` True) . isQuit
        ircCmd "QUIT" quitMsg
        fail "QUIT"

pinger ctx th = run
  where run = do threadDelay (1000 * 1000 * 120)
                 hadPong <- readIORef (lastPong ctx)
                 writeIORef (lastPong ctx) False
                 if hadPong then runReaderT (ircCmd "PING" "alive") ctx >> run
                            else throwTo th (ErrorCall "ping timeout")

processIrc handler = run wait
  where run p = ask >>= liftIO . hGetLine . conn >>= readMsg >>= p
        process _ m@(_, "PING", param) = ircSend "" "PONG" param
        process _ (_, "PONG", _) =
            ask >>= liftIO . (`writeIORef` True) . lastPong
        process h msg@(_, cmd, nick:_) | cmd == "NICK" || cmd == "001" =
            ask >>= liftIO . (`writeIORef` nick) . currentNick >> h msg
        process h msg  = h msg
        wait (_, "376", _) = handler ("", "CONNECTED", []) >> run ready
        wait msg = process (const (return ())) msg >> run wait
        ready msg = process handler msg >> run ready

connectIrc :: Integral a => String -> a -> String
                         -> ((String, String, [String]) -> Irc c ())
                         -> c -> IO ()
connectIrc host port nick handler cfg =
     do h <- connectTo host (PortNumber $ fromIntegral port)
        lastPong <- newIORef True
        sync <- newMVar ()
        buf <- newChan
        cfgRef <- newIORef cfg
        nick' <- newIORef nick
        quit <- newIORef True
        let ctx = IrcCtx h lastPong sync buf cfgRef nick' quit
            writer t = do threadDelay t
                          msg <- readChan buf
                          runReaderT (ircSend "" "PRIVMSG" msg) ctx
                          writer (sum (120 : map length msg) * 9000)
            ex (ErrorCall e) = fail e
            ex (IOException e) | ioeGetErrorString e == "QUIT" = return ()
            ex e = do q <- readIORef quit
                      if q then return () else throw e
        pingerTh <- myThreadId >>= forkIO . pinger ctx
        writerTh <- forkIO (writer 1)
        finally (catch (runReaderT run ctx) ex)
                (killThread pingerTh >> killThread writerTh >> hClose h)
  where run = do user <- liftIO $ getEnv "USER"
                 ircCmd "NICK" nick
                 ircSend "" "USER" [user, "localhost", "unknown", nick]
                 processIrc handler

escape :: Irc c (Irc c a -> IO a)
escape =
     do ctx <- ask
        return (\action -> runReaderT action ctx)

ircCatch :: Irc c a -> (String -> Irc c a) -> Irc c a
ircCatch action handler =
     do liftIrc <- escape
        let ex (ErrorCall e) = liftIrc $ handler e
            ex (IOException e) | isUserErrorType (ioeGetErrorType e) =
                liftIrc $ handler $ ioeGetErrorString e
            ex e = throw e
        liftIO $ catch (liftIrc action) ex

ircConfig :: Irc c c
ircConfig = ask >>= liftIO . readIORef . config

ircSetConfig :: c -> Irc c ()
ircSetConfig cfg = ask >>= liftIO . (`writeIORef` cfg) . config

myIrcNick :: Irc c String
myIrcNick = ask >>= liftIO . readIORef . currentNick
