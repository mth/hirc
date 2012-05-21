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
    Send !String [C.ByteString] |
    Say !C.ByteString | SayTo !C.ByteString !C.ByteString |
    Join !C.ByteString | Quit !C.ByteString | Perm !String |
    IfPerm !String [EventSpec] [EventSpec] | RandLine !String |
    Exec !String [C.ByteString] | Plugin [String] !C.ByteString |
    ExecMaxLines !Int String [C.ByteString] |
    Http !C.ByteString !C.ByteString !Int !Regex [EventSpec] |
    Calc !C.ByteString | Append !String !C.ByteString | Rehash |
    Call !C.ByteString [C.ByteString]
    deriving Read

data AllowSpec = Client !Regex | Group String

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
    servers  :: [(String, Int)],
    nick     :: String,
    encoding :: !EncodingSpec,
    define   :: [(C.ByteString, [EventSpec])],
    messages :: [(Regex, [EventSpec])],
    commands :: [(String, [Regex], [EventSpec])],
    permits  :: [(String, [String])],
    nopermit :: [EventSpec]
} deriving Read

data ConfigItem =
    Server String Integer |
    Nick String |
    On Regex EventSpecs |
    Command String [Regex] EventSpecs |
    Permit String Strings |
    NoPermit EventSpecs deriving Read, Show

parseConfigItems :: String -> [ConfigItem]
parseConfigItems str = skip parse 1 str
  where skip _ line ('\n':s) = skip parse (line + 1) s
        skip tr line (c:s) | isSpace c = skip tr line s
        skip _ _ "" = []
        skip tr line str = tr line str
        parse line str = case reads str of
            ((result, tail):_) ->
                let len = length s - length tail in
                let !nl = line + length (filter (== '\n') (take len)) in
                result : skip err nl tail
            _ -> error (show line ++ ": syntax error")
        err line _ = error (show line ++ ": expected newline after definition"))

