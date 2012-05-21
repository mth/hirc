{-
 - HircBot configuration parsing functions.
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
module Config where

import Data.Char
import Data.Maybe
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C
import Text.Regex.Posix
import System.Environment

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

type ConfigPatterns = M.Map String [([Regex], [EventSpec])]

data ConfigItem =
    Server String Integer |
    Nick String |
    On Regex [EventSpec] |
    Command String [Regex] [EventSpec] |
    Permit String [String] |
    NoPermit [EventSpec] deriving Read

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
        return $! read $ C.unpack $! rmComments s
  where rmComments = C.unlines . filter notComment . C.lines
        notComment s = let s' = C.dropWhile isSpace s in
                       C.null s' || C.head s' /= '#'

