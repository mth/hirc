{-
 - HircBot configuration conversion utility.
 - Copyright (C) 2011,2012  Madis Janson
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
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as C
import System.Environment

data EncodingSpec = Utf8 | Latin1 | Raw
    deriving (Read, Show)

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
    deriving (Read, Show)

data Regex = Regex String

instance Read Regex where
    readsPrec _ ('/':s) =
        let parse ('\\':'/':cs) acc = parse cs $ '/':'\\':acc
            parse ('/':'i':cs) acc = [(Regex (reverse ('i':'/':acc)), cs)]
            parse ('/':cs) acc = [(Regex (reverse ('/':acc)), cs)]
            parse (c:cs) acc = parse cs $ c:acc
            parse "" _ = [] in
        parse s "/"
    readsPrec x (c:cs) | isSpace c = readsPrec x cs
    readsPrec _ _ = []

instance Show Regex where
    show (Regex r) = r

data Config = Config {
    servers  :: [(String, Integer)],
    nick     :: String,
    encoding :: EncodingSpec,
    messages :: [(Regex, [EventSpec])],
    commands :: [(String, [Regex], [EventSpec])],
    permits  :: [(String, [String])],
    nopermit :: [EventSpec]
} deriving Read


str [] = "[]"
str [a] = "[" ++ show a ++ "]"
str l = concat ("[\n" : intersperse ",\n" (map (('\t':) . show) l) ++ ["\n]"])

data EventSpecs = EList [EventSpec]
data Strings = SList [String]
instance Show EventSpecs where show (EList l) = str l
instance Show Strings where show (SList l) = str l

data ConfigItem =
    Server String Integer |
    Nick String |
    Encoding EncodingSpec |
    On Regex EventSpecs |
    Command String [Regex] EventSpecs |
    Permit String Strings |
    NoPermit EventSpecs deriving Show

notComment s = let s' = C.dropWhile isSpace s in C.null s' || C.head s' /= '#'
rmComments = C.unlines . filter notComment . C.lines

main = do
    args <- getArgs
    s <- C.readFile (fromMaybe "hircrc" $ listToMaybe args)
    let !cfg = read $ C.unpack $! rmComments s
    mapM_ (\(s,p) -> print (Server s p)) (servers cfg)
    print (Nick (nick cfg))
    print (Encoding (encoding cfg))
    mapM_ (\(r,e) -> print (On r (EList e))) (messages cfg)
    mapM_ (\(c,p,e) -> print (Command c p (EList e))) (commands cfg)
    mapM_ (\(s,l) -> print (Permit s (SList l))) (permits cfg)
    print (NoPermit (EList (nopermit cfg)))
