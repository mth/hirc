{-
 - Simple calculator using only doubles to avoid DOS.
 - Copyright (C) 2010  Madis Janson
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
module Calculator (calc) where

import Data.Bits
import Data.Char
import Data.Maybe
import Numeric

calc :: String -> String
calc x = if result == 0 then "0" else
            if abs result > 4294967296 || abs result < 0.001
                then showEFloat (Just 15) result ""
                else dropTail $ showFFloat (Just prec) result ""
  where result = expr x
        prec = max 0 (14 - truncate (fromIntegral (exponent result) * 0.3))
        dropTail = reverse . dropWhile (== '.') . dropWhile (== '0') . reverse

functions :: [(String, Double -> Double)]
functions =
   [("abs", abs), ("acos", acos), ("asin", asin), ("atan", atan),
    ("sqrt", sqrt), ("cos", cos), ("sin", sin), ("tan", tan),
    ("exp", exp), ("ln", log), ("log", logBase 10), ("bits", logBase 2),
    ("ceil", fromIntegral . ceiling), ("floor", fromIntegral . floor),
    ("round", fromIntegral . round), ("int", fromIntegral . truncate),
    ("~", fromIntegral . (complement :: Integer -> Integer) . truncate)]

x % y = fromIntegral (truncate x `mod` truncate y)

expr :: String -> Double
expr = (binInt [("<<", shiftL), (">>", shiftR)] $
        bin [("+", (+)), ("-", (-))] $
        binInt [("|", (.|.)), ("&", (.&.)), ("xor", xor)] $
        bin [("*", (*)), ("/", (/)), ("%", (%))] $
        bin [("^", (**))] fun) . (\x -> tail (unary ("":lexer "" x)))

bin :: [(String, Double -> Double -> Double)]
        -> ([String] -> Double) -> [String] -> Double
bin ops eval = bin' id
  where bin' tr s =
         case span (not . (`elem` map fst ops)) s of
         (r, []) -> tr $ eval r
         (a, op:b) -> bin' (fromJust (lookup op ops) (tr (eval a))) b

binInt ops = bin (map (\(s, f) -> (s, tr f)) ops) 
  where tr f x y = fromIntegral $ (truncate x :: Integer) `f` truncate y

fun :: [String] -> Double
fun [] = 0/0
fun (('-':v):s) = -(fun (v:s))
fun ['(':s] = expr s
fun ["pi"] = pi
fun ["e"]  = exp 1
fun [x] = case reads x of
          ((v, ""):_) -> v
          _ -> error (show x ++ "?")
fun (x:xs) = maybe (error (show x ++ "?")) ($ (fun xs)) (lookup x functions)

unary [] = []
unary (a:"-":x:xs) | not (any isDigit a || elem a ["pi", "e"]) =
        a:unary (('-':x):xs)
unary (x:xs) = x:unary xs

lexer "" "" = []
lexer a (' ':s) = next lexer a s
lexer a ('(':s) = next (skip 0) a s
lexer _ (')':_) = error "Mismatched)"
lexer a (',':s) = lexer a ('.':s)
lexer a@(t:_) (x:xs) | x /= '-' && kind t == kind x &&
                       (not (all isAlpha a) || isAlpha x) = lexer (x:a) xs
  where kind c = c == '.' || c == '~' || isAlphaNum c
lexer a s = next lexer a s

skip n a ('(':s) = skip (n + 1) ('(':a) s
skip 0 a (')':s) = ('(':reverse a) : lexer "" s
skip n a (')':s) = skip (n - 1) (')':a) s
skip n a (x:xs)  = skip n (x:a) xs
skip _ _ _ = error "(Mismatched"

next f "" (x:xs) = f [x] xs
next f a x = reverse a : f "" x
