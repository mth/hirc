{-
 - Minimal utf8 <-> latin1 autoconversion functions.
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
module Utf8Conv (utf8Decode, utf8Encode) where

import Data.Bits
import Data.Char

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

