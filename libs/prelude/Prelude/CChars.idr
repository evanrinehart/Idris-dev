module Prelude.CChars
-- Functions operating over CChars

import Prelude.Bool
import Prelude.Classes
import Prelude.List
import Builtins

||| Return the ASCII representation of the character.
chr : Int -> CChar
chr x = prim__intToChar x

||| Convert the number to its ASCII equivalent.
ord : CChar -> Int
ord x = prim__charToInt x

||| Returns true if the character is in the range [A-Z].
isUpper : CChar -> Bool
isUpper x = x >= 'A' && x <= 'Z'

||| Returns true if the character is in the range [a-z]
isLower : CChar -> Bool
isLower x = x >= 'a' && x <= 'z'

||| Returns true if the character is in the ranges [A-Z][a-z].
isAlpha : CChar -> Bool
isAlpha x = isUpper x || isLower x

||| Returns true if the character is in the range [0-9]
isDigit : CChar -> Bool
isDigit x = (x >= '0' && x <= '9')

||| Returns true if the character is in the ranges [A-Z][a-z][0-9]
isAlphaNum : CChar -> Bool
isAlphaNum x = isDigit x || isAlpha x

||| Returns true if the character is a whitespace character.
isSpace : CChar -> Bool
isSpace x = x == ' '  || x == '\t' || x == '\r' ||
            x == '\n' || x == '\f' || x == '\v' ||
            x == '\xa0'

||| Returns true if the character represents a new line.
isNL : CChar -> Bool
isNL x = x == '\r' || x == '\n'

||| Convert a letter to the corresponding upper-case letter, if any.
||| Non-letters are ignored.
toUpper : CChar -> CChar
toUpper x = if (isLower x)
               then (prim__intToChar (prim__charToInt x - 32))
               else x

||| Convert a letter to the corresponding lower-case letter, if any.
||| Non-letters are ignored.
toLower : CChar -> CChar
toLower x = if (isUpper x)
               then (prim__intToChar (prim__charToInt x + 32))
               else x

||| Returns true if the character is a hexadecimal digit i.e. in the range [0-9][a-f][A-F]
isHexDigit : CChar -> Bool
isHexDigit x = elem (toUpper x) hexChars where
  hexChars : List CChar
  hexChars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
              'A', 'B', 'C', 'D', 'E', 'F']

||| Returns true if the character is an octal digit.
isOctDigit : CChar -> Bool
isOctDigit x = (x >= '0' && x <= '7')


