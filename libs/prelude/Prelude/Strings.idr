module Prelude.Strings

import Prelude.List

-- "super primitive"

-- "primitive operations"

||| The length-1 string of that character.
singleton : Char -> String
singleton c = pack [c]

||| If the string is not empty give the first character and the rest of the string.
uncons : String -> Maybe (Char, String)
--uncons _ = Just ('?', "<<uncons impl>>") --PRIMITIVE
uncons = ?missingUnconsPrim

||| Concatenate a list of strings interspersing some string between each piece.
join : String -> List String -> String
join delim strs = "<<join implementation>>" --PRIMITIVE

||| Reverse the characters of the string.
reverse : String -> String
reverse _ = "<<string reverse impl>>" --PRIMITIVE

||| If i is less than length of string, return the i-length prefix of
||| the string and the rest.
splitAt : Int -> String -> Maybe (String, String)
splitAt i s = Just ("<<split at", "impl>>")

||| If needle is found, return the prefix before the needle and the rest of
||| the string starting with the match.
breakOn : (needle : String) -> (haystack : String) -> Maybe (String, String)
breakOn needle s = Just ("<<breakOn", " impl>>")


-- "library operations"

infixr 7 ++

(++) : String -> String -> String
a ++ b = join "" [a,b]

instance Semigroup String where
  (<+>) = (++)

instance Monoid String where
  neutral = ""

foldr : (Char -> a -> a) -> a -> String -> a
foldr f acc0 s0 = foldr' acc0 s0 where
  foldr' acc s = case uncons s of
    Nothing => acc
    Just (c, s') => foldr' (f c acc) (assert_smaller s s')

unpack : String -> List Char
unpack s = foldr (::) [] s

pack : List Char -> String
pack cs = join "" (map singleton cs)

||| The number of characters in the string.
length : String -> Nat
length s = foldr (\_, k => S k) Z s
-- length s = PRIMITIVE
