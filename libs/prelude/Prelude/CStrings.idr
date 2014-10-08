module Prelude.CStrings

import Builtins
import IO

import Prelude.List
import Prelude.CChars
import Prelude.Cast
import Prelude.Either
import Prelude.Foldable
import Prelude.Strings

||| Appends two strings together.
|||
||| Idris> "AB" ++ "C"
||| "ABC" : String
(++) : CString -> CString -> CString
(++) = prim__concat

||| Returns the first character in the specified string.
|||
||| Doesn't work for empty strings.
|||
||| Idris> strHead "A"
||| 'A' : Char
partial
strHead : CString -> CChar
strHead = prim__strHead

||| Returns the characters specified after the head of the string.
|||
||| Doesn't work for empty strings.
|||
||| Idris> strTail "AB"
||| "B" : String
||| Idris> strTail "A"
||| "" : String
partial
strTail : CString -> CString
strTail = prim__strTail

||| Adds a character to the front of the specified string.
|||
||| Idris> strCons 'A' "B"
||| "AB" : String
||| Idris> strCons 'A' ""
||| "A" : String
strCons : CChar -> CString -> CString
strCons = prim__strCons

||| Returns the nth character (starting from 0) of the specified string.
|||
||| Precondition: '0 < i < length s' for 'strIndex s i'.
|||
||| Idris> strIndex "AB" 1
||| 'B' : Char
partial
strIndex : CString -> Int -> CChar
strIndex = prim__strIndex

||| Reverses the elements within a String.
|||
||| Idris> reverse "ABC"
||| "CBA" : String
||| Idris> reverse ""
||| "" : String
reverse : CString -> CString
reverse = prim__strRev

null : Ptr
null = prim__null

-- Some more complex string operations

data CStrM : CString -> Type where
    StrNil : CStrM ""
    StrCons : (x : CChar) -> (xs : CString) -> CStrM (strCons x xs)

||| Version of 'strHead' that statically verifies that the string is not empty.
strHead' : (x : CString) -> So (not (x == "")) -> CChar
strHead' x p = assert_total $ prim__strHead x

||| Version of 'strTail' that statically verifies that the string is not empty.
strTail' : (x : CString) -> So (not (x == "")) -> CString
strTail' x p = assert_total $ prim__strTail x

-- we need the 'believe_me' because the operations are primitives
strM : (x : CString) -> CStrM x
strM x with (choose (not (x == "")))
  strM x | (Left p)  = really_believe_me $ 
                           StrCons (assert_total (strHead' x p))
                                   (assert_total (strTail' x p))
  strM x | (Right p) = really_believe_me StrNil

-- annoyingly, we need these assert_totals because StrCons doesn't have
-- a recursive argument, therefore the termination checker doesn't believe
-- the string is guaranteed smaller. It makes a good point.

||| Turns a string into a list of characters.
|||
||| Idris> unpack "ABC"
||| ['A', 'B', 'C'] : List Char
unpack : CString -> List CChar
unpack s with (strM s)
  unpack ""             | StrNil = []
  unpack (strCons x xs) | (StrCons x xs) = x :: assert_total (unpack xs)

||| Turns a Foldable of characters into a string.
pack : (Foldable t) => t CChar -> CString
pack = foldr strCons ""

||| Creates a string of a single character.
|||
||| Idris> singleton 'A'
||| "A" : String
singleton : CChar -> CString
singleton c = strCons c ""

instance Cast CString (List CChar) where
  cast = unpack

instance Cast (List CChar) CString where
  cast = pack

instance Cast CChar CString where
  cast = singleton

instance Semigroup CString where
  (<+>) = (++)

instance Monoid CString where
  neutral = ""


||| Splits the string into a part before the predicate
||| returns False and the rest of the string.
|||
||| Idris> span (/= 'C') "ABCD"
||| ("AB", "CD") : (String, String)
||| Idris> span (/= 'C') "EFGH"
||| ("EFGH", "") : (String, String)
span : (CChar -> Bool) -> CString -> (CString, CString)
span p xs with (strM xs)
  span p ""             | StrNil        = ("", "")
  span p (strCons x xs) | (StrCons _ _) with (p x)
    | True with (assert_total (span p xs))
      | (ys, zs) = (strCons x ys, zs)
    | False = ("", strCons x xs)

||| Splits the string into a part before the predicate
||| returns True and the rest of the string.
|||
||| Idris> break (== 'C') "ABCD"
||| ("AB", "CD") : (String, String)
||| Idris> break (== 'C') "EFGH"
||| ("EFGH", "") : (String, String)
break : (CChar -> Bool) -> CString -> (CString, CString)
break p = span (not . p)

||| Splits the string into parts with the predicate
||| indicating separator characters.
|||
||| Idris> split (== '.') ".AB.C..D"
||| ["", "AB", "C", "", "D"] : List String
split : (CChar -> Bool) -> CString -> List CString
split p xs = map pack (split p (unpack xs))

||| Removes whitespace (determined with 'isSpace') from
||| the start of the string.
|||
||| Idris> ltrim " A\nB"
||| "A\nB" : String
||| Idris> ltrim " \nAB"
||| "AB" : String
ltrim : CString -> CString
ltrim xs with (strM xs)
    ltrim "" | StrNil = ""
    ltrim (strCons x xs) | StrCons _ _
        = if (isSpace x) then assert_total (ltrim xs) else (strCons x xs)

||| Removes whitespace (determined with 'isSpace') from
||| the start and end of the string.
|||
||| Idris> trim " A\nB C "
||| "A\nB C" : String
trim : CString -> CString
trim xs = ltrim (reverse (ltrim (reverse xs)))

||| Splits a character list into a list of whitespace separated character lists.
|||
||| Idris> words' (unpack " A B C  D E   ")
||| [['A'], ['B'], ['C'], ['D'], ['E']] : List (List Char)
words' : List CChar -> List (List CChar)
words' s = case dropWhile isSpace s of
            [] => []
            s' => let (w, s'') = break isSpace s'
                  in w :: words' (assert_smaller s s'')

||| Splits a string into a list of whitespace separated strings.
|||
||| Idris> words " A B C  D E   "
||| ["A", "B", "C", "D", "E"] : List String
words : CString -> List CString
words s = map pack $ words' $ unpack s

||| Splits a character list into a list of newline separated character lists.
|||
||| Idris> lines' (unpack "\rA BC\nD\r\nE\n")
||| [['A', ' ', 'B', 'C'], ['D'], ['E']] : List (List Char)
lines' : List CChar -> List (List CChar)
lines' s = case dropWhile isNL s of
            [] => []
            s' => let (w, s'') = break isNL s'
                  in w :: lines' (assert_smaller s s'')

||| Splits a string into a list of newline separated strings.
|||
||| Idris> lines  "\rA BC\nD\r\nE\n"
||| ["A BC", "D", "E"] : List String
lines : CString -> List CString
lines s = map pack $ lines' $ unpack s

partial
foldr1 : (a -> a -> a) -> List a -> a
foldr1 _ [x] = x
foldr1 f (x::xs) = f x (foldr1 f xs)

partial
foldl1 : (a -> a -> a) -> List a -> a
foldl1 f (x::xs) = foldl f x xs

||| Joins the character lists by spaces into a single character list.
|||
||| Idris> unwords' [['A'], ['B', 'C'], ['D'], ['E']]
||| ['A', ' ', 'B', 'C', ' ', 'D', ' ', 'E'] : List Char
unwords' : List (List CChar) -> List CChar
unwords' [] = []
unwords' ws = assert_total (foldr1 addSpace ws)
        where
            addSpace : List CChar -> List CChar -> List CChar
            addSpace w s = w ++ (' ' :: s)

||| Joins the strings by spaces into a single string. 
|||
||| Idris> unwords ["A", "BC", "D", "E"]
||| "A BC D E" : String
unwords : List CString -> CString
unwords = pack . unwords' . map unpack

||| Returns the length of the string.
|||
||| Idris> length ""
||| 0 : Nat
||| Idris> length "ABC"
||| 3 : Nat
length : CString -> Nat
length = fromInteger . prim__zextInt_BigInt . prim_lenString

||| Lowercases all characters in the string.
|||
||| Idris> toLower "aBc12!"
||| "abc12!" : String
toLower : CString -> CString
toLower x with (strM x)
  strToLower ""             | StrNil = ""
  strToLower (strCons c cs) | (StrCons c cs) =
    strCons (toLower c) (toLower (assert_smaller (strCons c cs) cs))

||| Uppercases all characters in the string.
|||
||| Idris> toLower "aBc12!"
||| "ABC12!" : String
toUpper : CString -> CString
toUpper x with (strM x)
  strToLower ""             | StrNil = ""
  strToLower (strCons c cs) | (StrCons c cs) =
    strCons (toUpper c) (toUpper (assert_smaller (strCons c cs) cs ))
   
--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

isPrefixOf : CString -> CString -> Bool
isPrefixOf a b = isPrefixOf (unpack a) (unpack b)

isSuffixOf : CString -> CString -> Bool
isSuffixOf a b = isSuffixOf (unpack a) (unpack b)

isInfixOf : CString -> CString -> Bool
isInfixOf a b = isInfixOf (unpack a) (unpack b)


utf8encode : String -> CString
utf8encode _ = "<<utf8encode>>"

utf8decode : CString -> Either Int String
utf8decode _ = Left 932

utf8decode' : CString -> String
utf8decode' _ = "<<utf8decode'>>"

partial
putStr : CString -> IO ()
putStr x = mkForeign (FFun "putStr" [FString] FUnit) x

partial
putStrLn : CString -> IO ()
putStrLn x = putStr (x ++ "\n")

||| Read one line of input from stdin
partial
getLine : IO CString
getLine = prim_fread prim__stdin

