module Prelude.Strings

import Prelude.List

infixr 7 ++

(++) : String -> String -> String

instance Semigroup String where
  (<+>) = (++)

instance Monoid String where
  neutral = ""

pack : List Char -> String
pack _ = "hello world"

unpack : String -> List Char
unpack _ = ['h', 'e', 'l', 'l', 'o']

strIndex : String -> Int -> Char

join : String -> List String -> String
join delim strs = "<<join implementation>>"

