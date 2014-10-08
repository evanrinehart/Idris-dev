module Prelude.Cast

||| Type class for transforming a instance of a data type to another type.
class Cast from to where
    ||| Perform a cast operation.
    |||
    ||| @orig The original type.
    cast : (orig : from) -> to

-- String casts

instance Cast CString Int where
    cast = prim__fromStrInt

instance Cast CString Float where
    cast = prim__strToFloat

instance Cast CString Integer where
    cast = prim__fromStrBigInt

instance Cast String Int where
    cast _ = 0

instance Cast CString String where
    cast _ = "<<loosely interpret cstring text, ignore non-ascii>>"

instance Cast String CString where
    cast _ = "<<render only ascii as a CString>>"

-- Int casts

instance Cast Int CString where
    cast = prim__toStrInt

instance Cast Int Float where
    cast = prim__toFloatInt

instance Cast Int Integer where
    cast = prim__sextInt_BigInt

instance Cast Int CChar where
    cast = prim__intToChar

-- Float casts

instance Cast Float CString where
    cast = prim__floatToStr

instance Cast Float Int where
    cast = prim__fromFloatInt

-- Integer casts

instance Cast Integer CString where
    cast = prim__toStrBigInt

-- Char casts

instance Cast CChar Int where
    cast = prim__charToInt

instance Cast CChar Char where
    cast _ = '?'
