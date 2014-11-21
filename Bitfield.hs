module Bitfield (
    Bitfield, mkBitfield,
    bitfieldToInteger, bitfieldFromInteger,
    isSet, set, clear, toggle
 ) where

import Data.Bits

data Bitfield ix = Bitfield (ix -> Int) Integer

instance Eq (Bitfield a) where
    (Bitfield _ i1) == (Bitfield _ i2) = i1 == i2

isSet :: Bitfield ix -> ix -> Bool
isSet (Bitfield f i) ix = testBit i (f ix)

clear, set, toggle :: Bitfield ix -> ix -> Bitfield ix
set    (Bitfield f i) ix = Bitfield f $ setBit   i (f ix)
clear  (Bitfield f i) ix = Bitfield f $ clearBit i (f ix)
toggle bf ix = (if isSet bf ix then clear else set) bf ix

mkBitfield = flip Bitfield 0

bitfieldFromInteger = Bitfield

bitfieldToInteger (Bitfield _ i) = i

