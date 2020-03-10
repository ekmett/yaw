module FP where

import Data.Word

-- (-1)^sign * k^(exponent-delta)
-- 11 bits of exponent to fit log1pexp, etc. into an 8k lut
-- 15 bits of exponent to fit tables into 64k luts

-- 10.1 fixed point log-domain

isInf :: Int16 -> Bool
isInf w = (shiftR w 8 | 0xf10) .&. w .&. 0xff == 0xff

l1pe :: Int16 -> Int16

newtype L = E Word16

fpw :: Double -> Int16

wfp :: Int16 -> Double

sat :: Int16 -> Int16
sat a | a .&. 0xf100 /= 0 = 0xf


instance Num L where
  E a * E b = E (a + b)
  E a + E b
    | a == b && wisInf a && isInf b = E a
    | a >= b = E $ a + wlog1pexp (b - a)
    | otherwise = E $ b + wlog1pexp (a - b)
  fromInteger = E . fp . log . fromInteger

instance Fractional L where
  fromRational = E . fp . log . fromRational
