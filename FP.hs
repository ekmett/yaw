-- this doesn't need to be fast, just right, using it to produce lookup tables for the x16
module FP where

import Data.Word
import Data.Bits
import Numeric

-- | not 2 like IEEE 754 doubles, why? no mantissa
pattern K :: Double
pattern K = 1.006

pattern Km1 :: Double
pattern Km1 = 0.006

logK :: Double
logK = log1p km1

-- | not 1023 like IEEE 754 double
-- why? to avoid the need for a 16 bit subtraction on multiplication, now i can subtract using just the high byte!
pattern D :: Num a => a
pattern D = 1024

-- we can represent exponents between 2047-1024 = 1023 down to 0-1024 = -1024
maxE :: Word16
maxE = 2047

-- | hi:SN00EEEE|lo:EEEEEEE0
-- (-1)^S * K^(E-D)
-- S=0 positive
-- LSB=0 so can use directly as an index into an 8k Bank on the x16 after masking S
-- N=1 indicates nan
newtype F = F Word16

-- False positive, True negative
sgn :: Word16 -> Bool
sgn a = testBit a 15

sgned :: Bool -> Word16 -> Word16
sgned False w = w
sgned True w = w .|. 0xf000

toDouble :: F -> Double
toDouble (F w)
  | w .&. 0x7001 = error $ "dbl: " ++ show w
  | sgn w = negate v
  | otherwise = v
  where v = exp $ logK * fromIntegral (unsafeShiftR (w .&. 0x0ffe) 1 - 1024)

fromDouble :: Double -> F
fromDouble d = F $ sgned (d < 0 || isNegativeZero d) $ unsafeShiftL e 1 where
  e = max 0 $ min 2047 $ round (log (abs d) / logK + D) -- between 0 and infinity, bias rounding?

-- | 
-- @
-- logk1pk x = logBase k (1+k^x) -- 'softplus'
--    = log1p (k^x) / log k
--    = log1p (exp (log (k^x))) / log k
--    = log1p (exp (x log k)) / log k
--    = log1pexp (x log k) / log k
--    = log1pexp (x log k) / log1p km1
--    = log1pexp (x log1p km1) / log1p km1
-- @
logk1pk :: F -> F
logk1pk = fromDouble $ log1pexp (toDouble x * logK) / logK

-- | 
-- @
-- logk1mk x = logBase k (1-k^-x)
-- @
logk1mk :: F -> F
logk1mk = fromDouble $ log1mexp (toDouble x * logK) / logK

-- |
-- @
-- logk x = logBase k x
-- @
logk :: F -> F
logk a = fromDouble $ log (toDouble x) / logK

-- |
-- @
-- logk1p x = logBase k (1+x)
-- @
logk1p :: Double -> F
logk1p d = fromDouble $ log1p (toDouble x) / logK

-- |
-- @
-- logk1m x = logBase k (1-x)
-- @
logk1m :: F -> F
logk1m = logk1p (negate x)

-- |
-- @
-- expkm1 x = (k^x)-1
-- @
expkm1 :: F -> F
expkm1 = fromDouble $ expm1 (toDouble x * logK) 

-- k^x
expk :: F -> F
expk = fromDouble $ exp (toDouble x * logK)

isinf :: Word16 -> Bool
isinf w = w .&. 0xffe == 0xffe

-- only works on unsigned encoded words
sat :: Word16 -> Word16
sat x 
  | x .&. 1 == error $ "sat: " ++ show x
  | x .&. 0x7000 == 0 = x  -- overflow into gutter
  | otherwise = 0xffe

instance Eq F where
  F a == F b = a == b -- handle a nan rep when running on a bigger machine?

instance Num F where
  F a * F b F $ sgn (complement (xor a b) .&. 0x8000 /= 0) + if
    | c .&. 0xf000 == 0 = c     -- common case
    | c .&. 0x8000 == 0 = 0     -- underflow, 0
    | otherwise         = 0xffe -- overflow, inf
    where
      c = (a .&. 0xffe) + (b .&. 0xffe) - unsafeShiftL delta 1

  F a + F b
    | a == b && isInf a && isInf b = E a
    | a >= b = E $ a + logk1pk (b - a)
    | otherwise = E $ b + log1pk (a - b)
    where
      parity = xor a b .&. 0x8000 /= 0
      wa = a .&. 0xffe -- 2 e + 2048
      wb = b .&. 0xffe -- 2 e + 2048
  negate (F a) = F $ xor 0x8000 a
  abs (F a) = F $ 0xffe .&. a
  signum (F a)
    | a .&. 0xffe == 0 = F 0 -- 0
    | otherwise = F $ sgned (sgn a) 0x0800 -- +/-1
  fromInteger = F . fromDouble . log . fromInteger

instance Fractional F where
  recip (F a) = (a .&. 0x8000) .|. (4096- (a.&.0xffe))
  fromRational = F . fromDouble . log . fromRational
