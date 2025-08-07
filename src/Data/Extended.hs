{-# LANGUAGE NamedFieldPuns #-}

module Data.Extended
    ( Extended
    , extendedToDouble, doubleToExtended
    , extendedFromParts, extendedToParts
    ) where

import Data.Bits
import Data.Int
import Data.Word

-- | An 80 bit extended float. If you are looking for greater precision then this probably won't
-- help - many operations defer to their implementation for 'Double', and convert to and from
-- 'Double' to give values.
--
-- Noteably the 'Eq' instance for 'Extended' /does not/ defer to 'Double''s instance of the same, so
-- it /is/ possible to distinguish values from one another at higher precision, even if they cannot
-- be operated on to that precision.
data Extended = Extended
    { signExponent :: Word16  -- ^ 16 most significant bits of the 'Extended'.
    , mantissa :: Word64  -- ^ 64 least significant bits of the 'Extended'.
    }

data ExtendedClass
    = Normalized
    | Denomalized
    | Zero
    | Infinity
    | NaN
    deriving (Show, Eq)

-- | Build an 'Extended' from a 'Word16' holding the sign bit and exponent, and a 'Word64' holding
-- the mantissa.
-- 
-- This function is to 'extendedToParts' as 'encodeFloat' is to 'decodeFloat':
-- 
-- @
-- uncurry extendedFromParts (extendedToParts e) == e
-- @
--
-- However, unlike 'encodeFloat', this function takes the actual binary representation of the
-- 'Extended'.
extendedFromParts :: Word16 -> Word64 -> Extended
extendedFromParts = Extended
-- | Breaks the 'Extended' down into the first 16 bits, which hold the sign bit and exponent, and
-- the remaining 64 bits which follow and contain the mantissa, which is made up of a single integer
-- bit in the most significant position, followed by a fraction part in the last 63 bits.
--
-- This function is to 'extendedFromParts' as 'decodeFloat' is to 'encodeFloat':
-- 
-- @
-- uncurry extendedFromParts (extendedToParts e) == e
-- @
--
-- However, unlike 'decodeFloat', this function gives the actuall binary representation of the
-- 'Extended', where the sign is packaged with the exponent and not the mantissa, meaning that the
-- value of the 'Extended' cannot be calculated using this function without further extracting
-- binary parts.
extendedToParts :: Extended -> (Word16, Word64)
extendedToParts Extended { signExponent, mantissa } = (signExponent, mantissa)

-- | Convert this 'Extended' to a 'Double'.
extendedToDouble :: Extended -> Double
extendedToDouble extended = if signBit extended == 1
    then -1 * fraction * (2**exponentValue)
    else fraction * (2**exponentValue)
  where
    fraction = fromIntegral (mantissa extended) / (2**63)
    exponentValue = fromIntegral (exponentBits extended) - 16383

-- | Convert the given 'Double' to an 'Extended'.
doubleToExtended :: Double -> Extended
doubleToExtended double = uncurry encodeFloat (decodeFloat double)

-- | Class of the 'Extended' value.
classOf :: Extended -> ExtendedClass
classOf extended =
    if fromIntegral (exponentBits extended) == exponentMax + 1 then
        if fractionBits extended == 0 then Infinity
        else NaN
    else if mantissa extended == 0 then Zero
    else if integerBit extended == 1 then Normalized
    else Denomalized

-- | Value of the sign bit, 1 if set 0 if not.
signBit :: Extended -> Word16
signBit Extended { signExponent } = (signExponent .&. 0x8000) `shiftR` 15

-- | The 15 exponent bits, or the absolute value of the top 16 bits.
exponentBits :: Extended -> Word16
exponentBits Extended { signExponent } = signExponent .&. 0x7FFF

-- | The single ineteger bit.
integerBit :: Extended -> Word64
integerBit Extended { mantissa } = mantissa `shiftR` 63

-- | All bits following the integer bit.
fractionBits :: Extended -> Word64
fractionBits Extended { mantissa } = mantissa `shiftL` 1 `shiftR` 1

-- | Maximum value of the exponent.
exponentMax :: Int16
exponentMax = 32766 - fromIntegral bias

-- | Exponent's bias.
bias :: Int
bias = 16383

instance RealFloat Extended where
    floatRadix = const 2
    floatDigits = const 64
    floatRange = const (-bias, 32766 - bias)
    decodeFloat extended =
        ( toInteger (mantissa extended) * if signBit extended == 1 then -1 else 1
        , fromIntegral (exponentBits extended) - bias - 63
        )
    encodeFloat signMantissa e = Extended
        { signExponent = fromIntegral (e + bias + 63) .&. 0x7FFF .|. if signMantissa < 0 then 0x8000 else 0x0000
        , mantissa = fromInteger $ abs signMantissa
        }
    isNaN extended = classOf extended == NaN
    isInfinite extended = classOf extended == Infinity
    isDenormalized extended = classOf extended == Denomalized
    isNegativeZero extended = classOf extended == Zero
    isIEEE = const True

instance Real Extended where
    toRational = toRational . extendedToDouble

instance Floating Extended where
    pi = realToFrac (pi :: Double)
    exp = realToFrac . exp . extendedToDouble
    log = realToFrac . log . extendedToDouble
    sin = realToFrac . sin . extendedToDouble
    cos = realToFrac . cos . extendedToDouble
    asin = realToFrac . asin . extendedToDouble
    acos = realToFrac . acos . extendedToDouble
    atan = realToFrac . atan . extendedToDouble
    sinh = realToFrac . sinh . extendedToDouble
    cosh = realToFrac . cosh . extendedToDouble
    asinh = realToFrac . asinh . extendedToDouble
    acosh = realToFrac . acosh . extendedToDouble
    atanh = realToFrac . atanh . extendedToDouble

instance RealFrac Extended where
    properFraction extended = (wholePart, doubleToExtended fractionPart)
      where
        (wholePart, fractionPart) = properFraction $ extendedToDouble extended

instance Fractional Extended where
    fromRational rational = doubleToExtended (fromRational rational :: Double)
    recip = doubleToExtended . recip . extendedToDouble

instance Num Extended where
    (+) extended = doubleToExtended . (+) (extendedToDouble extended) . extendedToDouble
    (*) extended = doubleToExtended . (*) (extendedToDouble extended) . extendedToDouble
    abs Extended { signExponent, mantissa } = Extended
        { signExponent = signExponent .&. 0x7FFF
        , mantissa
        }
    signum Extended { signExponent } = Extended
        { signExponent = signExponent .&. 0x8000
        , mantissa = 0x8000000000000000
        }
    fromInteger = realToFrac
    negate Extended { signExponent, mantissa } = Extended
        { signExponent = signExponent `complementBit` 15
        , mantissa
        }

instance Ord Extended where
    compare extended = compare (extendedToDouble extended) . extendedToDouble

instance Eq Extended where
    a == b = case (classOf a, classOf b) of
        (Infinity, Infinity) -> signBit a == signBit b
        (Zero, Zero) -> True
        (NaN, _) -> False
        (_, NaN) -> False
        (_, _) -> signExponent a == signExponent b && mantissa a == mantissa b

instance Show Extended where
    show = show . extendedToDouble
    
