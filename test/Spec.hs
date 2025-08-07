{-# LANGUAGE QuasiQuotes #-}

import Test.HUnit
import Data.Extended

import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import qualified Language.C.Inline as C

doubleConversion :: Assertion
doubleConversion = mapM_
    (\c -> c @=? (extendedToDouble (doubleToExtended c)))
    testCases
  where
    testCases = [ 1.0, 0.1, 100.2, 123.324, 12.4 ]

extendedRealToFrac :: Assertion
extendedRealToFrac = mapM_
        (\c -> do
            c @=? (realToFrac (realToFrac c :: Extended) :: Double)
            c @=? (realToFrac (doubleToExtended c) :: Double)
            c @=? (extendedToDouble (realToFrac c :: Extended))
        )
        testCases
  where
    testCases = [ 1.0, 0.1, 100.2, 123.324, 12.4 ]

extendedProperFraction :: Assertion
extendedProperFraction = mapM_
    (\c ->
        let (wholePart, fracPart) = properFraction (doubleToExtended c) :: (Int, Extended)
        in properFraction c @=? (wholePart, extendedToDouble fracPart)
    )
    testCases
  where
    testCases = [ 1.0, 0.1, 100.2, 123.324 ]

extendedDecode :: Assertion
extendedDecode = mapM_
    (\extended -> extended @=? uncurry encodeFloat (decodeFloat extended))
    testCases
  where
    testCases =
        [ extendedFromParts 0x0000 0x0000000000000000
        , extendedFromParts 0x6FFF 0x0000000000000000
        , extendedFromParts 0x6FFF 0x6FFFFFFFFFFFFFFF
        , extendedFromParts 0x0000 0x6FFFFFFFFFFFFFFF
        ]

cLongDouble :: Assertion
cLongDouble = do
    ldPtr <- malloc :: IO (Ptr CLongDouble)

    let ldValue = doubleToExtended 12.4
    poke ldPtr (CLongDouble ldValue)
    longDouble <- peek ldPtr :: IO CLongDouble

    ldValue @=? getLongDouble longDouble

    let d = extendedToDouble ldValue
    let voidPtr = castPtr ldPtr :: Ptr ()
    castedToDouble <- [C.block| double {
        long double* ptr = $(void* voidPtr);
        return (double)(*ptr);
    } |]
    d @=? realToFrac castedToDouble

tests :: Test
tests = TestList
    [ test doubleConversion
    , test extendedRealToFrac
    , test extendedProperFraction
    , test extendedDecode
    , test cLongDouble
    ]

main :: IO ()
main = runTestTTAndExit tests
