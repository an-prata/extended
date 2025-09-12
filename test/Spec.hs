{-# LANGUAGE QuasiQuotes #-}

import Test.HUnit
import Data.Extended

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

tests :: Test
tests = TestList
    [ test doubleConversion
    , test extendedRealToFrac
    , test extendedProperFraction
    , test extendedDecode
    ]

main :: IO ()
main = runTestTTAndExit tests
