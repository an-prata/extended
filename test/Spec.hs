import Data.Extended
import Test.HUnit

doubleConversion :: Assertion
doubleConversion = do
    _ <- mapM
        (\c -> c @=? (extendedToDouble (doubleToExtended c)))
        testCases
    pure ()
  where
    testCases = [ 1.0, 0.1, 100.2, 123.324 ]

extendedRealToFrac :: Assertion
extendedRealToFrac = do
    _ <- mapM
        (\c -> do
            c @=? (realToFrac (realToFrac c :: Extended) :: Double)
            c @=? (realToFrac (doubleToExtended c) :: Double)
            c @=? (extendedToDouble (realToFrac c :: Extended))
        )
        testCases
    pure ()
  where
    testCases = [ 1.0, 0.1, 100.2, 123.324 ]

extendedProperFraction :: Assertion
extendedProperFraction = do
    _ <- mapM
        (\c ->
            let (wholePart, fracPart) = properFraction (doubleToExtended c) :: (Int, Extended)
            in properFraction c @=? (wholePart, extendedToDouble fracPart)
        )
        testCases
    pure ()
  where
    testCases = [ 1.0, 0.1, 100.2, 123.324 ]

extendedDecode :: Assertion
extendedDecode = do
    _ <- mapM
        (\extended -> extended @=? uncurry encodeFloat (decodeFloat extended))
        testCases
    pure ()
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
