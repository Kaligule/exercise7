import Data.Word
import Data.Bits
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import qualified Test.QuickCheck as Q


type TwoChars = (Char, Char)

charValueList :: [(Char, Int)]
charValueList = zip (['0'..'9'] ++ ['a'..'f']) [0..]


fromTwoChars :: TwoChars -> Word8
fromTwoChars (x,y) = fromIntegral $ xInt * (2^4) + yInt
  where
    xInt = fromOneChar x
    yInt = fromOneChar y

    -- every Char not in the charvalueList is mapped to 0
    fromOneChar :: Char -> Int
    fromOneChar = fromMaybe 0 . flip lookup charValueList

toTwoChars :: Word8 -> TwoChars
toTwoChars byte = (toOneChar left4Bits, toOneChar right4Bits)
  where
    left4Bits = flip shiftR 4 . (.&.) (fromTwoChars ('f','0')) $  byte
    right4Bits =  (.&.) (fromTwoChars ('0','f')) $ byte

    -- make sure that the first 4 Bits are 0, or an error will occur
    toOneChar :: Word8 -> Char
    toOneChar = fromMaybe 'X' . flip lookup (map swap charValueList) . fromIntegral


xtime :: Word8 -> Word8
xtime p
  | testBit p 7 = xor (fromTwoChars ('1','b')) (shiftL p 1)
  | otherwise = shiftL p 1

xtime' :: TwoChars -> TwoChars
xtime' = toTwoChars . xtime . fromTwoChars

pmult :: Word8 -> Word8 -> Word8
pmult p q
  | p == zeroBits = zeroBits
  | testBit p 0 = q `xor` leftPartTimesq -- last bit of p is a 1
  | otherwise = leftPartTimesq
  where
    pDurchX = shiftR p 1
    leftPartTimesq = xtime (pmult pDurchX q)

    zeroBits = fromIntegral 0

-------------------------------------------------------------------------------------------------------

main = do
  testAll

testAll :: IO ()
testAll = do
  -- test converting
  Q.quickCheck prop_BigramRange
  mapM_ (Q.quickCheck . checkConverting) knownConverting
  Q.quickCheck prop_FromToFromTwoChars
  Q.quickCheck prop_ToFromToTwoChars

  -- test xtime
  mapM_ (Q.quickCheck . checkXtimePair) knownXtimePairs
  -- test pmult
  mapM_ (Q.quickCheck . checkPmult) knownPmultPairs
  Q.quickCheck prop_PmultComutative
  
from = fromTwoChars
to = toTwoChars

-- Test know pairs of coresponding (Char,Char) and Word8
-- I am not very creative here
checkConverting :: (TwoChars,  Word8) -> Bool
checkConverting (bigram, n) = (fromTwoChars bigram == n) && (bigram == toTwoChars n)

knownConverting :: [(TwoChars, Word8)]
knownConverting =
  [ (('f', 'f'), maxBound)
  , (('0', '0'), minBound)
  ]

-- Test the converting Word8 <-> TwoChars
prop_BigramRange :: TwoChars -> Bool
prop_BigramRange bigram = minBound <= n && n <= maxBound
  where
    n = fromTwoChars bigram

prop_FromToFromTwoChars :: TwoChars -> Bool
prop_FromToFromTwoChars bigram = (from . to . from) bigram == from bigram

prop_ToFromToTwoChars :: Word8 -> Bool
prop_ToFromToTwoChars byte = (to . from . to) byte == to byte


-- Test xtime with known pairs (x,y) where xtime' x == y
-- (from handout)
checkXtimePair :: (TwoChars, TwoChars) -> Bool
checkXtimePair (inp, outp) = xtime' inp == outp

knownXtimePairs :: [(TwoChars, TwoChars)]
knownXtimePairs =
  [ (('5','7'),('a','e'))
  , (('a','e'),('4','7'))
  , (('4','7'),('8','e'))
  , (('8','e'),('0','7'))
  ]

-- Test polynomial multiplication with already known pairs

checkPmult :: (TwoChars, TwoChars, TwoChars) -> Bool
checkPmult (x,y,xy) = pmult (fromTwoChars x) (fromTwoChars y) == (fromTwoChars xy)

knownPmultPairs :: [(TwoChars, TwoChars, TwoChars)]
knownPmultPairs =
  [(('0', '0'), ('0','0'), ('0','0'))
  ]

-- polynomial multiplication should be comutative
prop_PmultComutative :: Word8 -> Word8 -> Bool
prop_PmultComutative p q = pmult p q == pmult q p
