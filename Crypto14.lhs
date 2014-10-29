Exercise 7
==========

Importe
-------

Damit die aus dem Weg sind.

\begin{code}
import Data.Word --basicly bytes
import Data.Bits
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import qualified Test.QuickCheck as Q
\end{code}

Definitionen
-----------
Typesynonym für bessere Lesbarkeit
\begin{code}
type TwoChars = (Char, Char)
\end{code}

Liste, um Hexwerte nachschlagen zu können.

\begin{code}
charValueList :: [(Char, Int)]
charValueList = zip (['0'..'9'] ++ ['a'..'f']) [0..]
\end{code}

Das Nullpolynom werden wir noch brauchen.

\begin{code}
justZeros :: Word8
justZeros = fromIntegral 0
\end{code}

TwoChars -> Word8
-----------------

\begin{code}
fromTwoChars :: TwoChars -> Word8
fromTwoChars (x,y) = fromIntegral $ xInt*(2^4) + yInt
  where
    xInt = fromOneChar x
    yInt = fromOneChar y
\end{code}

Die einzelnen Buchstaben schauen wir in der ```charValueList``` nach. Alles, was da nicht drin steht wird einfach auf 0 abgebildet.

\begin{code}
fromOneChar :: Char -> Int
fromOneChar = fromMaybe 0 . flip lookup charValueList
\end{code}

Word8 -> TwoChars
-----------------
\begin{code}
toTwoChars :: Word8 -> TwoChars
toTwoChars byte =
          (toOneChar lHalfByte, toOneChar rHalfByte)
  where
    rHalfByte = (.&.) (fromTwoChars ('0','f'))
              $ byte

    lHalfByte = flip shiftR 4
              . (.&.) (fromTwoChars ('f','0'))
              $  byte

    -- make sure that the first 4 Bits are 0
    toOneChar :: Word8 -> Char
    toOneChar = fromMaybe 'X'
              . flip lookup (map swap charValueList)
              . fromIntegral
\end{code}

xtime
-----

Wenn das erste Bit ```1``` ist macht xtime einen Linksshift und verxodert mit dem 'magischen' Polynom. Wenn nicht, machen wir nur den Linksshift.

\begin{code}
xtime :: Word8 -> Word8
xtime p
  | testBit p 7 = xor magicPolynom (shiftL p 1)
  | otherwise = shiftL p 1
  where
    magicPolynom = fromTwoChars ('1', 'b')
\end{code}

. . .

Zweite Variante mit anderer Signatur, damit man einfacher auf TwoChars rechenen kann.

\begin{code}
xtime' :: TwoChars -> TwoChars
xtime' = toTwoChars . xtime . fromTwoChars
\end{code}

Polynom plutimikation
---------------------

```pmult p q``` -> Fallunterscheidung: Hat das Polynom ```p```...

* ... nur Nullkoefizienten?
* ... Konstanten Anteil 1?
* ... Konstanten Anteil 0?

. . .

\begin{code}
pmult :: Word8 -> Word8 -> Word8
pmult p q
  | p == justZeros = justZeros
  | testBit p 0    = q `xor` leftPartTimesq
  | otherwise      = leftPartTimesq
  where
    pDurchX = shiftR p 1
    leftPartTimesq = xtime (pmult pDurchX q)
\end{code}

Polynom plutimikation
---------------------

Nochmal eine Variante mit anderer Signatur. Das hier ist die Funktion, die man später als Mensch benutzen wird.

\begin{code}
pmult' :: TwoChars -> TwoChars -> TwoChars
pmult' x y = toTwoChars
           $ pmult (fromTwoChars x) (fromTwoChars y)
\end{code}

Ausgabe
-------

Ausgabe, wenn wir ein Produkt ausrechnen.

\begin{code}
showMult :: TwoChars -> TwoChars -> IO()
showMult ab cd = putStrLn
                 $ showTwoChars ab ++ "*"
                 ++ showTwoChars cd ++ "=" ++
                 showTwoChars (pmult' ab cd)
  where
    showTwoChars :: TwoChars -> String
    showTwoChars (a,b) = '(' : a : b : ")"
\end{code}

Lösungssatz
-----------

\begin{code}
main = do
  showMult ('5','e') ('8','3')  -- (5e)*(83)=(36)
  showMult ('7','f') ('4','a')  -- (7f)*(4a)=(d9)
  showMult ('4','0') ('7','5')  -- (40)*(75)=(44)
\end{code}

Dankeschön
==========

Anhang
------

Ab hier werden nur noch meine Tests dokumentiert. Seeeehr langweilig.
Gehört aber streng genommen zum Code schreiben dazu.











Testing
=======

Tests ausführen
---------------

\begin{code}

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
  Q.quickCheck prop_PmultExplicit



\end{code}

Testing: Konvertierung TwoChar <-> Word8
----------------------------------------

Für manche Paare von TwoChar und Word8 wissen wir, dass sie zusammengehören. Die können wir hier testen.

\begin{code}
checkConverting :: (TwoChars,  Word8) -> Bool
checkConverting (bigram, n) =
  (fromTwoChars bigram == n)
  && (bigram == toTwoChars n)
\end{code}

. . .

Alle mir bekannten Paare in einer Liste. Es gibt keinen Grund für überzogene Kreativität.
\begin{code}
knownConverting :: [(TwoChars, Word8)]
knownConverting =
  [ (('f', 'f'), maxBound)
  , (('0', '0'), minBound)
  ]
\end{code}

Testing: Konvertierung TwoChar <-> Word8
----------------------------------------

Durch ```TwoChar``` erzeugte Polynome sollten in einem gewissen Ramen liegen.

\begin{code}
prop_BigramRange :: TwoChars -> Bool
prop_BigramRange bigram = minBound <= n && n <= maxBound
  where
    n = fromTwoChars bigram
\end{code}

Testing: Konvertierung TwoChar <-> Word8
----------------------------------------

Beim wiederhohlten hin und her konvertieren sollte sich nichts verändern.

\begin{code}
from = fromTwoChars
to = toTwoChars

prop_FromToFromTwoChars :: TwoChars -> Bool
prop_FromToFromTwoChars bigram =
  (from . to . from) bigram == from bigram

prop_ToFromToTwoChars :: Word8 -> Bool
prop_ToFromToTwoChars byte =
  (to . from . to) byte == to byte
\end{code}

Testing: xtime
--------------

Wiederum gibt es Paare, wo wir ```p``` und ```xtime p``` schon kennen.

\begin{code}
checkXtimePair :: (TwoChars, TwoChars) -> Bool
checkXtimePair (inp, outp) = xtime' inp == outp
\end{code}

. . .

Hier klauen wir einfach die Beispiele aus dem Handout der letzten Stunde.
\begin{code}
knownXtimePairs :: [(TwoChars, TwoChars)]
knownXtimePairs =
  [ (('5','7'),('a','e'))
  , (('a','e'),('4','7'))
  , (('4','7'),('8','e'))
  , (('8','e'),('0','7'))
  ]
\end{code}

Testing: Polynom Multiplikation mit pmult
-----------------------------------------

Wieder mit Paaren, die man schon kennt.
\begin{code}

checkPmult :: (TwoChars, TwoChars, TwoChars) -> Bool
checkPmult (x,y,xy) = pmult' x y == xy

knownPmultPairs :: [(TwoChars, TwoChars, TwoChars)]
knownPmultPairs =
  [ (('5', '7'), ('0', '2'), ('a','e'))
  , (('5', '7'), ('0', '4'), ('4','7'))
  , (('5', '7'), ('0', '8'), ('8','e'))
  , (('5', '7'), ('1', '0'), ('0','7'))
  ]
\end{code}
Auch hier ist das Handout der letzten Übung eine gute Quelle.

Testing: Polynom Multiplikation mit pmult
-----------------------------------------

Der Ring der Polynome ist kommutativ.

\begin{code}
prop_PmultComutative :: Word8 -> Word8 -> Bool
prop_PmultComutative p q = pmult p q == pmult q p
\end{code}

Testing: Polynom Multiplikation mit pmult
-----------------------------------------

Bei der Null und der Eins im Polynomring stehen die Ergebnisse schon fest, die können wir auch testen.  

\begin{code}
prop_PmultExplicit :: Word8 -> Bool
prop_PmultExplicit p =
  pmult p justZeros == justZeros
  && pmult p (fromTwoChars ('0','1')) == p
\end{code}

Dankeschön
==========
