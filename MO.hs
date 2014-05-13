module MO128676 where

data Bit = L | R
     deriving (Show,Eq)

type Number = [Bit]

neg :: Bit -> Bit
neg L = R
neg R = L

-- zero is represented as an infinite stream of Ls
zero :: Number
zero = L : zero
-- infinity is represented as an infinite stream of Rs
inf :: Number
inf = R : inf

-- we can convert any number into an ordinary fraction/float, and conversely
-- (but note that toFrac x will not terminate if x is an infinite stream)
toFrac :: Fractional a => Number -> a
toFrac [] = 0
toFrac (L:x) = 1/(1 + 1 / toFrac x)
toFrac (R:x) = 1 + toFrac x

fromFrac :: (Fractional a,Ord a) => a -> Number
fromFrac x =
        if x < 0 then error "negative numbers not supported"
        else if x == 0 then zero
        else if x < 1 then L : fromFrac (1 / (-1 + 1 / x))
        else R : fromFrac (x - 1)

-- toFrac' x returns a (good) approximation of x
-- (note that toFrac' x always terminates, even if x is infinite)
toFrac' x = toFrac (take 100 x)

-- convert a continued fraction to canonical form (i.e., a list of integers)
canonize :: Number -> [Int]
canonize x = case x of
   R:y -> can y R 1
   L:y -> 0:can y L 1
 where
  can :: Number -> Bit -> Int -> [Int]
  can (R:x) R n = can x R (1 + n)
  can (L:x) L n = can x L (1 + n)
  can (L:x) R n = n : can x L 1
  can (R:x) L n = n : can x R 1

-- phi = [1;1,1,...]
phi :: Number
phi = R : L : phi
-- toFrac' phi == 1.618033988749895

-- e = [2;1,2,1,1,4,1,...,1,2k,1,...]
e :: Number
e = eacc R 0
  where
   eacc :: Bit -> Int -> Number
   eacc b k = b : take (2 * k) (repeat (neg b)) ++ b : eacc (neg b) (k+1)
-- toFrac' e == 2.7182818284590455
-- (take 20 $ canonize e) == [2,1,2,1,1,4,1,1,6,1,1,8,1,1,10,1,1,12,1,1]

-- a four-state Raney transducer for implementing x |-> x + 1/2
-- see http://mathoverflow.net/questions/128676/what-is-the-effect-of-adding-1-2-to-a-continued-fraction

data State4 = S1 | S2 | S3 | S4
machine :: State4 -> Number -> Number
machine S1 (R : x) = R : machine S1 x
machine S1 (L : L : x) = L : R : machine S2 x
machine S1 (L : R : x) = R : L : L : machine S3 x
machine S2 (R : x) = R : R : R : R : machine S2 x
machine S2 (L : R : x) = R : R : machine S4 x
machine S2 (L : L : R : x) = R : L : machine S1 x
machine S2 (L : L : L : R : x) = R : L : L : L : machine S3 x
machine S2 (L : L : L : L : x) = L : machine S2 x
machine S3 (L : x) = L : L : L : L : machine S3 x
machine S3 (R : L : x) = L : L : machine S1 x
machine S3 (R : R : L : x) = L : R : machine S4 x
machine S3 (R : R : R : L : x) = L : R : R : R : machine S2 x
machine S3 (R : R : R : R : x) = R : machine S3 x
machine S4 (L : x) = L : machine S4 x
machine S4 (R : R : x) = R : L : machine S3 x
machine S4 (R : L : x) = L : R : R : machine S2 x

-- machine S1 x == (2x + 1)/2 = x + 1/2
-- machine S2 x == 4x
-- machine S3 x == x/4
-- machine S4 x == 2x/(x + 2) = 2 / (1 + 2/x)

-- Examples:
-- (take 20 $ machine S1 phi) == [R,R,L,L,L,L,L,L,L,L,R,R,L,L,L,L,L,L,L,L]
-- (take 10 . canonize $ machine S1 phi) == [2,8,2,8,2,8,2,8,2,8]
-- (toFrac' $ machine S1 phi) == 2.118033988749895
-- (toFrac' $ machine S1 e) == 3.2182818284590455
-- (take 100 . canonize $ machine S1 e) == [3,4,1,1,2,1,1,2,1,2,1,1,1,1,1,1,1,2,2,2,1,1,1,2,1,1,1,2,3,2,1,1,1,3,1,1,1,2,4,2,1,1,1,4,1,1,1,2,5,2,1,1,1,5,1,1,1,2,6,2,1,1,1,6,1,1,1,2,7,2,1,1,1,7,1,1,1,2,8,2,1,1,1,8,1,1,1,2,9,2,1,1,1,9,1,1,1,2,10,2]
