-- https://en.wikipedia.org/wiki/Lychrel_number

digacc n acc = if n < 10 then n : acc else digacc (n `div` 10) (n `mod` 10 : acc)

digits n = digacc n []

undigits ds = foldl (\n d -> d + 10*n) (head ds) (tail ds)

isPalindrome n = digits n == reverse (digits n)

revAddSeq :: Integer -> [Integer]
revAddSeq n =
  let s = digits n in
  let s' = reverse s in
  if s == s' then [n] else
    n : revAddSeq (n + undigits s')

palIndex :: Integer -> Int
palIndex n = length (revAddSeq n) - 1

{-
> revAddSeq 59
[59,154,605,1111]
> palIndex 59
3
> revAddSeq 89
[89,187,968,1837,9218,17347,91718,173437,907808,1716517,8872688,17735476,85189247,159487405,664272356,1317544822,3602001953,7193004016,13297007933,47267087164,93445163438,176881317877,955594506548,1801200002107,8813200023188]
> palIndex 1999291987030606810
261
> palIndex 196
  C-c C-cInterrupted.
-}
