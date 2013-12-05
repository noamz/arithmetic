data Game = Game { left :: [Game], right :: [Game] }
  deriving Show

le :: Game -> Game -> Bool
le y x = not $ any (\xr -> le xr y) (right x) || any (\yl -> le x yl) (left y)

ge x y = le y x
eq x y = le x y && le y x
lt x y = le x y && not (le y x)
gt x y = lt y x

instance Num Game where
  x + y =
    Game { left =  [ xl + y | xl <- left x ] ++ [ x + yl | yl <- left y],
           right = [ xr + y | xr <- right x ] ++ [ x + yr |  yr <- right y] }
  negate x =
    Game { left =  [ negate xr | xr <- right x],
           right = [ negate xl | xl <- left x] }
  x - y = x + (negate y)
  x * y = Game { left =  [xl * y + x * yl - xl * yl | xl <- left x, yl <- left y] ++
                         [ xr * y + x * yr - xr * yr | xr <- right x, yr <- right y] ,
                 right = [xl * y + x * yr - xl * yr | xl <- left x, yr <- right y] ++
                         [ xr * y + x * yl - xr * yl | yl <- left y, xr <- right x] }

  fromInteger 0 = Game { left = [], right = [] }
  fromInteger n = if n < 0 then negate (fromInteger (negate n)) else Game { left = [fromInteger (n-1)], right = [] }

  signum g = error "sign undefined for general games"
  abs g = error "absolute value undefined for general games"

n :: Integer -> Game
n x = fromInteger x

star = Game { left = [n 0] , right = [n 0] }

test1 = eq (n 2 + n 2) (n 4)                 -- True
test2 = eq (n 2 * n 3) (n 3 * n 2)           -- True after a second or two
test3 = eq (n 8 - n 4) (- (n 4 - n 8))       -- True
test4 = eq (n 3 * n 3) (n 9)                 -- ?? I lose patience after about 15 minutes
test5 = eq (star * n 3) (star + star + star) -- True
test6 = eq ((star + star) * n 2 + star) (star + n 2 * (star + star))  -- True after a second or two
test7 = eq ((star + star) * n 3) (n 3 * (star + star))                -- ?? lose patience

