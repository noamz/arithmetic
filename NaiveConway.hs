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

g :: Game -> Game
g x = x

test1 = eq (g 2 + g 2) (g 4)                 -- True
test2 = eq (g 2 * g 3) (g 3 * g 2)           -- True after a second or two
test3 = eq (g 8 - g 4) (- (g 4 - g 8))       -- True
test4 = eq (g 3 * g 3) (g 9)                 -- False after a couple minutes
