import Data.List (nub, sort)

triple :: [[Int]]
triple
  = filter (\xs -> xs /= [1,1,1] && xs /= [0,0,0])
  $ sequence
  $ replicate 3 [0,1]

quad :: [[Int]]
quad
  = filter (\xs -> xs /= [1,1,0,0] && xs /= [0,0,1,1])
  $ sequence
  $ replicate 4 [0,1]

pyramid1
  = filter (\xss -> map (!!0) xss /= [1,1,1] && map (!!0) xss /= [0,0,0])
  $ sequence
  $ replicate 3 triple

-- 3188646 ≈ 2^27 / 42
pyramid2
  = filter (\xsss -> map ((!!0) . (!!0)) xsss /= [1,1,1] && map ((!!0) . (!!0)) xsss /= [0,0,0])
  $ sequence
  $ replicate 3 pyramid1

knot4
  = filter (\[[x,a,b], [y,c,d], [a1,d1,g], [b1,c1,h]] ->
    a == a1 && b == b1 && c == c1 && d == d1)
  $ sequence
  $ replicate 4 triple

knot6
  = filter (\[[a,b,c], [a1,d,e], [b1,f,g], [c1,h,x], [d1,f1,h1], [e1,g1,x1]] ->
    a == a1 && b == b1 && c == c1 && d == d1 && e == e1 && f == f1 && g == g1 && h == h1 && x == x1)
  $ sequence
  $ replicate 6 triple

chain_3_3_3 -- 30/32 [00100, 11011]
  = nub $ sort
  $ map (\[[a,b,x1], [x2,c,y1], [y2,d,e]] -> [a,b,c,d,e])
  $ filter (\[[a,b,x1], [x2,c,y1], [y2,d,e]] -> x1 == x2 && y1 == y2)
  $ sequence
  $ replicate 3 triple

chain_4_3 -- 30/32 [00100, 11011]
  = nub $ sort
  $ map (\[[a,b,c,d], [x,y,z]] -> [a,b,c,y,z])
  $ filter (\[[a,b,c,d], [x,y,z]] -> d == x)
  $ sequence
  $ [quad, triple]

fromBits :: [Int] -> Int
fromBits = foldl (\r x -> 2*r + x) 0

diff :: [Int] -> [Int]
diff xs = zipWith subtract xs $ tail xs
