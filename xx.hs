triple :: [[Int]]
triple
  = filter (\xs -> xs /= [1,1,1] && xs /= [0,0,0])
  $ sequence
  $ replicate 3 [0,1]

pyramid1
  = filter (\xss -> map (!!0) xss /= [1,1,1] && map (!!0) xss /= [0,0,0])
  $ sequence
  $ replicate 3 triple

-- 3188646 ≈ 2^27 / 42
pyramid2
  = filter (\xsss -> map ((!!0) . (!!0)) xsss /= [1,1,1] && map ((!!0) . (!!0)) xsss /= [0,0,0])
  $ sequence
  $ replicate 3 pyramid1


knot
  = filter (\[[a,b,c], [a1,d,e], [b1,f,g], [c1,h,x], [d1,f1,h1], [e1,g1,x1]] ->
    a == a1 && b == b1 && c == c1 && d == d1 && e == e1 && f == f1 && g == g1 && h == h1 && x == x1)
  $ sequence
  $ replicate 6 triple

-- 4 * pyramid1 = 688,747,536 (≈ 5Gb of 64bit words)
fromBits :: [Int] -> Int
fromBits = foldl (\r x -> 2*r + x) 0


diff :: [Int] -> [Int]
diff xs = zipWith subtract xs $ tail xs
