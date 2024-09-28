

-- build n-group
-- show removed records
-- replace n-group with O(n²) quads
--
-- check joined quads


-- build pyramid
-- show remaining/removed records

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


-- 4 * pyramid1 = 688,747,536 (≈ 5Gb of 64bit words)
fromBits :: [Int] -> Int
fromBits = foldl (\r x -> 2*r + x) 0


diff :: [Int] -> [Int]
diff xs = zipWith subtract xs $ tail xs
