pairs :: [a] -> [(a,a)]
pairs a = zip a (tail a)


sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

position :: Eq a => a -> [a] -> [Int]
position x xs = 
      [i | (x', i) <- zip xs [0..n],  x == x']
      where n = length xs - 1
