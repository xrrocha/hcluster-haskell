module StringDistance where

import Data.List

xjaro :: String -> String -> Double
xjaro s1 s2
 | 0 == m = 0
 | otherwise = (m / n1 + m / n2 + (m - t) / m) / 3
 where
  n1 = fromIntegral $ length s1
  n2 = fromIntegral $ length s2
  (used, m1) = matches s1 s2
  m2 = map (s2 !!) $ sort used
  m = fromIntegral $ length m1
  t = (/ 2) $ fromIntegral $ length $
      filter id $ zipWith (/=) m1 m2

matches :: String -> String -> ([Int], String)
matches s1 s2 = foldl snoc initst (zip s1 [0..]) where
 window = max 0 (max (length s1) (length s2) `div` 2 - 1)
 initst = ([], "")

 snoc st@(used, m1) (c1, idx1) =
   case find p [lo .. hi] of
     Nothing -> st
     Just idx2 -> (idx2 : used, m1 ++ [c1])
   where
     p idx2 = idx2 `notElem` used && c1 == s2 !! idx2
     lo = max 0 (idx1 - window)
     hi = min (length s2 - 1) (idx1 + window)

levenshtein sa sb = last $ foldl transform [0..length sa] sb
   where
      transform xs@(x:xs') c = scanl compute (x+1) (zip3 sa xs xs')
         where
            compute z (c', x, y) = minimum [y+1, z+1, x + fromEnum (c' /= c)]

-- Hello, i found error in your Jaro implementation that causes then metric sometimes are greater than 1.0
-- you should measure the distance as: abs (i-j) < (max (|s1|, |s2|) div2) - 1
-- in your case you do: abs (i-j) <= (max (|s1|, |s2|) div2)

-- Compute Jaro distance between two strings
jaro :: String -> String -> Double
jaro "" _ = 0.0
jaro _ "" = 0.0
jaro s1 s2
	| s1 == s2 = 1.0
	| s1 /= s2 =
		let
			-- Length of both strings
			l1 = length s1
			l2 = length s2
			-- Index s2
			z2 = zip [1..] s2
			m = foldl (++) [] [charMatch p ((max l1 l2) `div` 2 - 1) z2 | p <- zip [1..] s1]
			ml = length m
			t = sum [realToFrac (transposition p z2) / 2.0 | p <- m]
			ml1 = realToFrac ml / realToFrac l1
			ml2 = realToFrac ml / realToFrac l2
			mtm = (realToFrac ml - t) / realToFrac ml
		in
			(1 / 3) * (ml1 + ml2 + mtm)
		where
			-- [] of matching characters for 1 character
			charMatch (p,q) far list = filter (\(x,y) -> x > p - far && x < p + far && y == q) list
			-- # of transpositions for 1 character
			transposition (p,q) list = length $ filter (\(x,y) -> p /= x && q == y) list

-- Compute Winkler distance between two strings on top of Jaro distance
winkler :: String -> String -> Double -> Double
winkler "" _ _ = 0.0
winkler _ "" _ = 0.0
winkler s1 s2 jaro
	| s1 == s2 = 1.0
	| s1 /= s2 =
		let
			l = length $ commonPrefix s1 s2
			p = 0.1
		in
			jaro + ((realToFrac l * p) * (1.0 - jaro))

-- Compute Jaro-Winkler distance between two strings
jaroWinkler :: String -> String -> Double
jaroWinkler "" _ = 0.0
jaroWinkler _ "" = 0.0
jaroWinkler s1 s2
	| s1 == s2 = 1.0
	| s1 /= s2 = winkler s1 s2 $ jaro s1 s2


-- Common prefix of two strings; up to 4 characters
commonPrefix :: String -> String -> String
commonPrefix "" "" = ""
commonPrefix "" _ = ""
commonPrefix _ "" = ""
commonPrefix s1 s2
    | s1 == s2 = take 4 s1
    | otherwise = cp s1 s2 []
    where
        cp s1 s2 acc
            | length acc == 4 = acc
            | length s1 == 0 || length s2 == 0 = acc -- RRC
            | head s1 == head s2 = head s1 : cp (tail s1) (tail s2) acc
            | otherwise = acc
