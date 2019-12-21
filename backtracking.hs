import UtilGen

----------------------------------------------------
--------------- GENERIC BACKTRACKING ---------------
----------------------------------------------------

-- backtrack a solution on xs
-- isDoneF: function that says xs is a valid solution
-- getChildF: function that gives the child of (last xs)
-- hasBroF: True if (last xs) has a brother
-- getBroF: function that gives the brother of (last xs)
-- looksGoodF: True if (last xs) is valid (consistent with (init xs)) and it has at least a possible childs
-- lookForBro: if True backTrack looks for a solution substituting (last xs) with its brother
backTrack :: ([a] -> Bool) -> ([a] -> a) -> ([a] -> Bool) -> ([a] -> a) -> ([a] -> Bool) -> Bool -> [a] -> [a]
backTrack isDoneF getChildF hasBroF getBroF looksGoodF True [] = []
backTrack isDoneF getChildF hasBroF getBroF looksGoodF lookForBro xs
  -- it can be written also as follow:
  -- | lookForBro = backTrack isDoneF getChildF hasBroF getBroF looksGoodF (not (hasBroF xs)) ((init xs) ++ (if hasBroF xs then [getBroF xs] else []))
  -- but it's more readable like this:
  | lookForBro && hasBroF xs = backTrack isDoneF getChildF hasBroF getBroF looksGoodF False ((init xs) ++ [getBroF xs])
  | lookForBro && not (hasBroF xs) = backTrack isDoneF getChildF hasBroF getBroF looksGoodF True (init xs)
  | isDoneF xs = xs
  | looksGoodF xs = backTrack isDoneF getChildF hasBroF getBroF looksGoodF False (xs ++ [getChildF xs])
  | otherwise = backTrack isDoneF getChildF hasBroF getBroF looksGoodF True xs

-- backtrack all solutions
backTrackAll isDoneF getChildF hasBroF getBroF looksGoodF = btAll0 [] (backTrack isDoneF getChildF hasBroF getBroF looksGoodF False [])
  where btAll0 xss [] = xss
        btAll0 xss xs = btAll0 (xss ++ [xs]) (backTrack isDoneF getChildF hasBroF getBroF looksGoodF True xs)
  
---------------------------------------------------------------------------------------------
--- N-Queens problem ... as a test! - https://en.wikipedia.org/wiki/Eight_queens_puzzle
---------------------------------------------------------------------------------------------
queensN1 n = backTrack (qIsDone n) qGetChild (qHasBro n) qGetBro (qLooksGood n) False []
queensNAll n = backTrackAll (qIsDone n) qGetChild (qHasBro n) qGetBro (qLooksGood n)

qIsDone _ [] = False
qIsDone n xs = length xs == n && qLooksGood n xs

qGetChild _ = 1

qHasBro _ [] = True
qHasBro n xs = (last xs) < n

qGetBro xs = (last xs) + 1

qLooksGood n [] = True
qLooksGood n xs = vertSafe xs && diagSafe diag1 xs && diagSafe diag2 xs
  where vertSafe ys = all ((last ys)/=) (init ys)
        diag1 r c = c - r -- diagonal from left to right
        diag2 r c = (n - c + 1) - r -- diagonal from right to left
        diagSafe dF ys = all ((dF (length ys) (last ys))/=) (mapIdx (\i c -> dF (i + 1) c) (init ys))

