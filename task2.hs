import Data.List

single :: Int -> Int -> (Int,Int) -> [(Int,Int)]
single n k (px,py) = do (mx,my) <- [(2,-1),(2,1),(1,2),(-1,2),(-2,1),(-2,-1),(-1,-2),(1,-2)]
                        if px+mx>=1 && px+mx<=n && py+my >=1 && py+my<=k then return (px+mx,py+my) else fail ""

moves :: (Int,Int) -> Int -> Int -> Int -> [(Int,Int)]
moves pos steps n k = aux [pos] steps n k where
    aux list 0 n k = list
    aux list steps n k = aux (nub (list >>= (single n k))) (steps - 1) n k

