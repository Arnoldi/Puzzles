neighboradd :: Num a => [a] -> [a]
neighboradd l = zipWith (+) l (tail l)

grid_paths :: Integral a => a -> a -> a
grid_paths x y = grid_paths_ x y [1,1]
    where
        grid_paths_ :: Integral a => a -> a -> [a] -> a
        grid_paths_ 1 1 l
            | length l > 1 = grid_paths_ 1 1 (neighboradd l)
            | otherwise = head l
        grid_paths_ x 1 l = grid_paths_ (x-1) 1 ([1] ++ (neighboradd l))
        grid_paths_ 1 y l = grid_paths_ 1 (y-1) ((neighboradd l) ++ [1])
        grid_paths_ x y l = grid_paths_ (x-1) (y-1) ([1] ++ (neighboradd l) ++ [1])

main = putStrLn . show $ grid_paths 20 20
