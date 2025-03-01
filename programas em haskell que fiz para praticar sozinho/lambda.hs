-- Define the lambda functions separately (optional, for clarity)
lambda1 :: Num a => a -> a -> a
lambda1 = \x y -> x * x + y * y

lambda2 :: Num a => a -> a -> a
lambda2 = \c d -> c * c + d * d

-- Define the 'soma' function to use these lambda functions
soma :: Num a => a -> a -> a -> a -> a
soma a b c d = lambda1 a b + lambda2 c d
