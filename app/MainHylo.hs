import Data.Functor.Foldable

main :: IO ()
main = putStrLn $ show $ hylosum 1000000

hylosum :: Int -> Int
hylosum end = hylo alg coalg 1
  where 
    coalg :: Int -> ListF Int Int
    coalg m 
       | m > end = Nil
       | otherwise = Cons m (m + 1)
    alg :: ListF Int Int -> Int
    alg Nil  =  0
    alg (Cons a x) = a + x
    