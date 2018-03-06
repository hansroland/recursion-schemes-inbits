module AnaListSplit where 

import Data.Functor.Foldable

-- | Split up a string by a separation predicate
splitBy :: (Char -> Bool) -> String -> [String] 
splitBy p = ana splitF 
  where 
    splitF :: String -> ListF String String 
    splitF [] = Nil 
    splitF str =           
        let (line,rest) = break p str 
        in  Cons line (drop 1 rest)

-- drop 1 because  break (== ',') "abc,def"  returns  ("abc",",def") 

-- >>> splitBy  (== ',') "abc, def, ghi k"
-- ["abc"," def"," ghi k"]