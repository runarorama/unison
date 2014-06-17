module Unison.Syntax.Index where

-- how many scopes out to go to reach the lambda where this is bound
newtype Index = I Int deriving (Eq,Ord)

instance Show Index where
  show (I i) | i <= 0    = "t" ++ show (abs i)
  show (I i) | otherwise = "x" ++ show i

succ :: Index -> Index
succ (I i) = I (i + 1)

bound1 :: Index
bound1 = I 1
