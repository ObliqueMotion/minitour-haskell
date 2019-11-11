module Combinator
    ( module Combinator
    ) where

import Control.Applicative

newtype Combinator input output = C (input -> [(output, input)])

instance Functor (Combinator input) where
        -- fmap :: (a -> b) -> Combinator a -> Combinator b
        fmap f c = do x <- c
                      return (f x)

instance Applicative (Combinator input) where
        -- pure :: a -> Combinator a
        pure x = C (\inp -> [(x, inp)])

        -- <*> :: Combinator (a -> b) -> Combinator a -> Combinator b
        cf <*> cx = do f <- cf
                       x <- cx
                       return (f x)

instance Monad (Combinator input) where
        -- (>>=) :: Combinator a -> (a -> Combinator b) -> Combinator b
        cx >>= f = C (\inp -> case combine cx inp of
                                   []         -> []
                                   [(x, out)] -> combine (f x) out)


instance Alternative (Combinator input) where
     -- empty :: Combinator a
     empty = C (\_ -> [])

     -- (<|>) :: Combinator a -> Combinator a -> Combinator a
     px <|> py = C (\inp -> case combine px inp of 
                                 []  -> combine py inp
                                 [(y, out)] -> [(y, out)])
                                    
combine :: Combinator input output -> (input -> [(output, input)])
combine (C c) inp = c inp

