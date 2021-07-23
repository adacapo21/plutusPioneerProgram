module Week04.Writer where

import Control.Monad
import Week04.Monad

data Writer a = Writer a [String]
    deriving Show

number :: Int -> Writer Int
number n = Writer n $ ["number: " ++ show n]

tell :: [String] -> Writer ()
tell = Writer ()

foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int -- function that takes a list of message
foo (Writer k xs) (Writer l ys) (Writer m zs) =
  let
    s = k + l + m
    Writer _ us = tell ["sum: " ++ show s]                  -- log message showing the sum of the numbers
  in
    Writer s $ xs ++ ys ++ zs ++ us                         -- return a single computation that produces the sum of those Ints

-- SECOND IMPLEMENTATION
bindWriter :: Writer a -> (a -> Writer b) -> Writer b       -- bindWriter function is returning the Writer b and producing log messages
                                                            -- which are a concatenation of the xs that we pattern matched on input,
                                                            -- and the ys that we pattern matched when calling f a in order to produce the Writer b.
bindWriter (Writer a xs) f =
  let
    Writer b ys = f a
  in
    Writer b $ xs ++ ys                                     -- RESULT

foo' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo' x y z = x `bindWriter` \k ->
             y `bindWriter` \l ->
             z `bindWriter` \m ->
             let s = k + l + m
             in tell ["sum: " ++ show s] `bindWriter` \_ ->
                Writer s []

-- THIRD IMPLEMENTATION -- will be used to Monad.hs
foo'' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo'' x y z = do
    s <- threeInts x y z
    tell ["sum: " ++ show s]
    return s

instance Functor Writer where
    fmap = liftM

instance Applicative Writer where
    pure = return
    (<*>) = ap

instance Monad Writer where
    return a = Writer a []
    (>>=) = bindWriter
