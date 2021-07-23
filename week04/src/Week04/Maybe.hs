module Week04.Maybe where

import Text.Read (readMaybe)
import Week04.Monad

foo :: String -> String -> String -> Maybe Int  -- YOU CAN RUN THIS IN REPL by typing:
                                                -- foo "1" "2" "3"
foo x y z = case readMaybe x of                 -- 3 values to be a type of Integers
    Nothing -> Nothing                          -- FAILURE
    Just k  -> case readMaybe y of              -- If K int succeeds
        Nothing -> Nothing                      -- FAILURE
        Just l  -> case readMaybe z of          -- If l int succeeds
            Nothing -> Nothing                  -- FAILURE
            Just m  -> Just (k + l + m)         -- if m int succeeds
                                                -- the above way is too Noisy in HASKELL
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b -- a less NOISY WAY of of doing the completely same logic with above
bindMaybe Nothing  _ = Nothing
bindMaybe (Just x) f = f x
                                                -- YOU CAN RUN below example IN REPL by typing:
                                                -- foo' "1" "2" "3"
foo' :: String -> String -> String -> Maybe Int
foo' x y z = readMaybe x `bindMaybe` \k ->       -- If k is an INT succeeds
             readMaybe y `bindMaybe` \l ->       -- If l is an INT succeeds
             readMaybe z `bindMaybe` \m ->       -- If m is an INT succeeds
             Just (k + l + m)                    -- ADD THEM

foo'' :: String -> String -> String -> Maybe Int                    -- YOU CAN RUN THIS IN REPL by typing:
                                                                    -- foo'' "1" "2" "3"
foo'' x y z = threeInts (readMaybe x) (readMaybe y) (readMaybe z)   --
