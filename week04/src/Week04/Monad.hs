module Week04.Monad where

-- (>>=)      :: IO a            -> (a -> IO b)            -> IO b
-- bindMaybe  :: Maybe a         -> (a -> Maybe b)         -> Maybe b
-- bindEither :: Either String a -> (a -> Either String b) -> Either String b
-- bindWriter :: Writer a        -> (a -> Writer b)        -> Writer b
--
-- return              :: a -> IO a
-- Just                :: a -> Maybe a
-- Right               :: a -> Either String a
-- (\a -> Writer a []) :: a -> Writer a

threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts mx my mz =
    mx >>= \k ->
    my >>= \l ->
    mz >>= \m ->
    let s = k + l + m in return s

-- the above in Writer.hs
-- foo'' :: String -> String -> String -> Maybe Int
-- foo'' x y z = threeInts (readMaybe x) (readMaybe y) (readMaybe z)
-- and with the log message similarly:
-- foo'' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
-- foo'' x y z = do
--    s <- threeInts x y z
--    tell ["sum: " ++ show s]
--    return

threeInts' :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts' mx my mz = do
    k <- mx
    l <- my
    m <- mz
    let s = k + l + m           -- let statement does not use an in part.
                                -- It does not need to inside a do block.
    return s
