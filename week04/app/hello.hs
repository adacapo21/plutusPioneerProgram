-- public static int foo() {
-- ...
-- }
--
-- ...
--
-- ... foo() ... foo()

-- foo :: Int
-- foo = ...

-- let x = foo in ... x ... x ...
-- ... foo ... foo ...

-- foo :: IO Int
-- foo = ...

main :: IO ()
main = bar -- putStrLn "Hello, world!"

bar :: IO ()
bar = getLine >>= \s -> -- bind first action
      getLine >>= \t -> -- bind second action
      putStrLn (s ++ t) -- concatenation of both
