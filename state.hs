import Control.Monad.State

test :: StateT Int IO Int
test = do
    lift (putStrLn "Inside state.")
    a <- get
    put (a + 1)
    return 1

main :: IO ()
main = do
    (v, s) <- runStateT test 0
    putStrLn (show s)
    return ()
