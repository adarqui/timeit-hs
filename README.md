**timeit-simple-hs**

Simple library to time the execution of a function or IO action.

Modeled after timeit-go and the separate timeit hackage library.

See examples/timeit-hs.hs


```
import System.TimeitHs
import Control.Concurrent
import Data.List

nothingFunc :: IO ()
nothingFunc = return ()

loopFunc :: IO Integer
loopFunc = do
    -- break that lazyness
    let v = foldl' (+) 0 [1..100000000]
    print v
    return v

sleepFunc :: IO ()
sleepFunc = do
    threadDelay $ 5 * 1000000

main :: IO ()
main = do
    timeitPrint nothingFunc
    timeitPrint loopFunc
    timeitPrint sleepFunc
    putStrLn $ show $ timeitP nothingFunc
    putStrLn $ show $ timeitP loopFunc
    putStrLn $ show $ timeitP sleepFunc
```
