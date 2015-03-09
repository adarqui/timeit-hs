module System.TimeitHs (
    TimeSpent (..),
    timeit,
    timeitT,
    timeitP,
    timeitPrint
) where

-- used https://hackage.haskell.org/package/timeit as a template.

import Criterion.Measurement
import System.IO.Unsafe
import Data.UnixTime
import GHC.Word

data TimeSpent = TimeSpent {
    elapsed :: UnixDiffTime,
    cpu :: Double,
    ticks :: Word64
} deriving (Show)

-- |Wrap an 'IO' computation so that it returns a IO string (TimeSpent record)
timeit :: IO a -> IO String
timeit ioa = do
    (ts, _) <- timeitT ioa
    return $ show ts

-- |Wrap an 'IO' computation so that it returns a IO (TimeSpent record, value)
timeitT :: IO a -> IO (TimeSpent, a)
timeitT ioa = do
    u1 <- getUnixTime
    ticks1 <- getCycles
    t1 <- getCPUTime
    a <- ioa
    t2 <- getCPUTime
    ticks2 <- getCycles
    u2 <- getUnixTime
    let ts = TimeSpent { elapsed = (diffUnixTime u2 u1), cpu = (t2 - t1), ticks = (ticks2 - ticks1) }
    return (ts, a)

-- |Wrap an 'IO' computation so that it prints the TimeSpent structure
timeitPrint :: IO a -> IO ()
timeitPrint ioa = do
    v <- timeit ioa
    putStrLn v

-- |Wrap an 'IO' computation so that it returns a TimeSpent record
timeitP :: IO a -> (TimeSpent, a)
timeitP ioa = unsafePerformIO (timeitT ioa)
