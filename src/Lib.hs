{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( cpuUsage
  , memUsage
  ) where

import           Control.Concurrent     (threadDelay)
import           Control.Exception.Base
import           Control.Monad          (liftM)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as C
import           Data.List
import qualified Data.Map.Strict        as Map
import           Numeric                (showFFloat)

statFile :: FilePath
statFile = "/proc/stat"

memFile :: FilePath
memFile = "/proc/meminfo"

status :: [String]
status =
  [ "user"
  , "nice"
  , "system"
  , "idle"
  , "iowait"
  , "irq"
  , "softirq"
  , "steal"
  , "guest"
  , "guest_nice"
  ]

meminfos :: [String]
meminfos =
  [ "MemTotal"
  , "MemFree"
  , "Buffers"
  , "Cached"
  , "SReclaimable"
  , "Shmem"
  , "SwapTotal"
  , "SwapFree"
  ]

splitString :: Char -> String -> [String]
splitString _ [] = []
splitString c s =
  let (item, rest) = break (== c) s
      (_, next) = break (/= c) rest
  in item : splitString c next

computeIdle :: String -> (Double, Double)
computeIdle s = (idle, nonidle)
  where
    xs = map (fromInteger . read) $ tail $ splitString ' ' s
    infos = assert (length xs == length status) (Map.fromList $ zip status xs)
    sums xs' = sum $ (infos Map.!) <$> xs'
    idle = sums ["idle", "iowait"]
    nonidle = sums ["user", "nice", "system", "irq", "softirq", "steal"]

computeIdleM :: IO (Double, Double)
computeIdleM = do
  s <- (head . lines . C.unpack) <$> B.readFile statFile
  return $ computeIdle s

cpuUsage :: Int -> Int -> IO String
cpuUsage delay numOfDecimals = do
  (idle, nonIdle) <- computeIdleM
  threadDelay delay
  (idle', nonIdle') <- computeIdleM
  let totald = idle' + nonIdle' - idle - nonIdle
  let idled = idle' - idle
  let usage = showFFloat (Just numOfDecimals) ((totald - idled) / totald * 100) "%"
  return usage

getMemInfo :: [String] -> [(String, Double)]
getMemInfo filelines = (\i -> (i, getvalByKey i)) <$> meminfos
  where
    getvalByKey key =
        (\ys -> assert (ys!!2 == "kB") (read (ys!!1) :: Double))
      . (splitString ' ')
      . head
      . (filter $ isInfixOf key)
      $ filelines

getMemInfoM :: IO (Map.Map String Double)
getMemInfoM = liftM (Map.fromList . getMemInfo . lines . C.unpack)
  $ B.readFile memFile

memUsage :: IO String
memUsage = getMemInfoM >>= \info ->
  let total = info Map.! "MemTotal"
      free = info Map.! "MemFree"
      buffer = info Map.! "Buffers"
      cache = info Map.! "Cached" + info Map.! "SReclaimable" - info Map.! "Shmem"
      nonbuffer = total - free - buffer - cache
      m2k k = show $ round $ k / 1024
   in return (m2k nonbuffer ++ "/" ++ m2k total ++ "MB")
