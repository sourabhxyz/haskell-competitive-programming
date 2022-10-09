{-# OPTIONS_GHC -O2 #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Arrow         ((>>>))
import           Control.Monad
import           Control.Monad.State
import           Data.Array.IArray     as IA
import qualified Data.Array.IO         as MAIO
import qualified Data.Array.MArray     as MA
import           Data.Bits
import qualified Data.ByteString.Char8 as B
import           Data.Char
import qualified Data.IntMap.Strict    as IM
import qualified Data.Map              as M
import           Data.Maybe
import           System.IO

newtype Value = Value Int deriving Eq

data Node = Node { leftChild :: Node, rightChild :: Node, getValue :: Value } | Null deriving Eq

instance Semigroup Value where
  (Value v1) <> (Value v2) = Value (v1 + v2)

instance Monoid Value where
  mempty = Value 0

{-# INLINE getMiddle #-}
getMiddle start end = (start + end) `shiftR` 1

update atNode start end index val = do
  let atNode' = if atNode == Null then Node Null Null mempty else atNode
  if index < start || index > end then return atNode'
  else if start == end then return (Node Null Null (getValue atNode' <> val))
  else do
      let mid = getMiddle start end
      leftNode <- update (leftChild atNode') start mid index val
      rightNode <- update (rightChild atNode') (mid + 1) end index val
      return $ Node leftNode rightNode (getValue leftNode <> getValue rightNode)

query atNode start end rangeStart rangeEnd = do
  if atNode == Null || start > rangeEnd || end < rangeStart then return mempty
  else if rangeStart <= start && end <= rangeEnd then return $ getValue atNode
  else do
    let mid = getMiddle start end
    lq <- query (leftChild atNode) start mid rangeStart rangeEnd
    rq <- query (rightChild atNode) (mid + 1) end rangeStart rangeEnd
    return (lq <> rq)

{-# INLINE readInt #-}
readInt :: B.ByteString -> Int
readInt = B.readInt >>> fromJust >>> fst

{-# INLINE readInts #-}
readInts :: IO B.ByteString -> IO [Int]
readInts ioB = fmap readInt <$> (B.words <$> ioB)

{-# INLINE printString #-}
printString = B.pack >>> B.putStr

{-# INLINE printStringLn #-}
printStringLn = B.pack >>> B.putStrLn

maxPr :: Int
maxPr = 1_000_000_000

main :: IO ()
main = do
  q <-  readInt <$> B.getLine
  dayIdToPrMap <- MAIO.newArray (0, q) M.empty :: IO (MAIO.IOArray Int (M.Map Int Int))
  dayToSegTree <- MAIO.newArray (0, q) Null :: IO (MAIO.IOArray Int Node)
  let
    solveQueries (currentDay, stringToIdMap)
      | currentDay == (q + 1) = return ()
      | otherwise = do
          let
            -- need to check size logic
            strToId str = do
              if M.member str stringToIdMap then return (stringToIdMap M.! str, stringToIdMap)
              else do
                let newId = M.size stringToIdMap
                    updatedMap = M.insert str newId stringToIdMap
                return (newId, updatedMap)

          currentQ <- B.words <$> B.getLine

          case currentQ of
            ["set", as, x] -> do
              let x' = readInt x
              (id, updatedMap) <- strToId as
              -- printStringLn $ "Inside set, got id: " ++ show id
              prevDayIdToPrMap <- MAIO.readArray dayIdToPrMap (currentDay - 1)
              MAIO.writeArray dayIdToPrMap currentDay (M.insert id x' prevDayIdToPrMap)
              if M.member id prevDayIdToPrMap then do
                let previousPr = prevDayIdToPrMap M.! id
                prevDaySegTree <- MAIO.readArray dayToSegTree (currentDay - 1)
                updatedSegTree <- update prevDaySegTree 0 maxPr previousPr (Value (-1))
                updatedSegTree' <- update updatedSegTree 0 maxPr x' (Value 1)
                MAIO.writeArray dayToSegTree currentDay updatedSegTree'
              else do
                -- printStringLn $ "Hey, inside set, id not present before"
                prevDaySegTree <- MAIO.readArray dayToSegTree (currentDay - 1)
                updatedSegTree <- update prevDaySegTree 0 maxPr x' (Value 1)
                MAIO.writeArray dayToSegTree currentDay updatedSegTree

              solveQueries (currentDay + 1, updatedMap)
            ["remove", as] -> do
              (id, updatedMap) <- strToId as
              prevDayIdToPrMap <- MAIO.readArray dayIdToPrMap (currentDay - 1)
              MAIO.writeArray dayIdToPrMap currentDay (M.delete id prevDayIdToPrMap)
              if M.member id prevDayIdToPrMap then do
                let previousPr = prevDayIdToPrMap M.! id
                prevDaySegTree <- MAIO.readArray dayToSegTree (currentDay - 1)
                updatedSegTree <- update prevDaySegTree 0 maxPr previousPr (Value (-1))
                MAIO.writeArray dayToSegTree currentDay updatedSegTree
              else do
                prevDaySegTree <- MAIO.readArray dayToSegTree (currentDay - 1)
                MAIO.writeArray dayToSegTree currentDay prevDaySegTree

              solveQueries (currentDay + 1, updatedMap)
            ["undo", d] -> do
              let d' = readInt d

              prevIdToPrMap <- MAIO.readArray dayIdToPrMap (currentDay - 1 - d')
              MAIO.writeArray dayIdToPrMap currentDay prevIdToPrMap

              prevSegTree <- MAIO.readArray dayToSegTree (currentDay - 1 - d')
              MAIO.writeArray dayToSegTree currentDay prevSegTree
              solveQueries (currentDay + 1, stringToIdMap)
            ["query", as] -> do
              (id, updatedMap) <- strToId as
              prevDayIdToPrMap <- MAIO.readArray dayIdToPrMap (currentDay - 1)
              MAIO.writeArray dayIdToPrMap currentDay prevDayIdToPrMap
              prevDaySegTree <- MAIO.readArray dayToSegTree (currentDay - 1)
              MAIO.writeArray dayToSegTree currentDay prevDaySegTree
              if M.member id prevDayIdToPrMap then do
                let pr = prevDayIdToPrMap M.! id
                Value ans <- query prevDaySegTree 0 maxPr 0 (pr - 1)
                printStringLn $ show ans
              else printStringLn "-1"
              hFlush stdout
              solveQueries (currentDay + 1, updatedMap)

  solveQueries (1, M.empty :: M.Map B.ByteString Int)
