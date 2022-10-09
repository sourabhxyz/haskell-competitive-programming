{-# OPTIONS_GHC -O2 #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Arrow                ((>>>))
import           Control.Monad                (replicateM_, void)
import           Data.Bits
import qualified Data.ByteString.Char8        as B
import           Data.Char
import           Data.IORef
import           Data.Maybe
import qualified Data.Vector.Unboxed          as VU
import           Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed.Mutable  as VUM

newtype Node = Node { getValue :: Int }

derivingUnbox "Node"
  [t|Node -> Int|]
  [|\(Node v) -> v|]
  [|Node|]

instance Semigroup Node where
  Node a <> Node b = Node $ max a b

instance Monoid Node where
  mempty = Node (-1)

{-# INLINE readInt #-}
readInt :: B.ByteString -> Int
readInt = B.readInt >>> fromJust >>> fst

{-# INLINE printString #-}
printString = B.pack >>> B.putStr

{-# INLINE left #-}
left at = at + at

{-# INLINE right #-}
right at = (at + at) .|. 1

{-# INLINE getMiddle #-}
getMiddle start end = (start + end) `shiftR` 1

build at start end segTree as =
  if start == end then do
    VUM.write segTree at (Node $ as VU.! start)
  else do
    let mid = getMiddle start end
        leftIndex = left at
        rightIndex = right at
    void $ build leftIndex start mid segTree as
    void $ build rightIndex (mid + 1) end segTree as
    leftNode <- VUM.read segTree leftIndex
    rightNode <- VUM.read segTree rightIndex
    VUM.write segTree at (leftNode <> rightNode)

update at start end segTree index val
  | index < start || index > end = return ()
  | start == end = do
    VUM.write segTree at $ Node val
  | otherwise = do
      let mid = getMiddle start end
          leftIndex = left at
          rightIndex = right at
      update leftIndex start mid segTree index val
      update rightIndex (mid + 1) end segTree index val
      leftNode <- VUM.read segTree leftIndex
      rightNode <- VUM.read segTree rightIndex
      VUM.write segTree at (leftNode <> rightNode)

query at start end rangeStart rangeEnd segTree
  | start > rangeEnd|| end < rangeStart = return mempty
  | rangeStart <= start && end <= rangeEnd = VUM.read segTree at
  | otherwise = do
      leftNode <- query leftIndex start mid rangeStart rangeEnd segTree
      rightNode <- query rightIndex (mid + 1) end rangeStart rangeEnd segTree
      return $ leftNode <> rightNode
  where
    mid = getMiddle start end
    leftIndex = left at
    rightIndex = right at

getFirst at start end rangeStart rangeEnd segTree v
  | start > rangeEnd || end < rangeStart = return mempty
  | rangeStart <= start && end <= rangeEnd = do
      Node atValue <- VUM.read segTree at
      if atValue < v then return mempty
      else
        let
          binarySearch start' end' at'
            | start' /= end' = do
                let mid = getMiddle start' end'
                    leftIndex = left at'
                    rightIndex = right at'
                Node leftNode <- VUM.read segTree leftIndex
                if leftNode >= v then
                  binarySearch start' mid leftIndex
                else
                  binarySearch (mid + 1) end' rightIndex
            | otherwise = return $ Node start'
        in binarySearch start end at
  | otherwise = do
      let mid = getMiddle start end
      Node result <- getFirst (left at) start mid rangeStart rangeEnd segTree v
      if result /= (-1) then return $ Node result
      else getFirst (right at) (mid + 1) end rangeStart rangeEnd segTree v

main :: IO ()
main = do
  [n, q] <- fmap readInt <$> (B.words <$> B.getLine)
  as <- VU.unfoldr (B.dropWhile isSpace >>> B.readInt) <$> B.getLine  -- their setup doesn't have `dropSpace` variant.
  let powTwoGe n current
        | current >= n = current
        | otherwise = powTwoGe n $ current `shiftL` 1
  segTree <- VUM.new $ powTwoGe n 1 `shiftL` 1
  build 1 0 (n - 1) segTree as
  replicateM_ q $ do
    [t, a, b] <- fmap readInt <$> (B.words <$> B.getLine)
    case t of
      1 -> update 1 0 (n - 1) segTree (a - 1) b
      2 -> do
       Node result <- query 1 0 (n - 1) (a - 1) (b - 1) segTree
       printString $ show result ++ "\n"
      3 -> do
        Node result <- getFirst 1 0 (n - 1) (a - 1) (n - 1) segTree b
        if result == (-1) then printString $ show (n + 1) ++ "\n"
        else printString $ show (result + 1) ++ "\n"
      _ -> error "Invalide query type!"

