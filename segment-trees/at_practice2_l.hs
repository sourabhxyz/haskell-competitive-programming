{-# OPTIONS_GHC -O2 #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Arrow                ((>>>))
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString.Char8        as B
import           Data.Char
import           Data.IORef
import           Data.Maybe
import qualified Data.Vector.Unboxed          as VU
import           Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed.Mutable  as VUM


data Node = Node { getOnes :: Int, getZeroes :: Int, getInv :: Int } deriving Show

derivingUnbox "Node"
  [t|Node -> (Int, Int, Int)|]
  [|\(Node ones zeroes invs) -> (ones, zeroes, invs)|]
  [| \(o, z, i) -> Node o z i|]

instance Semigroup Node where
  (Node ones zeroes invs) <> (Node ones' zeroes' invs') = Node (ones + ones') (zeroes + zeroes') (invs + invs' + zeroes * ones')

instance Monoid Node where
  mempty = Node 0 0 0

newtype UpdateParam = UpdateParam { toInvert :: Bool } deriving (Eq, Show)

derivingUnbox "UpdateParam"
  [t|UpdateParam -> Bool|]
  [|\(UpdateParam toI) -> toI|]
  [| UpdateParam|]

{-# INLINE emptyParam #-}
emptyParam = UpdateParam False

{-# INLINE readInt #-}
readInt :: B.ByteString -> Int
readInt = B.readInt >>> fromJust >>> fst

{-# INLINE readInts #-}
readInts :: IO B.ByteString -> IO [Int]
readInts ioB = fmap readInt <$> (B.words <$> ioB)

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
    VUM.write segTree at (if as VU.! start == 0 then Node 0 1 0 else Node 1 0 0)
  else do
    let mid = getMiddle start end
        leftIndex = left at
        rightIndex = right at
    void $ build leftIndex start mid segTree as
    void $ build rightIndex (mid + 1) end segTree as
    leftNode <- VUM.read segTree leftIndex
    rightNode <- VUM.read segTree rightIndex
    VUM.write segTree at (leftNode <> rightNode)

pushDown at start end segTree lazyTree = do
  lazyAt <- VUM.read lazyTree at
  Node o z i <- VUM.read segTree at
  VUM.write segTree at $ if toInvert lazyAt then Node z o (o * z - i) else Node o z i
  VUM.write lazyTree at emptyParam
  when (start /= end) $ do
     let leftIndex = left at
         rightIndex = right at
         compose updateParam = UpdateParam $ not $ toInvert updateParam
     leftUpdateParam <- VUM.read lazyTree leftIndex
     rightUpdateParam <- VUM.read lazyTree rightIndex
     VUM.write lazyTree leftIndex $ compose leftUpdateParam
     VUM.write lazyTree rightIndex $ compose rightUpdateParam

update at start end segTree rangeStart rangeEnd lazyTree updateParam = do
    lazyAt <- VUM.read lazyTree at
    -- We must first do this lazy check before checking no intersection case, as we need to update the parent, which relies on this child's latest value.
    when (lazyAt /= emptyParam) $ pushDown at start end segTree lazyTree
    if start > rangeEnd || end < rangeStart then return ()
    else do
      if rangeStart <= start &&  end <= rangeEnd then do
        VUM.write lazyTree at updateParam
        pushDown at start end segTree lazyTree
      else do
        let mid = getMiddle start end
            leftIndex = left at
            rightIndex = right at
        update leftIndex start mid segTree rangeStart rangeEnd lazyTree updateParam
        update rightIndex (mid + 1) end segTree rangeStart rangeEnd lazyTree updateParam
        leftNode <- VUM.read segTree leftIndex
        rightNode <- VUM.read segTree rightIndex
        VUM.write segTree at (leftNode <> rightNode)

query at start end rangeStart rangeEnd segTree lazyTree
  | start > rangeEnd|| end < rangeStart = return mempty
  | otherwise = do
      lazyAt <- VUM.read lazyTree at
      when (lazyAt /= emptyParam) $ pushDown at start end segTree lazyTree
      if rangeStart <= start && end <= rangeEnd then VUM.read segTree at
      else do
        let mid = getMiddle start end
        leftNode <- query (left at) start mid rangeStart rangeEnd segTree lazyTree
        rightNode <- query (right at) (mid + 1) end rangeStart rangeEnd segTree lazyTree
        return $ leftNode <> rightNode

main :: IO ()
main = do
  [n, q] <- readInts B.getLine
  as <- VU.unfoldr (B.dropWhile isSpace >>> B.readInt) <$> B.getLine
  let powTwoGe n current
        | current >= n = current
        | otherwise = powTwoGe n $ current `shiftL` 1
      segTreeSize = powTwoGe n 1 `shiftL` 1
  segTree <- VUM.new segTreeSize
  build 1 0 (n - 1) segTree as
  lazyTree <- VUM.replicate segTreeSize emptyParam
  replicateM_ q $ do
    qs <- readInts B.getLine
    case qs of
      [1, l, r] -> update 1 0 (n - 1) segTree (l - 1) (r - 1) lazyTree (UpdateParam True)
      [2, l, r] -> do
        Node o z i <- query 1 0 (n - 1) (l - 1) (r - 1) segTree lazyTree
        printString $ show (o * z - i) ++ "\n"

