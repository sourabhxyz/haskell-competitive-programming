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


{-# INLINE modulo #-}
modulo :: Int
modulo = 998244353

{-# INLINE addModulo #-}
addModulo :: Int -> Int -> Int
addModulo a b =
  let res = (a + b)
  in if res < modulo then res else res - modulo

{-# INLINE mulModulo #-}
mulModulo :: Int -> Int -> Int
mulModulo a b = (a * b) `mod` modulo

data Node = Node { getValue :: Int, getLength :: Int } deriving Show

derivingUnbox "Node"
  [t|Node -> (Int, Int)|]
  [|\(Node v l) -> (v, l)|]
  [| uncurry Node|]

instance Semigroup Node where
  (Node v1 l1) <> (Node v2 l2) = Node (addModulo v1 v2) (l1 + l2)

instance Monoid Node where
  mempty = Node 0 0

data UpdateParam = UpdateParam { getB :: Int, getC :: Int } deriving (Eq, Show)

derivingUnbox "UpdateParam"
  [t|UpdateParam -> (Int, Int)|]
  [|\(UpdateParam b c) -> (b, c)|]
  [| uncurry UpdateParam|]

{-# INLINE emptyParam #-}
emptyParam = UpdateParam 1 0

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
    VUM.write segTree at (Node (as VU.! start) 1)
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
  UpdateParam b c <- VUM.read lazyTree at
  Node v l <- VUM.read segTree at
  VUM.write segTree at $ Node (addModulo (mulModulo v b) (mulModulo l c)) l
  VUM.write lazyTree at emptyParam
  when (start /= end) $ do
     let leftIndex = left at
         rightIndex = right at
         compose (UpdateParam prevB prevC) = UpdateParam (mulModulo b prevB) (addModulo (mulModulo b prevC) c)
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
      [0, l, r, b, c] -> update 1 0 (n - 1) segTree l (r - 1) lazyTree (UpdateParam b c)
      [1, l, r] -> do
        Node result _ <- query 1 0 (n - 1) l (r - 1) segTree lazyTree
        printString $ show result ++ "\n"

