# Segment Trees

## About

Segment tree is a binary tree, useful in solving interval queries over an array.

Suppose we are given an array of length $N = 2 ^ k$ (indexed $1$ to $2 ^ k$) where $k$ is a natural number. And we are interested in say, finding maximum array element in some arbitrary range $(i, j)$ where $1 \le i \le j \le 2 ^ k$. Naive implementation would be to query over each element in the range, giving worst case complexity of $O(N)$, segment tree's help in making it $O(\log N)$.

I will, in particular, explain this structure by considering [this](https://atcoder.jp/contests/practice2/tasks/practice2_j) problem.

## Representation

Each node in the segment tree represents a range (of array) as shown in the below diagram (diagram starts from the root).

```

                               1 Node, representing interval of length 2 ^ k
                 ┌────────────┐
                 │  Interval  │
                 │ 1 .. 2 ^ k │
          ┌───── └────────────┘ ───┐
          │        segTree[1]      │
          ▼                        ▼     2 Nodes, representing intervals of length 2 ^ (k - 1)
┌──────────────────┐     ┌──────────────────────────┐
│     Interval     │     │         Interval         │
│ 1 .. 2 ^ (k - 1) │     │ 2 ^ (k - 1) + 1 .. 2 ^ k │
└──────────────────┘     └──────────────────────────┘
     segTree[2]                segTree[3]
     /    \                     /   \
    /      \          . . .    /     \     4 Nodes, representing intervals of length 2 ^ (k - 2)
segTree[4] segTree[5]   segTree[6]  segTree[7]
                        .
                        .
                        .

            Lastly, 2 ^ k nodes each of length 1
```

This diagram implies that number of nodes in this tree is: $\sum_{i = 0}^k 2 ^ i$ $= 2 ^ {k + 1} - 1$ $= 2N - 1$.

We can store this segment tree in an array (say, `segTree`) where we first store root, then its two children from left to right, then their children in left to right order and so on, like in the diagram above.

Given an index, say `i` of this `segTree` array, we observe that index of its left child and right child are respectively, `2 * i` & `2 * i + 1`. Which by using bit operations (from [`Data.Bits`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Bits.html)) can be written as ``i `shiftL` 1`` & ``(i `shiftL` 1) .|. 1``. _Note_: Generally, bits operations are computational cheaper.

Storing segment tree as an array is more memory efficient than constructing a datatype where each node would have a pointer to its left & right child because in our array representation, we don't require space for storing pointer. 

## Construction

Let us consider a non leaf node. Suppose we have an answer for maximum value for an interval represented by both left child & right child of it. Then clearly answer for maximum value for an interval represented by parent node is [`max`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html#v:max) of these two values. Thus, we can define [`Semigroup`](https://en.wikipedia.org/wiki/Semigroup) instance for our nodes and build our tree like so:

```haskell
import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Unboxed.Mutable  as VUM

newtype Node = Node { getValue :: Int }

instance Semigroup Node where
  Node a <> Node b = Node $ max a b

{-# INLINE left #-}
left at = at `shiftL` 1

{-# INLINE right #-}
right at = (at `shiftL` 1) .|. 1

-- Initially call `build 1 0 (n - 1) segTree givenArray` where 
-- `0` represents starting index of `givenArray` and `(n - 1)` represents ending.
-- On contrast to `givenArray`, our `segTree` is one-indexed (and not zero), 
-- thus our first argument, which represents root's index, is given as `1`.
-- You can make it zero-indexed but then use `2 * i + 1` & `2 * i + 2` to get
-- index of left & right child respectively.
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
```

Here when `start == end`, means, we have reached leaf node and store corresponding `givenArray`'s (represented in code as `as`) element. Otherwise, we recurse over and left & right child and then combine their values using our `mappend`.

By [Master theorem](https://en.wikipedia.org/wiki/Master_theorem_(analysis_of_algorithms)), we can see that construction has time complexity of $O(N)$ as here $T(n) = 2T(n/2) + O(1)$.

## Unboxing `Node`

From [GHC docs](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/primitives.html): 
  > Most types in GHC are boxed, which means that values of that type are represented by a pointer to a heap object. 

This usually introduces significant overhead both in terms of speed and memory. For instance, you may want to compare these two submissions: [submission 1](https://atcoder.jp/contests/practice2/submissions/35457740) & [submission 2](https://atcoder.jp/contests/practice2/submissions/35457877) (just using boxed vector instead of unboxed in later) and see both time and memory consumption are increased by more than twice.

Thus it would be preferable to unbox our `Node` type. And we can do it with help of [`vector-th-unbox`](http://hackage.haskell.org/package/vector-th-unbox). Kindly see the documentation of [`derivingUnbox`](https://hackage.haskell.org/package/vector-th-unbox-0.2.2/docs/Data-Vector-Unboxed-Deriving.html#v:derivingUnbox).

```haskell
import           Data.Vector.Unboxed.Deriving

newtype Node = Node { getValue :: Int }

derivingUnbox "Node"
  [t|Node -> Int|]
  [|\(Node v) -> v|]
  [|Node|]  -- is same as `[|\v -> Node v|]`
```

This is also the reason, why I like atcoder.jp in contrast to other online judges. They have [rich](https://github.com/haskell-jp/atcoder-haskell-resources/blob/master/spec.md) setup. Whereas, one of the most popular judge, codeforces.com doesn't even have support of vector.

## Querying

Let's first see the code.

```haskell
instance Monoid Node where
  mempty = Node (-1)  -- our input is set of non-negative integers, so this works fine

-- Initially, call `query 1 0 (n - 1) givenQueryStart givenQueryEnd ourSegTree`
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
```

Idea is that, if the current interval represented by node have no intersection then we return monoidal identity, yes as such a value can be combined by caller (we therefore, would also need to define an instance of [`Monoid`](https://en.wikipedia.org/wiki/Monoid)).

And if the current interval represented by node lies entirely in the given range, then we simply return its value.

Else, we have case of partial intersection, we recurse on left & right child, and then combine their values.

Why it works should be clear but it's time complexity isn't clear. It's time complexity is $O(h)$ where $h$ represents height of the tree (in our case, it's $k = \log_2 N$) and proof of that is [here](https://stackoverflow.com/questions/30236813/segment-tree-query-complexity) (see [this](https://qr.ae/pvcXhT) answer, in case you want help of diagrams).

## Performing point updates on our tree

In case we want to update a single element (I call it "point update") of our `givenArray`, we can update our tree, in $O(h)$ like so:

```haskell

-- Initially, call `query 1 0 (n - 1) ourSegTree givenIndex valueToUpdate`.
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
```

Why it works and its time complexity should be easy to see.

### In case number of elements in an array is not power of two

We assumed that number of elements in the array ( $N$ ) to be power of two. In general, we can find a minimum power of two $\ge N$, which is $2 ^ {\lceil {\log_2 N} \rceil}$. In such a case, number of nodes in the tree would be $2 \times 2 ^ {\lceil {\log_2 N} \rceil} - 1$ (and in such a case, our tree won't be dense). Now, assume $N = 2 ^ l + 1$ for some natural number $l$. In, such a case, our bound for number of nodes would become $2 \times 2 ^ {l + 1} - 1$ which is $= 2 \times 2 \times (N - 1) - 1 = 4N - 5$. This is the reason, why at many times, users set the size of their segment tree data structure to be $4N$, although I have set it to $2 \times 2 ^ {\lceil {\log_2 N} \rceil}$ (not $2 \times 2 ^ {\lceil {\log_2 N} \rceil} - 1$ since, my array is one-indexed, so I need a placeholder for `segTree[0]`).

## [Binary search](https://en.wikipedia.org/wiki/Binary_search_algorithm) over segment tree

With regard to query of type `3`, I believe, it's best understood by going through the following code snippet and trying to understand it with the help of comments.

```haskell
-- Initially, call `getFirst 1 0 (n - 1) givenQueryStart givenQueryEnd ourSegTree givenValue`
getFirst at start end rangeStart rangeEnd segTree v
  -- No intersection
  | start > rangeEnd || end < rangeStart = return mempty
  -- Node lies inside our range.
  | rangeStart <= start && end <= rangeEnd = do
      Node atValue <- VUM.read segTree at
      if atValue < v then return mempty  -- this node is thus useless, we return to the caller.
      else  -- we now are very close to our answer.
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
      -- we first try the left child, if it doesn't work, we try right child.
      Node result <- getFirst (left at) start mid rangeStart rangeEnd segTree v
      if result /= (-1) then return $ Node result
      else getFirst (right at) (mid + 1) end rangeStart rangeEnd segTree v
```

Now that you have understood these basics, you can write your own solution to [this](https://atcoder.jp/contests/practice2/tasks/practice2_j) problem (my solution is [here](./at_practice2_j.hs)).

## Lazy tree for range updates

_Maybe it's better to see the below code snippet (full code [here](./at_practice2_k.hs)) for [this](https://atcoder.jp/contests/practice2/tasks/practice2_k) problem, along with the explaination provided here_.

We have seen that we can do point updates in $O(h)$ but if we want to update a range consisting of say $r$ elements, then performing point updates $r$ time for each given range element, would result in time complexity of $O(h \times r)$.

Just like how, we returned node's value when we had entire interval represented by our node inside the given range, for query operation. Similarly, when doing range update, if we can just update this node's value without recursing over its children, we would have solved our problem.

So, instead of recursing over children, we can mark them as pending and defer their operation. But what if our parent node got another update in future, parent node's value will get updated for sure, but to add the current operation as pending over children (on top of previous pending operations on them), we would want our operation to be _composable_. That's it.

Below code is snippet of [my submission](./at_practice2_k.hs) for [this](https://atcoder.jp/contests/practice2/tasks/practice2_k) problem.

```haskell
-- for each `Node`, we also maintain the length of the interval represented by it.
data Node = Node { getValue :: Int, getLength :: Int } deriving Show

derivingUnbox "Node"
  [t|Node -> (Int, Int)|]
  [|\(Node v l) -> (v, l)|]
  [| uncurry Node|]  -- `uncurry Node` is same as `\(v, l) -> Node v l`

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

pushDown at start end segTree lazyTree = do
  UpdateParam b c <- VUM.read lazyTree at
  Node v l <- VUM.read segTree at
  -- Applying update to this node.
  VUM.write segTree at $ Node (addModulo (mulModulo v b) (mulModulo l c)) l
  VUM.write lazyTree at emptyParam
  when (start /= end) $ do
     let leftIndex = left at
         rightIndex = right at
         -- Deferring updation for children, composing their operation. Try to use paper-pen to see why this composition works!
         compose (UpdateParam prevB prevC) = UpdateParam (mulModulo b prevB) (addModulo (mulModulo b prevC) c)
     leftUpdateParam <- VUM.read lazyTree leftIndex
     rightUpdateParam <- VUM.read lazyTree rightIndex
     VUM.write lazyTree leftIndex $ compose leftUpdateParam
     VUM.write lazyTree rightIndex $ compose rightUpdateParam

update at start end segTree rangeStart rangeEnd lazyTree updateParam = do
    lazyAt <- VUM.read lazyTree at
    -- We must first do this lazy check before checking no intersection case, as we need to update the parent (caller), which relies on this child's latest value.
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
```

## Persistent Segment trees

Unfortunately, I couldn't find a problem pertaining to this at atcoder.jp, so decided to explain this with the help of problem I am familiar with from codeforces.com, viz., [Jamie and To-do List](https://codeforces.com/contest/916/problem/D).

In this problem we construct a [`Map`](https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html), whose keys are the given string and value is an unique integer identifier. Why we do this? Well, for this problem, for each separate day, we maintain a `Map` whose keys are this unique identifier and value is the corresponding priority. Had this `Map` been a map from strings to priority, it would have taken too much space (as we have too many days).

In this problem, suppose our input array has indices from $0$ to maximum priority, $10^9$ and corresponding to these indices, array store _count_ of strings having this priority. Then to answer queries, we construct segment tree from this array where each node maintains _sum_ of its children value. We answer query by simply querying this tree for priorities from 0 to `givenPriority - 1`.

Now, as a usual rule of thumb, computer is able to do $10^8$ operations per second. But here I am saying to construct segment tree over $10^9$ elements. Well, we'll construct segment trees differently this time (yes by using pointers) and since the given number of priorities is bounded by the number of queries (which is $10^5$), we won't have these many nodes in our tree.

So we can start from empty tree and every time we need to, say add an element, we can call point `update` function with `index` being the given priority and value being `1` (here instead of setting leaf with the given priority, we'll add this priority with value at leaf, that is to say, we'll `mappend` to it). And in case of delete (provided the said element exist in our tree, we use our unique identifier to priority `Map` to check this), value would be `-1`.

Since we have `undo` operations here, we'll construct an array which will store a segment tree for each day. But that does not mean we are copying the entire tree each time. It's like how `Data.Map` works, each insertion in `Map` is $O(\log n)$ meaning, entire tree is not copied. In case of our segment tree, in case of `update`, only nodes related to our update path are updated and they are the ones which require additional memory.

Would suggest now to look at [my solution](./cf_457D2_D_simple.hs) for this problem. Unfortunately, my solution exceeds the [time limit](https://codeforces.com/contest/916/submission/175215265) of 2 seconds but that doesn't mean it's asymptotically inefficient, in fact, for the maxed out test case, my solution runs in around ~5 seconds.

Note that since we have recursive data structure (`Node` referring `Node`), I don't think, it makes sense to talk about unboxing it but would love to be corrected.
