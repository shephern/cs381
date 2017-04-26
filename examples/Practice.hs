--Huh
plus :: Int -> (Int -> Int)
plus x y = x+y

minus :: Int -> (Int -> Int)
minus x y = x-y

ones :: [Int]
ones = 1 : ones

nats = 1 : map succ nats

{-
  nats
= 1:map succ nats
= 1:map succ (1:map succ nats)
= 1:succ 1:map succ (map succ nats)
= 1:2:map succ (map succ nats)
= 1:2:map (succ . succ) nats
= 1:2:map (+2) nats
= 1:2:map (+2) (1:2:map (+2) nats)
= 1:2:3:4:map (+2) (map (+2) nats)
= 1:2:3:4:map ((+2) .(+2)) nats
= 1:2:3:4:map (+4) nats
...

-}

{-
1  1  2  3  5  8 13 21 ...   fibs
1  2  3  5  8 13 21 ...      tail fibs
----------------------------------------
2  3  5  8 13 21 34          zipWith (+)
-}

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)


{-
  fibs
= 1:1:zipWith (+) fibs (tail fibs)
= 1:1:zipWith (+) (1:1:zipWith (+) fibs (tail fibs))
                  (tail (1:1:zipWith (+) fibs (tail fibs)))
= 1:1:zipWith (+) (1:1:zipWith (+) fibs (tail fibs))
                  (1:zipWith (+) fibs (tail fibs))
= 1:1:2:zipWith (+) (1:zipWith (+) fibs (tail fibs))
                    (zipWith (+) fibs (tail fibs))

For the next step, it's best to substitute the zipWith call in the second line,
which we already know is a list starting with a 2:

= 1:1:2:zipWith (+) (1:zipWith (+) fibs (tail fibs))
                    (2:zipWith (+) (1:zipWith (+) fibs (tail fibs))
                                   (zipWith (+) fibs (tail fibs)))
= 1:1:2:3:zipWith (+) (zipWith (+) fibs (tail fibs))
                      (zipWith (+) (1:zipWith (+) fibs (tail fibs))
                                   (zipWith (+) fibs (tail fibs)))
= 1:1:2:3:zipWith (+) (2:...)
                      (zipWith (+) (1:...)
                                   (2:...))
= 1:1:2:3:zipWith (+) (2:...)
                      (3:zipWith (+) (...) (...))
= 1:1:2:3:5:zipWith (+) ...

-}

data Tree = Node Int Tree Tree
          | Leaf
          deriving (Show)


t = Node 3 (Node 1 Leaf Leaf)
           (Node 5 Leaf Leaf)

t' = Node 7 t t
