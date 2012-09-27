module ITMOPrelude.Tree where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive
import ITMOPrelude.List

data Tree a = Null | Node  a (Tree a) (Tree a)  deriving (Show,Read)

-- Пустое дерево
empty = Null

-- Добавить корень
addRoot :: Tree a -> a -> Tree a
addRoot Null r = Node r Null Null
addRoot t r = Node r t Null

-- Добавить элемент и сделать его самым левым
addLeft :: Tree a -> a -> Tree a
addLeft Null r = Node r Null Null
addLeft (Node t l r) e = Node t (addLeft l e) r

-- Добавить элемент и сделать его самым правым
addRight :: Tree a -> a -> Tree a
addRight Null r = Node r Null Null
addRight (Node t l r) e = Node t l (addRight r e)

-- Поворот влево
turnLeft :: Tree a -> Tree a
turnLeft Null = Null
turnLeft (Node t Null r) = Node t Null r
turnLeft (Node a (Node t tl tr) ar) = Node t tl (Node a tr ar)

-- Поворот вправо
turnRight :: Tree a -> Tree a
turnRight Null = Null
turnRight (Node t l Null) = Node t l Null
turnRight (Node a al (Node t tl tr)) = Node t (Node a tl al) tr

-- Аналог функции map. Применить функцию к каждому элементу дерева
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Null = Null
mapTree f (Node t l r) = (Node (f t) (mapTree f l) (mapTree f r))

-- Аналог функции foldr.
foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree f z Null = z
foldrTree f z (Node t l r) = foldrTree f (f t (foldrTree f z r)) l
