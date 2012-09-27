{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil |  Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length Nil = Zero
length (Cons a xs) = (natOne +. (length xs))

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
Nil ++ b = b
(Cons a xs)  ++ b = (Cons a (xs ++ b))

-- Список без первого элемента
tail :: List a -> List a
tail Nil = Nil
tail (Cons a xs) = xs

-- Список без последнего элемента
init :: List a -> List a
init Nil = Nil
init (Cons a Nil) = Nil
init (Cons a xs) = (Cons a (init xs))

-- Первый элемент
head :: List a -> a
head Nil = error "!!: empty list"
head (Cons a xs) = a

-- Последний элемент
last :: List a -> a
last (Cons a Nil) = a
last Nil = error "!!: empty list"
last (Cons a xs) = last xs

-- n первых элементов списка
take :: Nat -> List a -> List a
take Zero b = Nil
take n Nil = Nil
take n (Cons a xs) = Cons a (take (n -. natOne) xs)

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop Zero a = a
drop (Succ n) (Cons a xs) = drop n xs

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (Cons a xs) = (if' (p a) (Cons a (filter p xs)) (filter p xs))

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter p Nil = Nil
gfilter p (Cons a xs) = case p a of
	Nothing -> rest
	Just b -> Cons b rest
	where rest = gfilter p xs

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p Nil = Nil
takeWhile p (Cons a xs) = (if' (p a) (Cons a (takeWhile p xs)) Nil)

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p Nil = Nil
dropWhile p (Cons a xs) = (if' (p a) (takeWhile p xs) (Cons a xs))

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span p Nil = Pair Nil Nil
span p (Cons a xs) = (if' (p a) (Pair (Cons a ((fst . span p) xs)) ((snd . span p) xs)) (Pair Nil xs))

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break p a = span (not . p) a

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
(Cons a xs) !! Zero = a
(Cons a xs) !! (Succ n) = xs !! n

-- Список задом на перёд
reverse :: List a -> List a
reverse (Cons a xs) = (reverse xs) ++ (Cons a Nil)

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons a xs) = subsequences xs ++ (map (Cons a) $ subsequences xs)

-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations Nil = Cons Nil Nil
permutations a = extr (length a) a

extr :: Nat -> List a -> List (List a)
extr Zero a = Nil
extr (Succ a) (Cons b c) = (map (Cons b) $ permutations c) ++ (extr a $ reverse (Cons b $ reverse c))

-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat a = Cons a $ repeat a

-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl f z Nil = z
foldl f z (Cons a xs) = foldl f (f z a) xs

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl f z Nil = Cons z Nil
scanl f z (Cons a xs) = Cons fx $ scanl f fx xs where fx = f z a 

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z Nil = z
foldr f z (Cons a xs) = f a $ foldr f z xs
 
-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr f z Nil = (Cons z Nil)
scanr f z (Cons a xs) = Cons (f a $ head rest) rest where rest = scanr f z xs

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map f Nil = Nil
map f (Cons a xs) = Cons (f a) $ map f xs

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat = foldr (++) Nil

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap = undefined

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (Cons a xs) (Cons b ys) = Cons (Pair a b) $ zip xs ys

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f Nil _ = Nil
zipWith f _ Nil = Nil
zipWith f (Cons a xs) (Cons b ys) = Cons (f a b) (zipWith f xs ys)
