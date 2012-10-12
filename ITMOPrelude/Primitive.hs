{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read)

---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

pairBoolToTri :: Bool -> Bool -> Tri
pairBoolToTri fl True = EQ
pairBoolToTri False False = GT
pairBoolToTri True False = LT

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

natToBool :: Nat -> Bool
natToBool Zero = False
natToBool (Succ _) = True


-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
narCmp Zero Zero = EQ
natCmp Zero (Succ _) = LT
natCmp (Succ _) Zero = GT
natCmp (Succ n) (Succ m) = natCmp n m
-- natCmp = undefined

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq Zero     Zero     = True
natEq Zero     (Succ _) = False
natEq (Succ _) Zero     = False
natEq (Succ n) (Succ m) = natEq n m

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt Zero     Zero     = False
natLt Zero     (Succ m) = True
natLt (Succ n) Zero     = False
natLt (Succ n) (Succ m) = natLt n m

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
Zero -. Zero = Zero
Zero -. m = Zero
m -. Zero = m
(Succ n) -. (Succ m) = n -. m
--n -. m = undefined

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

natDiv :: Nat -> Nat -> Nat
natDiv n Zero = undefined
natDiv Zero n = Zero
natDiv n m = (if' (natLt n m)  Zero (Succ (natDiv (n -. m) m))) 

natMod :: Nat -> Nat -> Nat
natMod n Zero = undefined
natMod Zero n = Zero
natMod n m = (if' (natLt n m) n (natMod (n -. m) m))  

-- Целое и остаток от деления n на m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod Zero Zero = undefined
natDivMod n Zero = undefined
natDivMod Zero m = Pair Zero Zero
natDivMod n m = Pair (natDiv n m) (natMod n m) 
--natDivMod n m = undefined

-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd Zero n = Succ(Zero)
gcd n m = (if' (natLt n m) (gcd (natMod m n) n) (gcd (natMod n m) m))

-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data Int = Neg Nat | Pos Nat deriving (Show,Read)

intZero   = Pos natZero   -- 0
intOne    = Pos natOne   -- 1
intNegOne = Neg natZero -- -1

intAbs :: Int -> Nat
intAbs (Pos a) = a
intAbs (Neg a) = a +. natOne

intSign :: Int -> Nat
intSign (Pos a) = Zero
intSign (Neg a) = Succ Zero

pairNatToInt :: Nat -> Nat -> Int
pairNatToInt a Zero = intZero
pairNatToInt Zero a  = Pos a
pairNatToInt (Succ _) a = Neg (a -. natOne)

-- n -> - n
intNeg :: Int -> Int
intNeg (Pos Zero) = (Pos Zero)
intNeg (Pos (Succ a)) = (Neg a)
intNeg (Neg a) = (Pos (Succ a))

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp n m = pairBoolToTri (intLt n m) (intEq n m )

intEq :: Int -> Int -> Bool
intEq n m = natEq (intAbs n) (intAbs m) && natEq (intSign n) (intSign m)

intLt :: Int -> Int -> Bool
intLt n m = natToBool( intSign(n .-. m))

intInc :: Int -> Int -> Bool -> Int
intInc n m True = pairNatToInt (intSign n) ((intAbs n) +. (intAbs m))
intInc n m False = pairNatToInt (intSign n) ((intAbs n) -. (intAbs m))


infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
n .+. m = intInc (if' (natLt (intAbs n) (intAbs m)) m n) (if' (natLt (intAbs n) (intAbs m)) n m) (natEq (intSign n) (intSign m))
 

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
n .*. m = pairNatToInt (if' (natEq (intSign n) (intSign m)) natZero natOne) ((intAbs n) *. (intAbs m))

infixl 7 .*..
(.*..) :: Int -> Nat -> Int
n .*.. m = pairNatToInt (intSign n)((intAbs n) *. m)

-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat x y)= Rat (pairNatToInt (intSign x) y) (intAbs x)


ratNorm :: Rat -> Rat
ratNorm (Rat x y) = Rat (pairNatToInt (intSign x) (natDiv (intAbs x) (gcd (intAbs x) y))) (natDiv y (gcd (intAbs x) y))  

ratNumb :: Rat -> Int
ratNumb (Rat x y) = x

ratDec :: Rat -> Nat
ratDec (Rat x y) = y

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp n m = pairBoolToTri (ratLt n m) (ratEq n m)

ratEq :: Rat -> Rat -> Bool
ratEq n m = (intEq (ratNumb (n %- m)) intZero)

ratLt :: Rat -> Rat -> Bool
ratLt n m = (intLt (ratNumb (n %- m)) intZero)

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat x1 y1) %+ (Rat x2 y2) = ratNorm(Rat (x1 .*.. y2 .+. x2 .*..y1) (y1 *. y2))

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat x1 y1) %* (Rat x2 y2) = ratNorm (Rat (x1 .*. x2) (y1 *. y2))

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b
