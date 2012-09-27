{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read)

---------------------------------------------
-- ���⠪�� �ﬡ��-��ࠦ����

-- ����������� ��।������
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- ����� ��������� ��।������
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- ��横������ ��ࠦ����
undefined = undefined

-- ���� ᫥��� ॠ�������� �� ���, ����騥 �� undefined �����誨.
-- ��� ��� ����� ��९��뢠�� (natEq � natLt --- ��訥 ���������).

-------------------------------------------
-- �ਬ�⨢�� ⨯�

-- ��� � �����⢥��� ����⮬
data Unit = Unit deriving (Show,Read)

-- ���, �ந��������
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- ��ਠ��, ���ந��������
data Either a b = Left a | Right b deriving (Show,Read)

-- ����� ���� ��砩, ������䭮 Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- ����� ���� ��砩, ������䭮 Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- ������ �⬥���, �� ���஥��� if � �⨬ Bool �ᯮ�짮���� �����,
-- ��� case �ᥣ�� ࠡ�⠥�.

-- �� ��� ����� ॠ�������� ᢮� if
if' True a b = a
if' False a b = b

-- ���⮬��. �����⥫�� ⨯, �����뢠�騩 १���� �ࠢ�����
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- �㫥�� ���祭��

-- �����᪮� "��"
not :: Bool -> Bool
not True = False
not False = True

pairBoolToTri :: Bool -> Bool -> Tri
pairBoolToTri fl True = EQ
pairBoolToTri False False = GT
pairBoolToTri True False = LT

infixr 3 &&
-- �����᪮� "�"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- �����᪮� "���"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- ����ࠫ�� �᫠

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

natToBool :: Nat -> Bool
natToBool Zero = False
natToBool (Succ _) = True


-- �ࠢ������ ��� ����ࠫ��� �᫠
natCmp :: Nat -> Nat -> Tri
narCmp Zero Zero = EQ
natCmp Zero (Succ _) = LT
natCmp (Succ _) Zero = GT
natCmp (Succ n) (Succ m) = natCmp n m
-- natCmp = undefined

-- n ᮢ������ � m 
natEq :: Nat -> Nat -> Bool
natEq Zero     Zero     = True
natEq Zero     (Succ _) = False
natEq (Succ _) Zero     = False
natEq (Succ n) (Succ m) = natEq n m

-- n ����� m
natLt :: Nat -> Nat -> Bool
natLt Zero     Zero     = False
natLt Zero     (Succ m) = True
natLt (Succ n) Zero     = False
natLt (Succ n) (Succ m) = natLt n m

infixl 6 +.
-- �������� ��� ����ࠫ��� �ᥫ
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- ���⠭�� ��� ����ࠫ��� �ᥫ
(-.) :: Nat -> Nat -> Nat
Zero -. Zero = Zero
Zero -. m = Zero
m -. Zero = m
(Succ n) -. (Succ m) = n -. m
--n -. m = undefined

infixl 7 *.
-- ��������� ��� ����ࠫ��� �ᥫ
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

-- ����� � ���⮪ �� ������� n �� m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod Zero Zero = undefined
natDivMod n Zero = undefined
natDivMod Zero m = Pair Zero Zero
natDivMod n m = Pair (natDiv n m) (natMod n m) 
--natDivMod n m = undefined

-- ���� GCD �����⬮� ������� (������ �������� 2 (���᫨⥫��쭠� ����) + 1 (⨯) ���窨)
gcd :: Nat -> Nat -> Nat
gcd Zero n = Succ(Zero)
gcd n m = (if' (natLt n m) (gcd (natMod m n) n) (gcd (natMod n m) m))

-------------------------------------------
-- ���� �᫠

-- �ॡ����, �⮡� �।�⠢����� ������� �᫠ �뫮 �����⢥���
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

-- ����� ⠪�� ��� ��� ����ࠫ���
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
-- � ���� �� �����⢥��� ����� �� �� ��� 䠩��
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
-- ��樮����� �᫠

data Rat = Rat Int Nat

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- � �樮������ ��� ���� ����� ������
ratInv :: Rat -> Rat
ratInv (Rat x y)= Rat (pairNatToInt (intSign x) y) (intAbs x)


ratNorm :: Rat -> Rat
ratNorm (Rat x y) = Rat (pairNatToInt (intSign x) (natDiv (intAbs x) (gcd (intAbs x) y))) (natDiv y (gcd (intAbs x) y))  

ratNumb :: Rat -> Int
ratNumb (Rat x y) = x

ratDec :: Rat -> Nat
ratDec (Rat x y) = y

-- ����� ��� ���筮
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
-- ����樨 ��� �㭪�ﬨ.
-- ��।����� �����, �� �ᯮ�짮���� ����� � ���

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- ����������� ��।������
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- � ��� ��������� ��।������
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b
