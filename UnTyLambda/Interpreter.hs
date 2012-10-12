{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- В данном задании требуется реализовать интерпретатор для
-- нетипизированной лямбды
--------------------------------------------------------------------------------

module UnTyLambda.Interpreter where

-- Какие-то импорты. Заметьте, что в этом задании надо
-- использовать обычную Prelude
import Prelude hiding (catch)
import Control.Exception

------------------------------------------------------------
-- Определение дататайпа для нетипизированной лямбды
type Variable = String
data Term = Var Variable | Lam Variable Term | App Term Term deriving (Show,Read)

------------------------------------------------------------
-- Дальше всё на ваше усмотрение

-- Если внутри будете использовать именованное представление, то
-- я тут решил немного вам помочь
-- (иначе говоря, код из этого раздела можно совсем выкинуть,
-- если хочется)

free (Var v)    = [ v ]
free (Lam v t)  = filter (/= v) . free $ t
free (App t t') = (free t) ++ (free t')

subst :: Term -> Variable -> Term -> Term
subst t@(Var v)   var what = if v == var then what else t
subst t@(Lam v b) var what = if v == var then t else Lam v (subst b var what)
subst (App t t')  var what = App (subst t var what) (subst t' var what)

newname fv = head . filter (not . flip elem fv) . iterate ('_':)

alphaConvert :: Term -> [Variable] -> Term
alphaConvert (Var v) vars = (Var v)
alphaConvert (App t1 t2) vars = App (alphaConvert t1 vars) (alphaConvert t2 vars)
alphaConvert (Lam v t) vars = (Lam newfree (alphaConvert (subst t v (Var newfree)) (vars++[newfree]))) where
	newfree = newname vars v
--- ...
betaRuduction :: Term -> Term
betaRuduction (App (Lam v t1) t2) = subst ren v t1 where 
    (Lam _ ren) =  alphaConvert (Lam v t1) (free (App (Lam v t1) t2)) 
betaRuduction x  = x

------------------------------------------------------------
-- За исключением того, что требуется реализовать следующие
-- стратегии нормализации (они все принимают максимальное
-- число шагов интерпретатора в качестве первого 
-- параметра (n); если за n шагов нормализовать не удаётся,
-- то следует бросать error, тестер его поймает):

hasRegExpStrong :: Term -> Bool
hasRegExpStrong (Var v) = False
hasRegExpStrong (Lam v t) = hasRegExpStrong t
hasRegExpStrong (App (Lam v t1) t2) = True
hasRegExpStrong (App t1 t2) = (hasRegExpStrong t1) || (hasRegExpStrong t2)

hasRegExpPoor :: Term -> Bool
hasRegExpPoor (App (Lam v t1) t2) = True
hasRegExpPoor (App t1 t2) = (hasRegExpPoor t1) || (hasRegExpPoor t2)
hasRegExpPoor _ = False



wh, no, wa, sa :: Integer -> Term -> Term

-- Редукция аппликативным порядком
makeRegExSa :: Term -> Term
makeRegExSa (Var v) = (Var v)
makeRegExSa (Lam v t) = Lam v (makeRegExSa t)
makeRegExSa (App t1 t2) = if (hasRegExpStrong t2) then (App t1 (makeRegExSa t2)) else ololo
	where ololo = if (hasRegExpStrong t1) then (App (makeRegExSa t1) t2) else (betaRuduction (App t1 t2))
	
sa 0 t = if (hasRegExpStrong t) then error $ "Too long sequence at [" ++ show t ++ "]" else t
sa n t = sa (n - 1) (makeRegExSa t)

-- Нормализация нормальным порядком
makeRegExNo :: Term -> Term
makeRegExNo (Var v) = (Var v)
makeRegExNo (Lam v t) = Lam v (makeRegExNo t)
makeRegExNo (App (Lam v t1) t2) = betaRuduction (App (Lam v t1) t2)
makeRegExNo (App t1 t2) = if (hasRegExpStrong t1) then (App (makeRegExNo t1) t2) else (App t1 (makeRegExNo t2))
	
no 0 t = if (hasRegExpStrong t) then error $ "Too long sequence at [" ++ show t ++ "]" else t
no n t = no (n - 1) (makeRegExNo t)

-- Редукция в слабую головную нормальную форму
makeRegExWh :: Term -> Term
makeRegExWh (App (Lam v t1) t2) = betaRuduction (App (Lam v t1) t2)
makeRegExWh (App t1 t2) = if (hasRegExpPoor t1) then (App (makeRegExWh t1) t2) else (App t1 (makeRegExWh t2))
makeRegExWh t = t

wh 0 t = if (hasRegExpPoor t) then error $ "Too long sequence at [" ++ show t ++ "]" else t
wh n t = wh (n - 1) (makeRegExWh t)

-- (*) (не обязательно) Редукция "слабым" аппликативным порядком.
-- Отличается от обычного аппликативного тем, что не лезет внутрь
-- лямбд и правые части аппликаций, когда это возможно.
wa = undefined

-- Замечание: cкорость работы вашего интерпретатора специально не оценивается,
-- потому можно использовать свой изоморфный (с точностью до альфа-конверсии)
-- тип для представления термов и преобразовывать Term в него и обратно.

-- Перечисление всех этих порядков (в порядке отличном от
-- определения, да)
orders =
    [ ("wh", wh)
    , ("no", no)
--    , ("wa", wa) -- Можно раскоментировать, да
    , ("sa", sa) ]

------------------------------------------------------------
-- Игнорируйте это, если выглядит непонятно
pall term = mapM_ $ \(d, x) -> putStr (d ++ ": ") >> catch (let t = x 1000 term in seq t (print t)) (\(e :: SomeException) -> print e)
testfuncs funcs = mapM_ $ \t -> putStr "===== " >> print t >> pall t funcs

------------------------------------------------------------
-- Сюда можно добавлять тесты
lamxx = Lam "x" $ App (Var "x") (Var "x")
omega = App lamxx lamxx

test = testfuncs orders
    [ Var "a"
    , Lam "x" $ (Lam "y" $ Var "y") `App` (Var "x")
    , (Lam "x" $ Lam "y" $ Var "x") `App` (Var "y")
    , omega
    ]

------------------------------------------------------------
-- Немного теоретических замечаний, если они вас волнуют
--
-- Следует специально отметить, что поскольку в конце теста
-- результат вычисления печатают, то ленивость Haskell не
-- влияет на семантику интерпретируемого исчисления.
--
-- Чтобы это особенно подчеркнуть в тестере выше я написал
-- seq в интересном месте (хотя конкретно это там ничего не
-- гарантирует, на самом-то деле).
