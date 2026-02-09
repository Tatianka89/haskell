module Main where

import Data.List (nub)
import Data.Maybe (fromMaybe, isJust)

-- Типы для имен классов и методов
type ClassName = String
type Method = String

-- Рекурсивный тип данных для представления класса
-- Каждый класс содержит имя, список методов и список подклассов
data Class = Class 
    { className   :: ClassName
    , classMethods :: [Method]
    , subclasses  :: [Class]
    } deriving (Show, Eq)

-- Иерархия классов представлена лесом (список корневых классов)
type ClassHierarchy = [Class]

-- Вспомогательные функции для поиска в иерархии

-- Найти класс по имени (возвращает Maybe Class)
findClass :: ClassHierarchy -> ClassName -> Maybe Class
findClass [] _ = Nothing
findClass (c:cs) name
    | className c == name = Just c
    | otherwise = case findClass (subclasses c) name of
                    Just found -> Just found
                    Nothing -> findClass cs name

-- Найти родителя класса (возвращает Maybe ClassName)
findParent :: ClassHierarchy -> ClassName -> Maybe ClassName
findParent [] _ = Nothing
findParent (c:cs) name
    | any ((== name) . className) (subclasses c) = Just (className c)
    | otherwise = case findParent (subclasses c) name of
                    Just parent -> Just parent
                    Nothing -> findParent cs name

-- 1) getParent - возвращает непосредственного предка класса
getParent :: ClassHierarchy -> ClassName -> Maybe ClassName
getParent = findParent

-- 2) getPath - возвращает список всех предков (от непосредственного к корню)
getPath :: ClassHierarchy -> ClassName -> [ClassName]
getPath hierarchy name = 
    case getParent hierarchy name of
        Nothing -> []
        Just parent -> parent : getPath hierarchy parent

-- 3) getMethods - возвращает список методов класса с учётом наследования
--    (методы потомка имеют приоритет над методами предка)
getMethods :: ClassHierarchy -> ClassName -> [Method]
getMethods hierarchy name =
    case findClass hierarchy name of
        Nothing -> []  -- Класс не найден
        Just cls -> 
            let parentMethods = concatMap (getMethods hierarchy) (getPath hierarchy name)
            in nub (classMethods cls ++ parentMethods)

-- 4) inherit - добавляет новый класс в иерархию
inherit :: ClassHierarchy -> ClassName -> ClassName -> Maybe ClassHierarchy
inherit hierarchy childName parentName =
    if isJust (findClass hierarchy childName)
    then Nothing  -- Класс с таким именем уже существует
    else case findClass hierarchy parentName of
            Nothing -> Nothing  -- Родитель не найден
            Just _ -> Just (addClass hierarchy parentName childName)
    where
        -- Рекурсивная функция для добавления класса в нужное место иерархии
        addClass :: ClassHierarchy -> ClassName -> ClassName -> ClassHierarchy
        addClass [] _ _ = []
        addClass (c:cs) parentName childName
            | className c == parentName = 
                c { subclasses = Class childName [] [] : subclasses c } : cs
            | otherwise = 
                c { subclasses = addClass (subclasses c) parentName childName } : cs

-- Дополнительные функции

-- Проверка, является ли один класс предком другого
isAncestor :: ClassHierarchy -> ClassName -> ClassName -> Bool
isAncestor hierarchy ancestor descendant =
    ancestor `elem` getPath hierarchy descendant

-- Получить все подклассы класса (прямые и непрямые)
getAllSubclasses :: ClassHierarchy -> ClassName -> [ClassName]
getAllSubclasses hierarchy name =
    case findClass hierarchy name of
        Nothing -> []
        Just cls -> map className (subclasses cls) ++ 
                    concatMap (getAllSubclasses hierarchy . className) (subclasses cls)

-- Добавили метод в существующий класс
addMethod :: ClassHierarchy -> ClassName -> Method -> Maybe ClassHierarchy
addMethod hierarchy name method =
    case findClass hierarchy name of
        Nothing -> Nothing
        Just _ -> Just (updateClass hierarchy name method)
    where
        updateClass :: ClassHierarchy -> ClassName -> Method -> ClassHierarchy
        updateClass [] _ _ = []
        updateClass (c:cs) name method
            | className c == name = 
                c { classMethods = method : classMethods c } : cs
            | otherwise = 
                c { subclasses = updateClass (subclasses c) name method } : cs


-- Создание начальной иерархии
exampleHierarchy :: ClassHierarchy
exampleHierarchy = 
    [ Class "Object" [] 
        [ Class "Animal" ["eat", "sleep"]
            [ Class "Mammal" ["suckle"]
                [ Class "Dog" ["bark"] []
                , Class "Cat" ["meow"] []
                ]
            , Class "Bird" ["fly"] []
            ]
        , Class "Vehicle" ["move"]
            [ Class "Car" ["drive"] []
            , Class "Plane" ["fly"] []
            ]
        ]
    ]

-- Тестирование функций
main :: IO ()
main = do
    putStrLn "---- Тестирование иерархии классов ----"
    
    let hierarchy = exampleHierarchy
    
    putStrLn "\n1. Родитель класса Dog:"
    print $ getParent hierarchy "Dog"  -- Just "Mammal"
    
    putStrLn "\n2. Путь наследования для Dog:"
    print $ getPath hierarchy "Dog"  -- ["Mammal", "Animal", "Object"]
    
    putStrLn "\n3. Методы класса Dog (с наследованием):"
    print $ getMethods hierarchy "Dog"  -- ["bark", "suckle", "eat", "sleep"]
    
    putStrLn "\n4. Добавляем класс Wolf, наследующий от Mammal:"
    let newHierarchy = fromMaybe hierarchy $ inherit hierarchy "Wolf" "Mammal"
    print $ findClass newHierarchy "Wolf"  -- Должен найти новый класс
    
    putStrLn "\n5. Добавляем метод 'howl' классу Wolf:"
    let withMethod = fromMaybe newHierarchy $ addMethod newHierarchy "Wolf" "howl"
    print $ getMethods withMethod "Wolf"  -- ["howl", "suckle", "eat", "sleep"]
    
    putStrLn "\n6. Проверяем, является ли Animal предком Dog:"
    print $ isAncestor withMethod "Animal" "Dog"  -- True
    
    putStrLn "\n7. Все подклассы Animal:"
    print $ getAllSubclasses withMethod "Animal"  -- ["Mammal", "Bird", "Dog", "Cat", "Wolf"]