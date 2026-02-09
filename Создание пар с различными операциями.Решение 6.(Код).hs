module Main where

import Data.Char (toUpper)

-- 1. Базовая функция zipWith из стандартной библиотеки
-- Тип: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []  -- Если первый список пуст
zipWith' _ _ [] = []  -- Если второй список пуст
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- 2. Более безопасная версия с обработкой разной длины
-- (останавливается, когда один из списков заканчивается)
safeZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
safeZipWith _ [] _ = []
safeZipWith _ _ [] = []
safeZipWith f (x:xs) (y:ys) = f x y : safeZipWith f xs ys

-- 3. Версия, которая дополняет более короткий список значением по умолчанию
zipWithDefault :: a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithDefault defA defB f = go
  where
    go [] [] = []
    go [] (y:ys) = f defA y : go [] ys
    go (x:xs) [] = f x defB : go xs []
    go (x:xs) (y:ys) = f x y : go xs ys

-- Примеры использования

-- Пример 1: Сложение соответствующих элементов
addPairs :: [Int] -> [Int] -> [Int]
addPairs = zipWith' (+)

-- Пример 2: Объединение строк
concatPairs :: [String] -> [String] -> [String]
concatPairs = zipWith' (++)

-- Пример 3: Создание пар с различными операциями
makePairs :: [(Int, Char)]
makePairs = zipWith' (,) [1,2,3] ['a','b','c']  -- [(1,'a'), (2,'b'), (3,'c')]

-- Пример 4: Применение к спискам разной длины
example1 :: [Int]
example1 = safeZipWith (*) [1,2,3] [4,5]  -- [4,10] (только для пар)

-- Пример 5: С дополнением нулями
example2 :: [Int]
example2 = zipWithDefault 0 0 (+) [1,2,3] [4,5]  -- [5,7,3]

-- 5. Тестирование функции
testZipWith :: IO ()
testZipWith = do
    putStrLn "*** Тестирование zipWith' ***"
    
    putStr "1. Сложение: "
    print $ zipWith' (+) [1,2,3] [4,5,6]  -- [5,7,9]
    
    putStr "2. Умножение: "
    print $ zipWith' (*) [1,2,3] [4,5,6]  -- [4,10,18]
    
    putStr "3. Сравнение: "
    print $ zipWith' (<) [1,2,3] [4,5,1]  -- [True,True,False]
    
    putStr "4. Работа с разными типами: "
    print $ zipWith' (\x y -> (x, y)) [1,2,3] ['a','b','c']  -- [(1,'a'),(2,'b'),(3,'c')]
    
    putStr "5. Разные длины: "
    print $ safeZipWith (+) [1,2,3] [4,5]  -- [5,7]
    
    putStr "6. С дополнением: "
    print $ zipWithDefault 0 0 (+) [1,2,3] [4,5]  -- [5,7,3]

-- 6. Более сложный пример: обработка данных
data Person = Person { name :: String, age :: Int } deriving (Show)

-- Создание списка персон из двух списков
createPersons :: [String] -> [Int] -> [Person]
createPersons = zipWith' Person

-- Пример использования
examplePersons :: [Person]
examplePersons = createPersons ["Alice", "Bob", "Charlie"] [25, 30, 35]
-- [Person {name = "Alice", age = 25}, ...]

-- 7. Практическое применение: обработка изображений (упрощённо)
type Pixel = (Int, Int, Int)  -- RGB
type Image = [[Pixel]]

-- Применение функции к каждому пикселю двух изображений
blendImages :: (Pixel -> Pixel -> Pixel) -> Image -> Image -> Image
blendImages f img1 img2 = zipWith' (zipWith' f) img1 img2

-- Функция смешивания пикселей (усреднение)
averagePixels :: Pixel -> Pixel -> Pixel
averagePixels (r1,g1,b1) (r2,g2,b2) = 
    ( (r1 + r2) `div` 2
    , (g1 + g2) `div` 2
    , (b1 + b2) `div` 2
    )

-- 8. Расширенная версия: zipWith с индексом
zipWithIndex :: (Int -> a -> b -> c) -> [a] -> [b] -> [c]
zipWithIndex f xs ys = go 0 xs ys
  where
    go _ [] _ = []
    go _ _ [] = []
    go i (x:xs) (y:ys) = f i x y : go (i+1) xs ys

-- Пример: нумерованные пары
indexedPairs :: [String] -> [Int] -> [(Int, String, Int)]
indexedPairs = zipWithIndex (\i s n -> (i, s, n))

-- 9. Основная функция для демонстрации
main :: IO ()
main = do
    testZipWith
    
    putStrLn "\n*** Создание персон ***"
    print examplePersons
    
    putStrLn "\n*** ZipWith с индексом ***"
    print $ indexedPairs ["a","b","c"] [1,2,3]  -- [(0,"a",1),(1,"b",2),(2,"c",3)]
    
    putStrLn "\n*** Практический пример: обработка текста ***"
    let words1 = ["Hello", "Functional", "Programming"]
        words2 = ["World", "is", "awesome"]
        combined = zipWith' (\w1 w2 -> w1 ++ " " ++ w2) words1 words2
    mapM_ putStrLn combined
    -- Hello World
    -- Functional is
    -- Programming awesome