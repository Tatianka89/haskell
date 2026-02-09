module Main where

-- Рекурсивная функция замены символа
f :: Char -> Char -> String -> String
f _ _ [] = []  -- Базовый случай: пустая строка
f old new (c:cs)
    | c == old  = new : f old new cs  -- Замена символа
    | otherwise = c : f old new cs    -- Оставляем символ без изменений

-- Тесты для проверки функции
testF :: Bool
testF = and
    [ f 'e' 'i' "eigenvalue" == "iiginvalui"
    , f 'a' 'o' "banana" == "bonono"
    , f 'x' 'y' "hello" == "hello"      -- Символ для замены отсутствует
    , f ' ' '_' "hello world" == "hello_world"
    , f 'l' 'L' "hello" == "heLLo"
    ]

-- Вспомогательная функция для демонстрации
demo :: IO ()
demo = do
    putStrLn "Функции замены символа:"
    putStrLn $ "f 'e' 'i' \"eigenvalue\" = " ++ show (f 'e' 'i' "eigenvalue")
    putStrLn $ "f 'a' 'o' \"banana\" = " ++ show (f 'a' 'o' "banana")
    putStrLn $ "f ' ' '_' \"hello world\" = " ++ show (f ' ' '_' "hello world")

-- Основная программа
main :: IO ()
main = do
    demo
    putStrLn ""
    if testF
        then putStrLn "Все тесты пройдены успешно!"
        else putStrLn "Тесты не пройдены."

    -- Дополнительно: интерактивный пример
    putStrLn "\nИнтерактивный пример:"
    putStr "Введите символ для замены: "
    old <- getLine
    putStr "Введите новый символ: "
    new <- getLine
    putStr "Введите строку: "
    str <- getLine
    case (old, new, str) of
        ([o], [n], s) -> putStrLn $ "Результат: " ++ f o n s
        _ -> putStrLn "Ошибка ввода: нужно ввести ровно по одному символу для замены и нового символа."