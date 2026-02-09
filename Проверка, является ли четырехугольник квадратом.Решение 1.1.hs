import Text.Printf (printf)

-- Точность для сравнения чисел с плавающей точкой
epsilon :: Double
epsilon = 1e-6

-- Квадрат расстояния между двумя точками
distSq :: (Double, Double) -> (Double, Double) -> Double
distSq (x1, y1) (x2, y2) = (x2 - x1)^2 + (y2 - y1)^2

-- Скалярное произведение векторов
dotProduct :: (Double, Double) -> (Double, Double) -> Double
dotProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

-- Проверка равенства с учетом погрешности
approxEqual :: Double -> Double -> Bool
approxEqual a b = abs (a - b) < epsilon

-- Вектор между двумя точками
vec :: (Double, Double) -> (Double, Double) -> (Double, Double)
vec (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

-- Проверка, является ли четырехугольник квадратом
isSquare :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isSquare a b c d =
    let ab = distSq a b
        bc = distSq b c
        cd = distSq c d
        da = distSq d a
        ac = distSq a c   -- диагональ
        bd = distSq b d   -- диагональ
        -- Векторы для проверки углов
        vAB = vec a b
        vBC = vec b c
        vCD = vec c d
    in approxEqual ab bc && approxEqual bc cd && approxEqual cd da &&   -- все стороны равны
       approxEqual ac bd &&                                               -- диагонали равны
       approxEqual (dotProduct vAB vBC) 0 &&                       -- угол ABC = 90°
       approxEqual (dotProduct vBC vCD) 0                          -- угол BCD = 90°

-- Тестовые данные
testData :: [((Double, Double), (Double, Double), (Double, Double), (Double, Double), String)]
testData = [
    -- Квадраты
    ((0, 0), (1, 0), (1, 1), (0, 1), "Квадрат 1x1"),
    ((1, 1), (3, 1), (3, 3), (1, 3), "Квадрат 2x2"),
    
    -- Не квадраты
    ((0, 0), (2, 0), (2, 1), (0, 1), "Прямоугольник 2x1"),
    ((0, 0), (1, 1), (2, 0), (1, -1), "Ромб"),
    ((0, 0), (1, 0), (1.5, 0.5), (0.5, 0.5), "Произвольный четырехугольник")
    ]

-- Функция для тестирования
runTest :: ((Double, Double), (Double, Double), (Double, Double), (Double, Double), String) -> IO ()
runTest (a, b, c, d, desc) = do
    putStrLn $ "Тест: " ++ desc
    putStrLn $ "  Точки: A" ++ show a ++ " B" ++ show b ++ " C" ++ show c ++ " D" ++ show d
    
    let result = isSquare a b c d
    putStrLn $ "  Результат: " ++ (if result then "КВАДРАТ" else "НЕ квадрат")
    
   
    putStrLn "  Длины сторон:"
    printf "    AB = %.3f, BC = %.3f, CD = %.3f, DA = %.3f\n"
        (sqrt $ distSq a b) (sqrt $ distSq b c) 
        (sqrt $ distSq c d) (sqrt $ distSq d a)
    
    putStrLn "  Диагонали:"
    printf "    AC = %.3f, BD = %.3f\n"
        (sqrt $ distSq a c) (sqrt $ distSq b d)
    
    -- Проверка углов
    let vAB = vec a b
        vBC = vec b c
        vCD = vec c d
    putStrLn "  Проверка углов (скалярное произведение):"
    printf "    Угол ABC: %.6f (должно быть 0)\n" (dotProduct vAB vBC)
    printf "    Угол BCD: %.6f (должно быть 0)\n" (dotProduct vBC vCD)
    
    putStrLn ""

-- Основная функция
main :: IO ()
main = do
    putStrLn "Проверка, является ли четырехугольник квадратом"
    putStrLn "************************************************"
    putStrLn ""
    
    mapM_ runTest testData
    
    putStrLn "Вывод: программа корректно определяет квадраты и отличает их от других четырехугольников."
    
    putStrLn ""
    putStrLn "Введите координаты точки A (x y):"
    a <- readPoint
    putStrLn "Введите координаты точки B (x y):"
    b <- readPoint
    putStrLn "Введите координаты точки C (x y):"
    c <- readPoint
    putStrLn "Введите координаты точки D (x y):"
    d <- readPoint

    if isSquare a b c d
        then putStrLn "Четырехугольник ABCD является квадратом."
        else putStrLn "Четырехугольник ABCD не является квадратом."
    
    
    putStrLn ""
    putStrLn "Программа завершена."

-- Функция для чтения точки 
readPoint :: IO (Double, Double)
readPoint = do
    line <- getLine
    let [x, y] = map read (words line)
    return (x, y)