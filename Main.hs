condicion :: Float -> Float
condicion x
  | x == 3 = 100
  | x == 5 = 25
  | otherwise = 0

flotante :: Int -> Float
flotante = int
  where int = fromIntegral

solucion :: String -> Float
solucion = head . foldl funciones [] . words
  where   funciones (x:y:ys) "*" = (x * y):ys
          funciones (x:y:ys) "+" = (x + y):ys
          funciones (x:y:ys) "-" = (y - x):ys
          funciones (x:y:ys) "/" = (y / x):ys
          funciones (x:xs) "neg" = - x:xs
          funciones (x:xs) "raiz" = sqrt x:xs
          funciones (x:xs) "condNumero" = condicion x:xs
          funciones xs "sum" = [sum xs]
          funciones xs "product" = [product xs]
          funciones xs "promedio" = [sum xs/flotante(length xs)]
          funciones xs numberString = read numberString:xs
         
main :: IO ()
main =  do
  print(solucion "10 3 + 2 * 6 - 2 / 6 + raiz neg")
  print(solucion "5 8 3 condNumero sum")
  print(solucion "4 3 5 promedio 4 + 6 product")