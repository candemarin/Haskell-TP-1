module Aritmetica where
import Tipos
import Data.Tuple
import Data.Bits


--(1)
mcd :: Integer -> Integer -> Integer
mcd a b | mod a b == 0 = b
        | otherwise = mcd b (mod a b)

emcd :: (Integer, Integer) -> (Integer, Integer)
emcd (a,0) = (1, 0)
emcd (a,b) = (t, s-q*t)
  where q = div a b 
        (s,t) = emcd (b, (mod a b))

mcdExt :: Integer -> Integer -> (Integer,(Integer,Integer))
mcdExt a b = (mcd a b, (emcd (a,b))) 

--(2)
contarDivisoresDesde :: Integer -> Integer -> Integer
contarDivisoresDesde n k | k == n = 1
                         | mod n k == 0 = 1 + contarDivisoresDesde n (k+1)
                         | otherwise = contarDivisoresDesde n (k+1)

esPrimo :: Integer -> Bool
esPrimo n |n <= 0 = undefined 
          |n == 1 = undefined
          |contarDivisoresDesde n 1 == 2 = True
          |otherwise = False

cribaAux :: Integer -> Set Integer -> Set Integer
cribaAux n a | n == 1 = a
             | n == 0 = a -- [] 
             | esPrimo n = cribaAux (n-1) (agregarCriba n a)
             | otherwise = cribaAux (n-1) a 

agregarCriba :: Integer -> Set Integer -> Set Integer
agregarCriba n a = n:a 

criba :: Integer -> Set Integer
criba n = cribaAux (n-1) []

--(3)
sonCoprimosDesde :: Integer -> Integer -> Integer -> Bool
sonCoprimosDesde n m k | k > n || k > m = True
                       | mod n k == 0 && mod m k == 0 = False
                       | otherwise = sonCoprimosDesde n m (k+1)

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m = sonCoprimosDesde n m 2

coprimoConAux :: Integer -> Integer -> Integer
coprimoConAux n m | m == 1 = undefined
                  | sonCoprimos n m == True = m
                  | otherwise = coprimoConAux n (m-1)

coprimoCon:: Integer -> Integer
coprimoCon n | n <= 2 = undefined
             | n == 3 || n == 4 = undefined
             | otherwise = coprimoConAux n (n-2)


--(4)
inversoMultiplicativoDesde :: Integer -> Integer -> Integer -> Integer
inversoMultiplicativoDesde n m x | sonCoprimos n m == False = undefined
                                 | mod (n*x) m == 1 = x
                                 | otherwise = inversoMultiplicativoDesde n m (x+1)

inversoMultiplicativo:: Integer -> Integer -> Integer
inversoMultiplicativo n m = inversoMultiplicativoDesde n m 1


-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1
