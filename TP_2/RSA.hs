module RSA where
import Tipos
import Aritmetica


--(3)
clavesDesde :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer, Integer)
clavesDesde p q e d | mod (e*d) m == 1 = (e, d, n)
                    | mod (e*d) m /= 1 = clavesDesde p q e (d+1)
    where m = (p-1)*(q-1)
          n = p*q
          e = coprimoCon m

claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q = clavesDesde p q e 0
    where m = (p-1)*(q-1)
          e = coprimoCon m

--(6)
codificadorascii :: Clpub -> [Integer] -> Cifrado
codificadorascii clavepub [] = []
codificadorascii clavepub (l:xl) | mcd n l == 1 = (modExp l e n):(codificadorascii clavepub xl)
                                 | otherwise = -l:(codificadorascii clavepub xl)
    where e = fst clavepub
          n = snd clavepub

codificador :: Clpub -> Mensaje -> Cifrado
codificador clavepub mensaje | ascii == [] = []
                             | otherwise = codificadorascii clavepub ascii
    where ascii = aEnteros mensaje

--(7)
decodificadorAux :: Clpri -> [Integer] -> Cifrado 
decodificadorAux clavepri [] = []
decodificadorAux clavepri (l:xl) | l >= 0 = (modExp l d n):(decodificadorAux clavepri xl)
                                 | l < 0 = -l:(decodificadorAux clavepri xl)
    where d = fst clavepri
          n = snd clavepri

decodificador :: Clpri -> Cifrado -> Mensaje
decodificador clavepri l = aChars (decodificadorAux clavepri l)
