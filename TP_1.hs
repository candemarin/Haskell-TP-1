esUnCubo :: Integer -> Bool
esUnCubo x = (round (fromIntegral x**(1/3)))^3 == x

-- 1
esSumaDeDosCubosDesde :: Integer -> Integer -> Bool
esSumaDeDosCubosDesde n a | a > n = False
                          | esUnCubo a && esUnCubo (n - a) = True
                          | otherwise = esSumaDeDosCubosDesde n (a+1)
                        
esSumaDeDosCubos :: Integer -> Bool
esSumaDeDosCubos n = esSumaDeDosCubosDesde n 1

-- en la 2da condicion de esSumaDeDosCubosDesde se fija si a y (n-a) son cubos y en ese caso significa que la a + otro cubo = n entonces si es suma de 2 cubos
-- si alguna de las partes no es un cubo le suma 1 a a y empieza la recursion hasta que llegue a la 1ra condicion que si a > n no es suma de cubos 
-- luego en esSumaDeDosCubos le decimos que evalue desde a = 1 asi pasa por todos los casos posibles hasta a>n o llegar a True

--2
descomposicionCubosDesde:: Integer->Integer-> (Integer,Integer)
descomposicionCubosDesde n a | esSumaDeDosCubos n == False = undefined 
                             | esUnCubo(n - (a^3)) = (a,(round (fromIntegral (n-a)**(1/3)))) 
                             | otherwise = descomposicionCubosDesde n (a+1)

descomposicionCubos :: Integer -> (Integer,Integer)
descomposicionCubos n = descomposicionCubosDesde n 1

-- si n no es suma 2 cubos = undefined ya que no hay cubos en los cuales descomponerla
-- si (n-a^3) es un cubo entonces los cubos que formarian a n serian a y el numero sin elevar al cubo de n-a
-- si (n-a^3) no es un cubo entonces le sumamos 1 a a hasta que lo sea y se devuelva los valores indicados en la condicion anterior
-- finalmente en descomposicionCubos le indicamos que con un n dada haga descomposicionCubosDesde a=1 asi recorre todas las opciones posibles aunque devolvera la 1era que encuentre, cortando el programa

--3
cantidadDeFormasDesde :: Integer -> Integer -> Integer
cantidadDeFormasDesde n a | a^3 > n = 0
                          | esUnCubo (n - (a^3)) = 1 + cantidadDeFormasDesde n (a+1)
                          | otherwise = cantidadDeFormasDesde n (a+1)

cantidadDeFormas :: Integer -> Integer
cantidadDeFormas n = div (cantidadDeFormasDesde n 1) 2

-- en la segunda condicion de cantidadDeFormasDesde decimos que si (n-a^3) es un cubo entonces hay por lo menos una forma de que n sea suma de cubos y luego
-- le sumamos la misma funcion pero desde el a cubo + 1 hasta que encuentre nuevamente que (n-a^3) es un cubo sumando nuevamente 1 y repitiendo la recursion
-- si (n-a^3) no es un cubo le sumara 1 a a hasta que encuantre un a que cumpla que la 2da condicion o llegue a que a^3 > n entonces sume 0 y corte el programa
-- por ultimo definimos cantidadDeFormas como cantidadDeFormasDesde a=1 para empezar recorrer todos los casos^, desde 1 hasta a^3 > n
-- y lo dividimos por 2 ya que, debido a la propiedad conmutativa de la suma, cuenta cada forma de escribirlo dos veces,
-- por ej: n=9 devolveria (1,2) y (2,1) ya que 1^3 + 2^3 = 9 y tambien 2^3 + 1^3 = 9 pero enrealidad son el mismo par de numeros