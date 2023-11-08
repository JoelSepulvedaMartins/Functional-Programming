module Main where

dobra :: Int -> Int
dobra x = x + x

farenheit :: Float -> Float
farenheit c = 32+c/5*9

price :: Double -> Double
price v
  |v <= 10 = 2*v
  |v <= 20  = 2*v+2
  |otherwise = v+1

next :: Int -> Int
next n
  |n `mod` 2 == 0 = n `div` 2
  |otherwise      = 3*n+1

numbers :: Int -> [Int]
numbers n
     |n==1      = [1]
     |otherwise = n : numbers(next n)

main :: IO()
main = do
  print "Destinatario do e-mail?"
  recipient <- getLine
  print "Como me refiro a ele?"
  title <- getLine
  print "Quem esta enviando?"
  author <- getLine
  print ("Prezado " ++ recipient ++ ", " ++
    "Obrigado por comprar nosso curso." ++ title ++ " obrigado! " ++ author )
 
  print ("vinte e cinco graus em farenheit: " ++ show
   (farenheit 25))    
  --este é um comentário.
  {- 
  Comentário em bloco
  -}
  print $ "Trinta e cinco graus em farenheit: " ++ show
   (farenheit 35)
  
  print (price 5)
  print (price 11)
  print (price 30)

  print "Digite um valor em graus celsius?"
  graus <- getLine
  print (farenheit (read graus))

  print (numbers 10)
