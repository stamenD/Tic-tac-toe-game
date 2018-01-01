import S
    
printMatrix :: [[Char]] -> IO()
printMatrix matrix= do
 putStrLn (matrix!!0)
 putStrLn (matrix!!1)
 putStrLn (matrix!!2)
 
validMove :: [[Char]] ->Int ->Int-> Bool
validMove matrix x  y = 
 if elem x [0,1,2] && elem x [0,1,2] && (matrix!!x!!y)=='.'
 then True
 else False
 
start :: [[Char]] -> IO()
start matrix = do
 printMatrix matrix
 if elem (startGame matrix 'x' 'o' '.') [(10,0),(0,10),(10,10)]
 then  putStrLn "End"
 else do
  putStrLn ""
  putStrLn ""
  -- putStrLn "x="
  inputjar <- getLine
  let x = read inputjar :: Int
  -- putStrLn "y="
  inputjar <- getLine
  let y = read inputjar :: Int
  if validMove matrix x y
  then do
   let firstMove = (createNewMatrix matrix 'o' x y)
   printMatrix firstMove
   putStrLn ""
   start (createNewMatrix firstMove 'x' (fst (startGame firstMove 'x' 'o' '.')) (snd(startGame firstMove 'x' 'o' '.')))
  else
   start matrix
     

main = do
 start ["...", "...", "..."]

