module S (
  startGame,
  bestMove,
  howManyForksHas,
  itIsFull,
  createNewMatrix,
  createNewList,
  findWhere,
  row,
  column,
  haveWinner,
  oneTurnToWin,
  isStreak,
  inRow,
  inColumn,
  inDiagonal,
  firstDiagonal,
  secondDiagonal,
  transposeM
 )where



startGame :: [[Char]] -> Char-> Char ->Char-> (Int,Int)
startGame matrix symbolOne symbolTwo free
 |haveWinner matrix symbolOne symbolTwo =(10,0)
 |haveWinner matrix symbolTwo symbolOne =(0,10)
 |itIsFull matrix free =(10,10)
 |oneTurnToWin matrix symbolOne symbolTwo = findWhere matrix symbolOne symbolTwo
 |oneTurnToWin matrix symbolTwo symbolOne = findWhere matrix symbolTwo symbolOne 
 |otherwise = bestMove matrix symbolOne symbolTwo free
   
-- ------------------------------------------------------------------------------------------------------      

bestMove:: [[Char]] -> Char-> Char-> Char -> (Int,Int)
bestMove matrix symbolOne symbolTwo free
 | not (null firstPriority) = head firstPriority
 | length againstPriority == 2 = head [el| el<-all, (matrix!!(fst el)!!(snd el))==free,not (elem el againstPriority)]
 | length againstPriority == 1 = head againstPriority
 | not (null secondPriority) = head [x|x<-secondPriority]
 | otherwise = head [el| el<-all, (matrix!!(fst el)!!(snd el))==free]
   where
   firstPriority = [el| el<-all, (matrix!!(fst el)!!(snd el))==free,(howManyForksHas (createNewMatrix matrix symbolOne (fst el) (snd el)) symbolOne symbolTwo [inRow,inColumn,inDiagonal])==2]
   againstPriority = [el| el<-all, (matrix!!(fst el)!!(snd el))==free,(howManyForksHas (createNewMatrix matrix symbolTwo (fst el) (snd el)) symbolTwo symbolOne [inRow,inColumn,inDiagonal])==2]
   secondPriority = [el| el<-all, (matrix!!(fst el)!!(snd el))==free,(howManyForksHas (createNewMatrix matrix symbolOne (fst el) (snd el)) symbolOne symbolTwo [inRow,inColumn,inDiagonal])==1]
   all = [(1,1),(0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2)]

howManyForksHas :: [[Char]]->Char->Char->[([[Char]] -> Char-> Char->Int -> Bool)]->Int
howManyForksHas matrix symbolOne symbolTwo [] = 0
howManyForksHas matrix symbolOne symbolTwo searchIn =
 if((head searchIn) matrix symbolOne symbolTwo 2)
 then 1+howManyForksHas matrix symbolOne symbolTwo (tail searchIn)
 else howManyForksHas matrix symbolOne symbolTwo (tail searchIn)
   
-- ------------------------------------------------------------------------------------------------------

itIsFull :: [[Char]] -> Char ->Bool
itIsFull matrix free =
 null [el| el<-freeZones, (matrix!!(fst el)!!(snd el))==free]
 where 
   freeZones = [(1,1),(0,0),(0,2),(2,0),(2,2),(0,1),(1,0),(1,2),(2,1)]

-- ------------------------------------------------------------------------------------------------------
createNewMatrix :: [[Char]] -> Char -> Int->Int->[[Char]]
createNewMatrix [] _ _ _= []        
createNewMatrix (y:ys) s 0 indexY =(createNewList y s indexY):ys 
createNewMatrix (y:ys) s indexX indexY =y:(createNewMatrix ys s (indexX-1) indexY)

createNewList :: [Char]->Char->Int->[Char]
createNewList [] _ _ = []        
createNewList (x:xs) s 0 =s:xs 
createNewList (x:xs) s index =x:(createNewList xs s (index-1))

-- ------------------------------------------------------------------------------------------------------ 

findWhere :: [[Char]] -> Char-> Char -> (Int,Int)
findWhere matrix symbolOne symbolTwo 
 |(inRow matrix symbolOne symbolTwo 2) = 
  (row matrix symbolOne symbolTwo,column matrix symbolOne (row matrix symbolOne symbolTwo))
 |(inColumn matrix symbolOne symbolTwo 2) = 
  (column (transposeM matrix) symbolOne (row (transposeM matrix) symbolOne symbolTwo),row (transposeM matrix) symbolOne symbolTwo)
 |(inDiagonal matrix symbolOne symbolTwo 2) =
  if(isStreak (firstDiagonal matrix) symbolOne symbolTwo 2)
  then head [snd el| el<-((matrix!!0!!0,(0,0)) : (matrix!!1!!1,(1,1)) :[(matrix!!2!!2,(2,2))]),not((fst el)==symbolOne)]
  else head [snd el| el<-((matrix!!0!!2,(0,2)) : (matrix!!1!!1,(1,1)) :[(matrix!!2!!0,(2,0))]),not((fst el)==symbolOne)]

row :: [[Char]] -> Char-> Char -> Int
row matrix symbolOne symbolTwo  = 
 head [(fst y)|y<-(zip [0,1,2] [isStreak x symbolOne symbolTwo 2|x<-matrix] ),(snd y)==True] 

column :: [[Char]] -> Char->Int -> Int
column matrix symbolOne r= 
 head [(fst y)|y<-(zip [0,1,2] (matrix!!r)),not((snd y)==symbolOne)] 
 
-- ------------------------------------------------------------------------------------------------------

oneTurnToWin :: [[Char]] -> Char-> Char -> Bool
oneTurnToWin matrix symbolOne symbolTwo = 
 inRow matrix symbolOne symbolTwo 2||
 inColumn matrix symbolOne symbolTwo 2||
 inDiagonal matrix symbolOne symbolTwo 2
 
haveWinner :: [[Char]] -> Char-> Char ->Bool
haveWinner matrix symbolOne symbolTwo = 
 inRow matrix symbolOne symbolTwo 3||
 inColumn matrix symbolOne symbolTwo 3||
 inDiagonal matrix symbolOne symbolTwo 3 
 
-- ------------------------------------------------------------------------------------------------------


isStreak :: [Char] -> Char -> Char->Int -> Bool
isStreak xs symbolOne symbolTwo value= not(elem symbolTwo xs) && length[x|x<-xs,x == symbolOne] == value


-- ------------------------------------------------------------------------------------------------------
inRow :: [[Char]] -> Char-> Char->Int -> Bool
inRow matrix symbolOne symbolTwo value= foldr 
  (||) False [isStreak x symbolOne symbolTwo value|x<-matrix]

inColumn :: [[Char]] -> Char-> Char ->Int-> Bool
inColumn matrix symbolOne symbolTwo value= 
  foldr (||) False [isStreak x symbolOne symbolTwo value|x<-(transposeM matrix)]

inDiagonal :: [[Char]] -> Char-> Char ->Int-> Bool
inDiagonal matrix symbolOne symbolTwo value=
  isStreak (firstDiagonal matrix) symbolOne symbolTwo value|| 
  isStreak (secondDiagonal matrix) symbolOne symbolTwo value
-- ---------------
firstDiagonal :: [[Char]] -> [Char]
firstDiagonal matrix = matrix!!0!!0 : matrix!!1!!1 :[matrix!!2!!2]

secondDiagonal :: [[Char]] -> [Char]
secondDiagonal matrix = matrix!!0!!2 : matrix!!1!!1 :[matrix!!2!!0]
-- ------------------------------------------------------------------------------------------------------
transposeM :: [[Char]] -> [[Char]]
transposeM [] = []
transposeM m = if (length (head m)) == 1 then [(map head m)] else (map head m) : transposeM (map tail m)

