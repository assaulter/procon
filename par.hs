import Data.Array
import Debug.Trace

-- nをm分割した際の総数を返す関数
part :: Int -> Int -> Int
part num divider = 
  -- num,dividerの範囲の配列。i,jはインデックス
  let dp = array ((0,0),(num,divider))[((i,j),func i j) | i<-[0..num], j<-[0..divider]]
      func i j
	| i == 0 && j == 0  = trace("(i=j=0, i,j)" ++ show(i,j)) (1)
	| i > 0 && j == 0  = trace("(i>0,j=0, i,j)" ++ show(i,j)) (0)
	| i < j = trace("(i<j), dp!(i,j-1)" ++ show(i,j,dp!(i,j-1))) (dp!(i,j-1))
	| otherwise = trace("(i>=j, dp...)" ++ show(i,j,dp!(i,j-1) + dp!(i-j,j))) (dp!(i,j-1) + dp!(i-j,j))
  in dp!(num,divider)

-- 漸化式を解く関数
solver :: Int -> Int -> Int
solver num divider
	| i == 0 && j == 0  = trace("(i=j=0, i,j)" ++ show(i,j)) (1)
	| i > 0 && j == 0  = trace("(i>0,j=0, i,j)" ++ show(i,j)) (0)
	| i < j = trace("(i<j), dp!(i,j-1)" ++ show(i,j,dp!(i,j-1))) (dp!(i,j-1))
	| otherwise = trace("(i>=j, dp...)" ++ show(i,j,dp!(i,j-1) + dp!(i-j,j))) (dp!(i,j-1) + dp!(i-j,j))