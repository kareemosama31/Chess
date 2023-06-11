---Libraries:-
import Data.Char
---Variabels:-
type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Project Methods:-
setBoard :: Board
setBoard = (White, [R ('h',1),N ('g',1),B ('f',1),K ('e',1),
		Q ('d',1),B ('b',1),N ('b',1),R ('a',1),
		P ('h',2),P ('g',2),P ('f',2),P ('e',2),
		P ('d',2),P ('c',2),P ('b',2),P ('a',2)],
		[R ('h',8),N ('g',8),B ('f',8),K ('e',8),
		Q ('d',8),B ('c',8),N ('b',8),R ('a',8),
		P ('h',7),P ('g',7),P ('f',7),P ('e',7),
		P ('d',7),P ('c',7),P ('b',7),P ('a',7)])
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------		
visualizeBoard:: Board->String 
visualizeBoard (player,w,b) =unlines[
		 "     a  b  c  d  e  f  g  h"
		,"8 |"++set w b ('a',8)++"|"++set w b ('b',8)++"|"++set w b ('c',8)++"|"++set w b ('d',8)++"|"++set w b ('e',8)++"|"++set w b ('f',8)++"|"++set w b ('g',8)++"|"++set w b ('h',8)++"|"
	    ,"7 |"++set w b ('a',7)++"|"++set w b ('b',7)++"|"++set w b ('c',7)++"|"++set w b ('d',7)++"|"++set w b ('e',7)++"|"++set w b ('f',7)++"|"++set w b ('g',7)++"|"++set w b ('h',7)++"|"
		,"6 |"++set w b ('a',6)++"|"++set w b ('b',6)++"|"++set w b ('c',6)++"|"++set w b ('d',6)++"|"++set w b ('e',6)++"|"++set w b ('f',6)++"|"++set w b ('g',6)++"|"++set w b ('h',6)++"|"
		,"5 |"++set w b ('a',5)++"|"++set w b ('b',5)++"|"++set w b ('c',5)++"|"++set w b ('d',5)++"|"++set w b ('e',5)++"|"++set w b ('f',5)++"|"++set w b ('g',5)++"|"++set w b ('h',5)++"|"
		,"4 |"++set w b ('a',4)++"|"++set w b ('b',4)++"|"++set w b ('c',4)++"|"++set w b ('d',4)++"|"++set w b ('e',4)++"|"++set w b ('f',4)++"|"++set w b ('g',4)++"|"++set w b ('h',4)++"|"
		,"3 |"++set w b ('a',3)++"|"++set w b ('b',3)++"|"++set w b ('c',3)++"|"++set w b ('d',3)++"|"++set w b ('e',3)++"|"++set w b ('f',3)++"|"++set w b ('g',3)++"|"++set w b ('h',3)++"|"
		,"2 |"++set w b ('a',2)++"|"++set w b ('b',2)++"|"++set w b ('c',2)++"|"++set w b ('d',2)++"|"++set w b ('e',2)++"|"++set w b ('f',2)++"|"++set w b ('g',2)++"|"++set w b ('h',2)++"|"
		,"1 |"++set w b ('a',1)++"|"++set w b ('b',1)++"|"++set w b ('c',1)++"|"++set w b ('d',1)++"|"++set w b ('e',1)++"|"++set w b ('f',1)++"|"++set w b ('g',1)++"|"++set w b ('h',1)++"|"
		,""
		,"Turn:"++show(player)
		]
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	
isLegal:: Piece -> Board -> Location -> Bool
isLegal p (_,w,b) l= if (elem p w && getPieceType p=="P") then (wpawn p l && locEmpty w l && locEmpty b l)
					 else if (elem p b && getPieceType p=="P") then (bpawn p l && locEmpty w l && locEmpty b l)
					 else if (getPieceType p=="R") then (rook p l && locEmpty w l && locEmpty b l)
					 else if (getPieceType p=="Q") then (queen p l && locEmpty w l && locEmpty b l)
					 else if (getPieceType p=="N") then (knight p l && locEmpty w l && locEmpty b l)
					 else if (getPieceType p=="B") then (bishop p l && locEmpty w l && locEmpty b l)
					 else (king p l && locEmpty w l && locEmpty b l)
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					 
suggestMove:: Piece -> Board -> [Location]
suggestMove p (player,w,b) = let (x,y)= getLocation p in if (elem p w && getPieceType p=="P") then (sugWpawn p (player,w,b))
					 else if (elem p b && getPieceType p=="P") then (sugBpawn p (player,w,b))
					 else if (getPieceType p=="R") then (sugRook p (player,w,b))
					 else if (getPieceType p=="Q") then (sugQueen p (player,w,b))
					 else if (getPieceType p=="N") then (sugNight p (player,w,b))
					 else if (getPieceType p=="B") then (sugBishop p (player,w,b))
					 else (sugKing p (player,w,b))			
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
move :: Piece -> Location -> Board -> Board
move p (c, i) (player, w, b)
  | player == White && p `elem` b = error "This is the other player's turn. You can't move."
  | player == Black && p `elem` w = error "This is the other player's turn. You can't move."
  | not (isLegal p (player, w, b) (c, i)) = error ("Illegal move for piece " ++ show p)
  | elem p w =  (Black, updateList p w (c, i), b)
  | otherwise =  (White, w, updateList p b (c, i))					 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Helper Methods:-		
getPieceType :: Piece -> String
getPieceType (P _) = "P"
getPieceType (N _) = "N"
getPieceType (K _) = "K"
getPieceType (Q _) = "Q"
getPieceType (R _) = "R"
getPieceType (B _) = "B"
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
getLocation :: Piece -> Location
getLocation (P loc) = loc
getLocation (N loc) = loc
getLocation (K loc) = loc
getLocation (Q loc) = loc
getLocation (R loc) = loc
getLocation (B loc) = loc
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set ::[Piece]->[Piece]->Location->String
set [] [] l =" "
set (hw:w) (hb:b) l = 
   if getLocation hw == l then (getPieceType hw)++"W" 
   else if getLocation hb == l then (getPieceType  hb)++"B"
   else set w b l
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   
incrementLetter2 :: Char -> Char
incrementLetter2 c = chr (ord c + 2)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
deLetter2 :: Char -> Char
deLetter2 c =chr (ord c - 2)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
incrementLetter :: Char -> Char
incrementLetter c = chr (ord c + 1)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
deLetter :: Char -> Char
deLetter c =chr (ord c - 1)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
diagonalxy :: Location->Location->Bool
diagonalxy ('h',_) (_,_)= False
diagonalxy (_,8) (_,_)=False
diagonalxy (x,y) (x1,y1)= if (incrementLetter x==x1 && y1==y+1) then True 
						  else  diagonalxy (incrementLetter x,y+1) (x1,y1)
diagonalxY :: Location->Location->Bool
diagonalxY ('h',_) (_,_)= False
diagonalxY (_,1) (_,_)=False
diagonalxY (x,y) (x1,y1)= if (incrementLetter x==x1 && y1==y-1) then True 
						  else  diagonalxY (incrementLetter x,y-1) (x1,y1)
diagonalXY :: Location->Location->Bool
diagonalXY ('a',_) (_,_)= False
diagonalXY (_,1) (_,_)=False
diagonalXY (x,y) (x1,y1)= if (deLetter x==x1 && y1==y-1) then True 
						  else  diagonalXY (deLetter x,y-1) (x1,y1)
diagonalXy :: Location->Location->Bool
diagonalXy ('a',_) (_,_)= False
diagonalXy(_,8) (_,_)=False
diagonalXy (x,y) (x1,y1)= if (deLetter x==x1 && y1==y+1) then True 
						  else  diagonalXy (deLetter x,y+1) (x1,y1)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------						  
wpawn :: Piece -> Location -> Bool
wpawn p (x,y)=  let (x1,y1)=getLocation p in if (y1==2 &&(x1,y1+2)==(x,y)) || (y1==2 &&(x1,y1+1)==(x,y)) then  True
			   else False
wpawn p (x,y)=  let (x1,y1)=getLocation p in if (x1,y1+1)==(x,y) then  True
			   else False
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	   
bpawn :: Piece -> Location -> Bool
bpawn p (x,y)=  let (x1,y1)=getLocation p in if (y1==7 &&(x1,y1-2)==(x,y))|| (y1==2 &&(x1,y1-1)==(x,y)) then  True
			   else False
bpawn p (x,y)=  let (x1,y1)=getLocation p in if (x1,y1-1)==(x,y) then  True
			   else False
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------			   
rook :: Piece -> Location -> Bool
rook p (x,y)= let (x1,y1)=getLocation p in if (x1==x && (1<=y1 && y1<=8) && y1/=y && (1<=y && y<=8)) then True
               else if (y1==y && ('a'<=x1 && x1<='h') && x1/= x && ('a'<=x && x<='h')) then True
						   else False
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
bishop :: Piece -> Location -> Bool
bishop p (x,y) = let (x1,y1)=getLocation p in (diagonalxy (x1,y1) (x,y) || diagonalxY (x1,y1) (x,y) || diagonalXy (x1,y1) (x,y) || diagonalXY (x1,y1) (x,y))
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
queen :: Piece -> Location -> Bool
queen p (x,y)=(rook p (x,y) || bishop p (x,y))
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knightF :: Location -> Location -> Bool
knightF (_,7)(_,_)=False
knightF (_,8)(_,_)=False
knightF (x1,y1) (x,y)=if ( x1/='h' && incrementLetter x1==x && y1+2==y)||( x1/='a' && deLetter x1==x && y1+2==y) then True 	else False
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knightB :: Location -> Location -> Bool
knightB (_,1)(_,_)=False
knightB (_,2)(_,_)=False
knightB (x1,y1) (x,y)=if ( x1/='h' && incrementLetter x1==x && y1-2==y)||(x1/='a' && deLetter x1==x && y1-2==y) then True else False
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knightL :: Location -> Location -> Bool
knightL ('a',_)(_,_)=False
knightL ('b',_)(_,_)=False
knightL (x1,y1) (x,y)=if ( y1/=1 && deLetter2 x1==x && y1-1==y)||(y1/=8 && deLetter2 x1==x && y1+1==y) then True else False
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knightR :: Location -> Location -> Bool
knightR ('g',_)(_,_)=False
knightR ('h',_)(_,_)=False
knightR (x1,y1) (x,y)=if ( y1/=1 && incrementLetter2 x1==x && y1-1==y)||(y1/=8 && incrementLetter2 x1==x && y1+1==y) then True else False
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knight :: Piece -> Location -> Bool
knight p (x,y)= let (x1,y1)=getLocation p in (knightF (x1,y1) (x,y) || knightB (x1,y1) (x,y) || knightR (x1,y1) (x,y) || knightL (x1,y1) (x,y))
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kingU :: Location -> Location -> Bool
kingU (_,8) (_,_)=False
kingU (x1,y1) (x,y)=if(y1+1==y)then True else False
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kingD :: Location-> Location -> Bool
kingD (_,1) (_,_)=False
kingD (x1,y1) (x,y)=if(y1-1==y)then True else False
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kingL :: Location-> Location -> Bool
kingL ('a',_) (_,_)=False
kingL (x1,y1) (x,y)=if(deLetter x1==x)then True else False
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kingR :: Location-> Location -> Bool
kingR ('h',_) (_,_)=False
kingR (x1,y1) (x,y)=if(incrementLetter x1==x)then True else False
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kingUL :: Location-> Location -> Bool
kingUL (_,8) (_,_)=False
kingUL ('a',_) (_,_)=False
kingUL (x1,y1) (x,y)=if(deLetter x1==x && y1+1==y)then True else False
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kingUR :: Location-> Location -> Bool
kingUR (_,8) (_,_)=False
kingUR ('h',_) (_,_)=False
kingUR (x1,y1) (x,y)=if(incrementLetter x1==x && y1+1==y)then True else False
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kingDL :: Location-> Location -> Bool
kingDL (_,1) (_,_)=False
kingDL ('a',_) (_,_)=False
kingDL (x1,y1) (x,y)=if(deLetter x1==x && y1-1==y)then True else False
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kingDR :: Location-> Location -> Bool
kingDR (_,1) (_,_)=False
kingDR ('h',_) (_,_)=False
kingDR (x1,y1) (x,y)=if(incrementLetter x1==x && y1-1==y)then True else False
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
king :: Piece -> Location -> Bool
king p (x,y)= let (x1,y1)=getLocation p in(kingL (x1,y1) (x,y) || kingR (x1,y1) (x,y) ||kingU (x1,y1) (x,y) ||kingD (x1,y1) (x,y) ||kingUL (x1,y1) (x,y) ||kingUR (x1,y1) (x,y) ||kingDL (x1,y1) (x,y) ||kingDR (x1,y1) (x,y) )
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
locEmpty::[Piece]->Location->Bool
locEmpty [] l=True
locEmpty (w:wt) l= let (x1,y1)=getLocation w in if((x1,y1)==l)then False else locEmpty wt l
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sugWpawn :: Piece->Board->[Location]
sugWpawn p (_,w,b)= let (x,y)=getLocation p in if ((locEmpty w (x,y+1))&&(locEmpty b (x,y+1)) && (locEmpty w (x,y+2))&&(locEmpty b (x,y+2)) && y==2) then [(x,y+1),(x,y+2)]
else sugWpawn2 p (x,w,b)
sugWpawn2 p (_,w,b)=let (x,y)=getLocation p in if (locEmpty w (x,y+1) && locEmpty b (x,y+1))  then [(x,y+1)]
else[]
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sugBpawn :: Piece->Board->[Location]
sugBpawn p (_,w,b)= let (x,y)=getLocation p in if ((locEmpty w (x,y-1))&&(locEmpty b (x,y-1)) && (locEmpty w (x,y-2))&&(locEmpty b (x,y-2)) && y==7) then [(x,y-1),(x,y-2)]
else sugBpawn2 p (x,w,b)
sugBpawn2 p (_,w,b)=let (x,y)=getLocation p in if (locEmpty w (x,y-1))&&(locEmpty b (x,y-1)) then [(x,y-1)]
else[]
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sugRookVU:: Piece->Location->Board->[Location]
sugRookVU p (c,8) (x,w,b)=[]
sugRookVU p (c,i) (x,w,b)=if (locEmpty w (c,i+1) && locEmpty b (c,i+1)) then (c,i+1):sugRookVU p (c,i+1) (x,w,b) 
else []
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sugRookVD:: Piece->Location->Board->[Location]
sugRookVD p (c,1) (x,w,b)=[]
sugRookVD p (c,i) (x,w,b)=if (locEmpty w (c,i-1) && locEmpty b (c,i-1)) then (c,i-1):sugRookVD p (c,i-1) (x,w,b) 
else []
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sugRookHR:: Piece->Location->Board->[Location]
sugRookHR p ('h',_) (x,w,b)=[]
sugRookHR p (c,i) (x,w,b)=if (locEmpty w (incrementLetter c,i) && locEmpty b (incrementLetter c,i)) then (incrementLetter c,i):sugRookHR p (incrementLetter c,i) (x,w,b) 
else []
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sugRookHL:: Piece->Location->Board->[Location]
sugRookHL p ('a',_) (x,w,b)=[]
sugRookHL p (c,i) (x,w,b)=if (locEmpty w (deLetter c,i) && locEmpty b (deLetter c,i)) then (deLetter c,i):sugRookHL p (deLetter c,i) (x,w,b) 
else []
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sugRook :: Piece -> Board -> [Location]
sugRook p (c, w, b) =
  let (x, y) = getLocation p
   in sugRookVU p (x, y) (c, w, b) ++ sugRookVD p (x, y) (c, w, b) ++ sugRookHL p (x, y) (c, w, b) ++ sugRookHR p (x, y) (c, w, b)

sugBishopxy :: Piece->Location->Board->[Location]
sugBishopxy p ('h',_) (_,_,_)=[]
sugBishopxy p (_,8) (_,_,_)=[]
sugBishopxy p (c,i) (x,w,b)=if( locEmpty w (incrementLetter c,i+1) &&  locEmpty b (incrementLetter c,i+1)) then (incrementLetter c,i+1) :sugBishopxy p (incrementLetter c,i+1) (x,w,b)
else []


sugBishopXY :: Piece->Location->Board->[Location]
sugBishopXY p ('a',_) (_,_,_)=[]
sugBishopXY p (_,1) (_,_,_)=[]
sugBishopXY p (c,i) (x,w,b)=if( locEmpty w (deLetter c,i-1) &&  locEmpty b (deLetter c,i-1)) then (deLetter c,i-1) :sugBishopXY p (deLetter c,i-1) (x,w,b)
else []

sugBishopxY :: Piece->Location->Board->[Location]
sugBishopxY p ('h',_) (_,_,_)=[]
sugBishopxY p (_,1) (_,_,_)=[]
sugBishopxY p (c,i) (x,w,b)=if( locEmpty w (incrementLetter c,i-1) &&  locEmpty b (incrementLetter c,i-1)) then (incrementLetter c,i-1) :sugBishopxY p (incrementLetter c,i-1) (x,w,b)
else []

sugBishopXy :: Piece->Location->Board->[Location]
sugBishopXy p ('a',_) (_,_,_)=[]
sugBishopXy p (_,8) (_,_,_)=[]
sugBishopXy p (c,i) (x,w,b)=if( locEmpty w (deLetter c,i+1) &&  locEmpty b (deLetter c,i+1)) then (deLetter c,i+1) :sugBishopXy p (deLetter c,i+1) (x,w,b)
else []

sugBishop :: Piece-> Board ->[Location]
sugBishop p (c, w, b)=let (x, y) = getLocation p in sugBishopxy p (x,y) (c,w,b) ++ sugBishopXY p (x,y) (c,w,b) ++ sugBishopxY p (x,y) (c,w,b) ++ sugBishopXy p (x,y) (c,w,b)

sugQueen :: Piece-> Board ->[Location]
sugQueen p (c,w,b) =let (x, y) = getLocation p in sugBishopxy p (x,y) (c,w,b) ++ sugBishopXY p (x,y) (c,w,b) ++ sugBishopxY p (x,y) (c,w,b) ++ sugBishopXy p (x,y) (c,w,b) ++ sugRookVU p (x, y) (c, w, b) ++ sugRookVD p (x, y) (c, w, b) ++ sugRookHL p (x, y) (c, w, b) ++ sugRookHR p (x, y) (c, w, b)

sugKingU :: Piece->Location->Board->[Location]
sugKingU _ (_,8) (_,_,_)=[]
sugKingU p (c,i) (x,w,b) =if( locEmpty w (c,i+1) && locEmpty b (c,i+1)) then [(c,i+1)] 
else []

sugKingD :: Piece->Location->Board->[Location]
sugKingD _ (_,1) (_,_,_)=[]
sugKingD p (c,i) (x,w,b) =if( locEmpty w (c,i-1) && locEmpty b (c,i-1)) then [(c,i-1)] 
else []

sugKingR :: Piece->Location->Board->[Location]
sugKingR _ ('h',_) (_,_,_)=[]
sugKingR p (c,i) (x,w,b) =if( locEmpty w (incrementLetter c,i) && locEmpty b (incrementLetter c,i)) then [(incrementLetter c,i)] 
else []

sugKingL :: Piece->Location->Board->[Location]
sugKingL _ ('a',_) (_,_,_)=[]
sugKingL p (c,i) (x,w,b) =if( locEmpty w (deLetter c,i) && locEmpty b (deLetter c,i)) then [(deLetter c,i)] 
else []

sugKingRU :: Piece->Location->Board->[Location]
sugKingRU _ ('h',_) (_,_,_)=[]
sugKingRU _ (_,8) (_,_,_)=[]
sugKingRU p (c,i) (x,w,b) =if( locEmpty w (incrementLetter c,i+1) && locEmpty b (incrementLetter c,i+1)) then [(incrementLetter c,i+1)] 
else []

sugKingRD :: Piece->Location->Board->[Location]
sugKingRD _ ('h',_) (_,_,_)=[]
sugKingRD _ (_,1) (_,_,_)=[]
sugKingRD p (c,i) (x,w,b) =if( locEmpty w (incrementLetter c,i-1) && locEmpty b (incrementLetter c,i-1)) then [(incrementLetter c,i-1)] 
else []

sugKingLU :: Piece->Location->Board->[Location]
sugKingLU _ ('a',_) (_,_,_)=[]
sugKingLU _ (_,8) (_,_,_)=[]
sugKingLU p (c,i) (x,w,b) =if( locEmpty w (deLetter c,i+1) && locEmpty b (deLetter c,i+1)) then [(deLetter c,i+1)] 
else []

sugKingLD :: Piece->Location->Board->[Location]
sugKingLD _ ('a',_) (_,_,_)=[]
sugKingLD _ (_,1) (_,_,_)=[]
sugKingLD p (c,i) (x,w,b) =if( locEmpty w (deLetter c,i-1) && locEmpty b (deLetter c,i-1)) then [(deLetter c,i-1)] 
else []

sugKing :: Piece-> Board ->[Location]
sugKing p (c,w,b)=let (x, y) = getLocation p in sugKingU p (x,y) (c,w,b) ++ sugKingD p (x,y) (c,w,b) ++sugKingR p (x,y) (c,w,b) ++ sugKingL p (x,y) (c,w,b) ++ sugKingLD p (x,y) (c,w,b) ++ sugKingLU p (x,y) (c,w,b) ++ sugKingRD p (x,y) (c,w,b) ++ sugKingRU p (x,y) (c,w,b)


sugNightFR :: Piece->Location->Board->[Location]
sugNightFR p (_,7) (_,_,_)=[]
sugNightFR p (_,8) (_,_,_)=[]
sugNightFR p ('h',_) (_,_,_)=[]
sugNightFR p (c,i) (x,w,b)=if (locEmpty w (incrementLetter c,i+2) && locEmpty b (incrementLetter c,i+2)) then [(incrementLetter c,i+2)]
else []

sugNightFL :: Piece->Location->Board->[Location]
sugNightFL p (_,7) (_,_,_)=[]
sugNightFL p (_,8) (_,_,_)=[]
sugNightFL p ('a',_) (_,_,_)=[]
sugNightFL p (c,i) (x,w,b)=if (locEmpty w (deLetter c,i+2) && locEmpty b (deLetter c,i+2)) then [(deLetter c,i+2)]
else []

sugNightDL :: Piece->Location->Board->[Location]
sugNightDL p (_,1) (_,_,_)=[]
sugNightDL p (_,2) (_,_,_)=[]
sugNightDL p ('a',_) (_,_,_)=[]
sugNightDL p (c,i) (x,w,b)=if (locEmpty w (deLetter c,i-2) && locEmpty b (deLetter c,i-2)) then [(deLetter c,i-2)]
else []

sugNightDR :: Piece->Location->Board->[Location]
sugNightDR p (_,1) (_,_,_)=[]
sugNightDR p (_,2) (_,_,_)=[]
sugNightDR p ('h',_) (_,_,_)=[]
sugNightDR p (c,i) (x,w,b)=if (locEmpty w (incrementLetter c,i-2) && locEmpty b (incrementLetter c,i-2)) then [(incrementLetter c,i-2)]
else []

sugNightLU :: Piece->Location->Board->[Location]
sugNightLU p ('a',_) (_,_,_)=[]
sugNightLU p ('b',_) (_,_,_)=[]
sugNightLU p (_,8) (_,_,_)=[]
sugNightLU p (c,i) (x,w,b)=if (locEmpty w (deLetter2 c,i+1) && locEmpty b (deLetter2 c,i+1)) then [(deLetter2 c,i+1)]
else []

sugNightLD :: Piece->Location->Board->[Location]
sugNightLD p ('a',_) (_,_,_)=[]
sugNightLD p ('b',_) (_,_,_)=[]
sugNightLD p (_,1) (_,_,_)=[]
sugNightLD p (c,i) (x,w,b)=if (locEmpty w (deLetter2 c,i-1) && locEmpty b (deLetter2 c,i-1)) then [(deLetter2 c,i-1)]
else []

sugNightRD :: Piece->Location->Board->[Location]
sugNightRD p ('g',_) (_,_,_)=[]
sugNightRD p ('h',_) (_,_,_)=[]
sugNightRD p (_,1) (_,_,_)=[]
sugNightRD p (c,i) (x,w,b)=if (locEmpty w (incrementLetter2 c,i-1) && locEmpty b (incrementLetter2 c,i-1)) then [(incrementLetter2 c,i-1)]
else []

sugNightRU :: Piece->Location->Board->[Location]
sugNightRU p ('g',_) (_,_,_)=[]
sugNightRU p ('h',_) (_,_,_)=[]
sugNightRU p (_,8) (_,_,_)=[]
sugNightRU p (c,i) (x,w,b)=if (locEmpty w (incrementLetter2 c,i+1) && locEmpty b (incrementLetter2 c,i+1)) then [(incrementLetter2 c,i+1)]
else []

sugNight:: Piece-> Board ->[Location]
sugNight p (c,w,b)=let (x, y) = getLocation p in sugNightFR p (x, y) (c,w,b) ++  sugNightFL p (x, y) (c,w,b) ++ sugNightDL p (x, y) (c,w,b) ++ sugNightDR p (x, y) (c,w,b) ++ sugNightLD p (x, y) (c,w,b) ++ sugNightLU p (x, y) (c,w,b) ++ sugNightRD p (x, y) (c,w,b) ++ sugNightRU p (x, y) (c,w,b) 

updateLocation :: Piece -> Location -> Piece
updateLocation (P _) loc = P loc
updateLocation (N _) loc = N loc
updateLocation (K _) loc = K loc
updateLocation (Q _) loc = Q loc
updateLocation (R _) loc = R loc
updateLocation (B _) loc = B loc

updateList::Piece->[Piece]->Location->[Piece]
updateList p  [] (c,i)=[]
updateList p  (x:xs) (c,i)=if p==x then updateLocation p (c,i):xs else x:updateList p (xs) (c,i)