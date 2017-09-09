-- leetcode 657: judge route circle
{-
Initially, there is a Robot at position (0, 0). Given a sequence of its moves, judge if this robot makes a circle, which means it moves back to the original place.

The move sequence is represented by a string. And each move is represent by a character. The valid robot moves are R (Right), L (Left), U (Up) and D (down). The output should be true or false representing whether the robot makes a circle. 
-}
data Offset a = Coord a a
judgeCircle :: [Char] -> Bool
judgeCircle moves =
  let
    aux :: [Char] -> Offset Integer -> Bool
    aux [] (Coord x y) = (x == 0) && (y == 0)
    aux moves@(cur:rest) (Coord x y) =
      case cur of
        'L' -> aux rest (Coord (x+1) y)
        'R' -> aux rest (Coord (x-1) y)
        'U' -> aux rest (Coord x (y+1))
        _ -> aux rest (Coord x (y-1))
  in
    aux moves (Coord 0 0)
