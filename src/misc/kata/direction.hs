

data DirectionType = STRAIGHT | LEFT | RIGHT deriving Show
data Point = Point { x :: Int, y :: Int } deriving Show
data Direction = Direction { directionType :: DirectionType  } deriving Show

isLeft :: Point -> Point -> Point -> Bool
isLeft a b c = x(a) == x(b) && ( x(a) - 1 == x(c) )

isRight :: Point -> Point -> Point -> Bool
isRight a b c = x(a) == x(b) && ( x(a) + 1 == x(c) )

isStraight :: Point -> Point -> Point -> Bool
isStraight a b c = x(a) == x(b) && x(b) == x(c)

direction :: Point -> Point -> Point -> Direction
direction a b c 
    | isLeft a b c = Direction LEFT
    | isRight a b c = Direction RIGHT
    | isStraight a b c = Direction STRAIGHT


