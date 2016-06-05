module Direction
    (
      calculateDirection
    , Point(..)
    , Direction(..)
    , successiveTripleDirections
    , grahamScanAlgo
    , getLowestPoint
    , getSecondPoint
    , buildHull
    ) where

-- For testing with QuckCheck on direction data types
import Test.QuickCheck.Arbitrary
instance Arbitrary Point where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Point x y

-- Completed using help from comments and Wikipedia
data Point      = Point Float Float
  deriving (Show, Eq)
data Direction  = MyLeft | MyRight | MyStraight
  deriving (Show, Eq)

calculateDirection :: Point -> Point -> Point -> Maybe Direction
calculateDirection a b c
  | a == b || a == c || b == c  = Nothing -- No angle exists
  | result == 0  = Just MyStraight
  | result < 0   = Just MyRight
  | result > 0   = Just MyLeft
  where
    computeZ (Point ax ay)
      (Point bx by) (Point cx cy) = 
      (bx-ax)*(cy-ay)-(by-ay)*(cx-ax)
    result = computeZ a b c
calculateDirection _ _ _        = Nothing

--a' = Point 1 2
--b' = Point 2 2
--c' = Point 2 3
--d' = Point (-1) 1
--e' = Point 22 3
--f' = Point 4 7
--g' = Point 4 (-7)

-- myDirection = calculateDirection a' b' c'

successiveTripleDirections :: [Point] -> [Maybe Direction]
successiveTripleDirections []         = []
successiveTripleDirections (_:[])     = []
successiveTripleDirections (_:_:[])   = []
successiveTripleDirections (a:b:c:s)  =
  (calculateDirection a b c):successiveTripleDirections (b:c:s)


---- Takes a Point list and finds the convex hull as a
---- list of ordered Points
grahamScanAlgo :: [Point] -> [Point]
grahamScanAlgo []           = []
grahamScanAlgo (_:[])       = []
grahamScanAlgo (_:_:[])     = []
grahamScanAlgo (a:b:c:s)    = 
  buildHull (a:b:c:s) [lowestPoint, secondPoint]
  where
    lowestPoint = getLowestPoint (a:b:c:s)
    secondPoint = getSecondPoint lowestPoint (a:b:c:s)

-- Assumes the Point list is not empty
getLowestPoint :: [Point] -> Point
getLowestPoint ([])   = error "No points available"
getLowestPoint (a:[]) = a
getLowestPoint ((Point ax ay):(Point bx by):s)
  | ay < by || (ay == by && ax < bx)  =
    getLowestPoint ((Point ax ay):s)
  | otherwise =
    getLowestPoint ((Point bx by):s)

-- Assumes the list is not empty
getSecondPoint  :: Point    -- lowest point
                -> [Point]  -- all points
                -> Point    -- second point in the hull
getSecondPoint _ []     = error "No points available"
getSecondPoint _ (a:[]) = a
getSecondPoint lowestPoint (a:b:s)
  | lowestPoint == a  = getSecondPoint lowestPoint (b:s)
  | lowestPoint == b  = getSecondPoint lowestPoint (a:s)
  | slopeA == 0 = getSecondPoint lowestPoint (a:s)
  | slopeB == 0 = getSecondPoint lowestPoint (b:s)
  | slopeA < 0 && slopeB < 0 && slopeA >= slopeB = getSecondPoint lowestPoint (a:s)
  | slopeA < 0 && slopeB < 0 && slopeA <= slopeB = getSecondPoint lowestPoint (b:s)
  | slopeA < 0 && slopeB > 0 = getSecondPoint lowestPoint (b:s)
  | slopeA > 0 && slopeB < 0 = getSecondPoint lowestPoint (a:s)
  | slopeA > 0 && slopeB > 0 && slopeA <= slopeB = getSecondPoint lowestPoint (a:s)
  | slopeA > 0 && slopeB > 0 && slopeA >= slopeB = getSecondPoint lowestPoint (b:s)
  | otherwise =
    error ("No slope cases matched A:" ++ show slopeA ++ " B:" ++ show slopeB)
    where
      slopeA = slope lowestPoint a
      slopeB = slope lowestPoint b
      slope :: Point -> Point -> Float
      slope (Point ax ay) (Point bx by) = (ay-by)/(ax-bx)

-- Build hull from the first two points given
buildHull :: [Point] -- All points
          -> [Point] -- Current hull
          -> [Point] -- Final hull returned
buildHull points hull
  -- Return the hull minus the last, duplicate point
  | head hull == last hull  = init hull 
  | otherwise               = buildHull points
    (hull ++ 
    [findNextPoint points 
      ((last $ init hull):(last hull):[])])
  where
    findNextPoint :: [Point] -> [Point] -> Point
    findNextPoint [] _      = error "No point available"
    findNextPoint (_:_:_) []  =
        error "Hull is incorrect size"
    findNextPoint (_:_:_) [_] =
        error "Hull is incorrect size"
    findNextPoint (_:_:_) (_:_:_:_) =
        error "Hull is incorrect size"
    findNextPoint (p1:[]) _ = p1
    findNextPoint (p1:p2:ps) (h1:h2:[])
      | h1 == p1 || h2 == p1 = findNextPoint (p2:ps) [h1,h2]
      | h1 == p2 || h2 == p2 = findNextPoint (p1:ps) [h1,h2]
      | calculateDirection h1 h2 p1 == Just MyLeft &&
        calculateDirection h2 p1 p2 == Just MyLeft  =
        findNextPoint (p1:ps) [h1,h2]
      | otherwise =
        findNextPoint (p2:ps) [h1,h2]
