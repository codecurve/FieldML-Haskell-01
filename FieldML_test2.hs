connectivity = 
  [
    1,2,3,4,5, 6, 7, 8,
    5,6,7,8,9,10,11,12
  ]

dofs = 
  [
    1.0, 2.0, 1.0, 0.5, 0.0, 0.0, 0.0, 0.0, -0.5, -1.0, 0.0, 1.0
  ]

data TopologicalSpace = 
  CartesianProduct [TopologicalSpace] |
  IntegerEnsemble [Int]

  
cubeLocalNodes = IntegerEnsemble [1..8]
  
globalNodes = IntegerEnsemble [1..12]

elements = IntegerEnsemble [1..2]

-- elementNodes :: [Int]->[Int]
--elementNodes = fromParametersInt1 connectivity (CartesianProduct [elements, cubeLocalNodes])

data Cardinality =
  Discrete Int |
  Continuous
  deriving (Show)

cardinality :: TopologicalSpace -> Cardinality
cardinality (IntegerEnsemble xs) = Discrete $ length xs
-- cardinality ((CartesianProduct xs)) =
  -- a * b
  -- where
    -- Discrete a = (cardinality $ head xs) 
    -- Discrete b = (cardinality $ CartesianProduct xs)

-- fromParametersInt :: [Int]->TopologicalSpace->([Int]->[Int])
-- fromParametersInt dataSource (CartesianProduct headEnsemble:listOfEnsembles ) =
  -- let 
    -- n1 = length index1
    -- n2 = length index2
  -- in
    -- \xs -> 
      -- let 
        -- index1Value = head xs
        -- dropCount = n2 * (index1Value-1)
      -- in
        -- take n2  $ drop dropCount dataSource
