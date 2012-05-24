import FieldML_Haskell

-- Tests
real2 = Product [Reals, Reals]
real3 = Product [Reals, Reals, Reals]

elementIds = labelsFromIntegerRange 1 4
  
f :: Label->TopologicalSpace
f "1" = Reals
f _ = Reals

-- m1 = DisjointUnion elementIds f

m2 = Product [real2, Labels elementIds]

x = RealVariable "x"

expression1 :: Map
expression1 =  (x `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` x)

-- Todo: get a chart for a topological space, and name the coordinates in the chart so that they can be mapped to the free variables of a real expression.

unitLineSegment = SimpleSubset expression1

-- As above, but all inline:
unitLineSegment' = 
  SimpleSubset (
    (x `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` x)
  )

xy = Lambda [RealVariable "xx", RealVariable "yy"] (Tuple [RealVariable "xx", RealVariable "yy"])
  
expression2 :: Map
expression2 =
    ((Project 1 xy) `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` (Project 1 xy)) 
    `And`
    ((Project 2 xy) `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` (Project 2 xy))

testResult1 = (domain expression2 == Product [Reals,Reals] )
  
expression3a :: Map
expression3a =
  Restriction 
  unitLineSegment -- A validator would have to check that uniLineSegment is a sensible restriction of the original domain of the map.
  ( (Lambda [RealVariable "x"] (RealConstant 1) ) `Minus` RealVariable "x" )

expression3b :: Map
expression3b =
  Restriction 
  unitLineSegment
  (RealVariable "x")
  
expression3c =
  Tuple [expression3a, expression3b]

testResult3a = ( domain expression3c == unitLineSegment )
testResult3b = ( codomain expression3c == Product [Reals,Reals] )


expression4 :: Map
expression4 =
  Lambda [RealVariable "x", RealVariable "y"] $
    ( (RealConstant 0) `LessThan` RealVariable "x" )
    `And`
    ( (RealConstant 0) `LessThan` RealVariable "y" )
    `And`
    ( ( RealVariable "x" `Plus` RealVariable "y" ) `LessThan` (RealConstant 1)  )

simplex2d = SimpleSubset expression4
  
expression5 :: Map
expression5 =
  Lambda [RealVariable "x", RealVariable "y"] $
    (RealVariable "x" `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` RealVariable "x") 
    `And`
    (RealVariable "y" `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` RealVariable "y")

testResult5 = (domain expression5 == Product [Reals,Reals] )

unitSquare = SimpleSubset expression5
    
    
-- Todo: This is really a poor man's way of doing unit testing, must improve this.
testResults = [
  testResult1,
  testResult3a,
  testResult3b,
  testResult5
  ]

  
  
-- Just playing with Haskell Syntax here for convenience.  Will eventually delete everything below this line, and this comment.
