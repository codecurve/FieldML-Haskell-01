import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Show.Functions

-- By Randall Britten
-- Auckland Bioengineering Institute
-- 2012
-- University of Auckland
-- Permission granted to redistribute in source code or binary form.  No warranty of any kind is given.
--
-- The ideas here were strongly influenced by Andrew Miller's open source "ModML", with some code copied directly from ModML.

-- This is under construction
-- Todo list:
-- - Validation:
-- --Validate Maps like "And" to check that both operands have consistent domains, and codomains are boolean.


type Label = String
type SetOfLabels = Set.Set Label

labelsFromIntegerRange :: Int->Int->SetOfLabels
labelsFromIntegerRange a b =
  Set.fromList $ map show [a..b]

data Map = 

  -- A constant true or false.
  BooleanConstant Bool |

  -- Logical and of two expressions.
  And Map Map |

  -- Logical not of an expression.
  Not Map |

  -- Logical or of two expressions.
  Or Map Map |

  LessThan Map Map |

  Equal Map Map |

  -- Any real value, as a constant.
  RealConstant Double |
  
  -- A free variable...
  RealVariable String |  
  
  -- If x {- then -} a {- else -} b, assumes codomain of "a" and "b" are the same, and that codomain of x is Booleans
  If Map Map Map |
  
  -- Assumes codomains of the two maps are the same, and that Plus has meaning on the codomain.  
  Plus Map Map |
  Minus Map Map |
  Times Map Map |
  Divide Map Map |

  -- Compose f g = f(g(x)), assumes f::b->c, g::a->b (i.e. domain/codomain compatibility).
  Compose Map Map |
  
  -- The domain must be the CartesianProduct of n discrete TopologicalSpaces, with a total cardinality equal to the number of parameters, 
  -- and n equal to the dimensionality of the parameter source.
  FromParameterSource [Double] TopologicalSpace |
  
  Project { factor :: Int, source :: Map } |

  Tuple [Map]
  deriving (Show, Eq)
  
-- Place holder in the design for a point located in a topological space.
data Point = Point
  deriving (Show, Eq)

data TopologicalSpace = 
  UnitSpace |
  Reals |
  Booleans |
  Labels SetOfLabels |
  Product [TopologicalSpace] |
--  DisjointUnion SetOfLabels (Label->TopologicalSpace) |
  
  -- The Map have codomain = Booleans, the resulting TopologicalSpace is the subset of the BooleanMap's domain where the BooleanMap evaluates to True.
  SimpleSubset Map |
  
  -- Used for creating the quotient TopologicalSpace from the provided TopologicalSpace. The map is required to be a boolean map.
  -- The resulting space is like the original space, but with points where the boolean map evaluates to True treated as a single point.
  Quotient TopologicalSpace TopologicalSpace Map 
  
  -- If the given space is a smooth manifold then this constructs the tangent space at that point.
--  TangetSpaceAtPoint TopologicalSpace Point
  deriving (Show, Eq)

  
-- Focus here is on processing the "FieldML" data structures.  

listOfFreeRealVariables :: Map -> Set.Set String
listOfFreeRealVariables (RealConstant _ ) = Set.empty
listOfFreeRealVariables (RealVariable variableName ) = Set.singleton variableName
listOfFreeRealVariables (If x a b ) = listOfFreeRealVariables $ Tuple [ x, a, b ]
listOfFreeRealVariables (Plus a b) = listOfFreeRealVariables $ Tuple [ a, b ]
listOfFreeRealVariables (Minus a b) = listOfFreeRealVariables $ Tuple [ a, b ]
listOfFreeRealVariables (Times a b) = listOfFreeRealVariables $ Tuple [ a, b ]
listOfFreeRealVariables (Divide a b) = listOfFreeRealVariables $ Tuple [ a, b ]
listOfFreeRealVariables (Compose f g) = listOfFreeRealVariables g
listOfFreeRealVariables (FromParameterSource _ a) = Set.empty -- Todo: Ouch, this is not correct, but Poul and Richard are right, factors have to be named, otherwise, where do the names come from?
listOfFreeRealVariables (Project n f) = listOfFreeRealVariables f
listOfFreeRealVariables (Tuple fs) = Set.fromList (foldr Set.union (map listOfFreeRealVariables fs))
listOfFreeRealVariables (BooleanConstant _) = Set.empty
listOfFreeRealVariables (And a b) = listOfFreeRealVariables $ Tuple [ a, b ]
listOfFreeRealVariables (Or a b) = listOfFreeRealVariables $ Tuple [ a, b ]
listOfFreeRealVariables (Not a) = listOfFreeRealVariables $ Tuple a
listOfFreeRealVariables (LessThan a b) = listOfFreeRealVariables $ Tuple [ a, b ]
listOfFreeRealVariables (Equal a b) = listOfFreeRealVariables $ Tuple [ a, b ]

  
domain :: Map -> TopologicalSpace
domain (RealConstant _ ) = UnitSpace
domain (RealVariable _ ) = Reals
domain (If x _ _ ) = domain x -- Should check somewhere that x, a and b have same domain, here?  Similarly for some other lines that follow.
domain (Plus a _) = domain a
domain (Minus a _) = domain a
domain (Times a _) = domain a
domain (Divide a _) = domain a
domain (Compose _ g) = domain g
domain (FromParameterSource _ a) = a
domain (Project n f) = domain f
domain (Tuple fs) = Product (map domain fs)
domain (BooleanConstant _) = UnitSpace
domain (And a _) = domain a
domain (Or a _) = domain a
domain (Not a) = domain a
domain (LessThan a _) = domain a
domain (Equal a _) = domain a

codomain :: Map ->TopologicalSpace
codomain (RealConstant _ ) = Reals
codomain (RealVariable _ ) = Reals
codomain (If _ a _ ) = codomain a -- Should check somewhere that x, a and b have same domain, here?  Similarly for some other lines that follow.
codomain (Plus a _) = codomain a  -- Should check if Plus is valid operator on codomain. Here?  Similarly for some others that follow.
codomain (Minus a _) = codomain a
codomain (Times a _) = codomain a
codomain (Divide a _) = codomain a
codomain (Compose f _) = codomain f
codomain (FromParameterSource _ a) = Reals -- Not sure if vector, matrix (tensor) valued params would be useful?
codomain (Project n f) = getFactor n (domain f)
codomain (Tuple fs) = Product (map codomain fs)
codomain (BooleanConstant _) = Booleans
codomain (And a _) = Booleans
codomain (Or a _) = Booleans
codomain (Not a) = Booleans
codomain (LessThan a _) = Booleans
codomain (Equal a _) = Booleans

getFactor :: Int -> TopologicalSpace -> TopologicalSpace
getFactor n (Product xs) = xs !! n

  
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

xy = Tuple [RealVariable "xx", RealVariable "yy"]
  
expression2 :: Map
expression2 =
    ((Project 1 xy) `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` (Project 1 xy)) 
    `And`
    ((Project 2 xy) `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` (Project 2 xy))

testResult1 = (domain expression2 == Product [Reals,Reals] )
  
unitSquare' = 
  SimpleSubset (
    ((Project 1 x) `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` (Project 1 x)) 
    `And`
    ((Project 2 x) `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` (Project 2 x))
  )

expression3a :: Map
expression3a =
  RealVariable "x" `Minus` RealConstant 1

expression3b :: Map
expression3b =
  RealVariable "x"
  
expression3c =
  Tuple [expression3a, expression3b]

testResult3a = ( domain expression3c == Reals )
testResult3b = ( codomain expression3c == Product [Reals,Reals] )


expression4 :: Map
expression4 =
    ( (RealConstant 0) `LessThan` (Project 1 xy) )
    `And`
    ( (RealConstant 0) `LessThan` (Project 2 xy) )
    `And`
    ( ( (Project 1 xy) `Plus` (Project 2 xy) ) `LessThan` (RealConstant 1)  )

-- Todo: This is really a poor man's way of doing unit testing, must improve this.
testResults = [
  testResult1,
  testResult3a,
  testResult3b
  ]

  
  
-- Just playing with Haskell Syntax here for convenience.  Will eventually delete everything below this line, and this comment.
