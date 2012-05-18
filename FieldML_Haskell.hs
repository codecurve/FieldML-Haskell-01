import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Show.Functions

-- By Randall Britten
-- Auckland Bioengineering Institute
-- 2012
-- University of Auckland
-- Permission granted to redistribute in source code or binary form.  No warranty of any kind is given.
--
-- The ideas here were strongly influenced by Andrew Miller's "ModML", with some code copied directly from ModML

-- This is under construction

type Label = String
type SetOfLabels = Set.Set Label

labelsFromIntegerRange :: Int->Int->SetOfLabels
labelsFromIntegerRange a b =
  Set.fromList $ map show [a..b]

data BooleanExpression =
  -- A constant true or false.
  BooleanConstant Bool |
  -- Logical and of two expressions.
  And BooleanExpression BooleanExpression |
  -- Logical not of an expression.
  Not BooleanExpression |
  -- Logical or of two expressions.
  Or BooleanExpression BooleanExpression |
  LessThan RealExpression RealExpression |
  Equal RealExpression RealExpression
  deriving (Show)
  
data RealExpression =
  -- Any real value, as a constant.
  RealConstant Double |
  -- A free variable...
  RealVariable String |  
  -- If x {- then -} b {- else -} b
  If BooleanExpression RealExpression RealExpression |
  Plus RealExpression RealExpression |
  Minus RealExpression RealExpression |
  Times RealExpression RealExpression |
  Divide RealExpression RealExpression
  deriving (Show)
  -- Etc

data Map = 
  Map { domain :: TopologicalSpace, codomain :: TopologicalSpace } |
  -- The domain and codomain are both Reals
  RealExpression |
  Compose Map Map |
  Project { factor :: Int, domain :: TopologicalSpace, codomain :: TopologicalSpace } |
  -- The codomain is implicitly BooleanTopologicalSpace
  BooleanMap { domain :: TopologicalSpace, predicate :: BooleanExpression } |
  -- The domain must be the CartesianProduct of n discrete TopologicalSpaces, with a total cardinality equal to the number of parameters, 
  -- and n equal to the dimensionality of the parameter source.
  FromParameterSource [Double] TopologicalSpace
  deriving (Show)

-- Place holder in the design for a point located in a topological space.
data Point = Point
  deriving (Show)

data TopologicalSpace = 
  Reals |
  BooleanTopologicalSpace |
  Labels SetOfLabels |
  Product [TopologicalSpace] |
  DisjointUnion SetOfLabels (Label->TopologicalSpace) |
  -- The Map must be a BooleanMap, the resulting TopologicalSpace is the subset of the BooleanMap's domain where the BooleanMap evaluates to True.
  SimpleSubset Map |
  -- Used for creating the quotient TopologicalSpace from the provided TopologicalSpace. The map is required to be a boolean map.
  -- The resulting space is like the original space, but with points where the boolean map evaluates to True treated as a single point.
  Quotient TopologicalSpace TopologicalSpace Map |
  -- If the given space is a smooth manifold then this constructs the tangent space at that point.
  TangetSpaceAtPoint TopologicalSpace Point
  deriving (Show)

-- Tests
real2 = Product [Reals, Reals]
real3 = Product [Reals, Reals, Reals]

elementIds = labelsFromIntegerRange 1 4
  
f :: Label->TopologicalSpace
f "1" = Reals
f _ = Reals

m1 = DisjointUnion elementIds f

m2 = Product [real2, Labels elementIds]

map1 = Project { factor=1, domain=m2, codomain=real2}

x = RealVariable "x"

expression1 :: BooleanExpression
expression1 =  (x `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` x)

-- Todo: get a chart for a topological space, and name the coordinates in the chart so that they can be mapped to the free variables of a real expression.

map2 = BooleanMap { domain=Reals, predicate=expression1 }
unitLineSegment = SimpleSubset map2

-- Just playing with Haskell Syntax here for convenience.  Will eventually delete everything below this line, and this comment.
