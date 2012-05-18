import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Show.Functions

-- By Randall Britten
-- Auckland Bioengineering Institute
-- University of Auckland
--
-- The ideas here were strongly influenced by Andrew Miller's "ModML".

type Label = String
type SetOfLabels = Set.Set Label

labelsFromIntegerRange :: Int->Int->SetOfLabels
labelsFromIntegerRange a b =
  Set.fromList $ map show [a..b]

data BoolExpression =
  -- A constant true or false.
  BoolConstant Bool |
  -- Logical and of two expressions.
  And BoolExpression BoolExpression |
  -- Logical not of an expression.
  Not BoolExpression |
  -- Logical or of two expressions.
  Or BoolExpression BoolExpression |
  LessThan RealExpression RealExpression |
  Equal RealExpression RealExpression
  deriving (Show)
  
data RealExpression =
  RealConstant Double |
  -- If x {- then -} b {- else -} b
  If BoolExpression RealExpression RealExpression |
  Plus RealExpression RealExpression |
  Minus RealExpression RealExpression |
  Times RealExpression RealExpression |
  Divide RealExpression RealExpression
  deriving (Show)
  -- Etc

data Map = 
  Map { domain :: Manifold, codomain :: Manifold } |
  -- The domain and codomain are both Reals
  RealExpression |
  Compose Map Map |
  Project { factor :: Int, domain :: Manifold, codomain :: Manifold } |
  -- The codomain is implicitly BooleanManifold
  BooleanMap { domain :: Manifold } |
  -- The domain must be the CartesianProduct of n discrete manifolds, with a total cardinality equal to the number of parameters, 
  -- and n equal to the dimensionality of the parameter source.
  FromParameterSource { domain :: Manifold }
  deriving (Show)

-- Used for creating quotient manifolds, the map is required to be a boolean map.
data EquivalenceRelationship = EquivalenceRelationship Manifold Manifold Map
  deriving (Show)
  
data Manifold = 
  Reals |
  BooleanManifold |
  Labels SetOfLabels |
  Product [Manifold] |
  DisjointUnion SetOfLabels (Label->Manifold) |
  SimpleSubset Map |
  Quotient Manifold Manifold EquivalenceRelationship
  deriving (Show)


-- Tests
real2 = Product [Reals, Reals]
real3 = Product [Reals, Reals, Reals]

elementIds = labelsFromIntegerRange 1 4
  
f :: Label->Manifold
f "1" = Reals
f _ = Reals

m1 = DisjointUnion elementIds f

m2 = Product [real2, Labels elementIds]

map1 = Project { factor=1, domain=m2, codomain=real2}

-- Just playing with Haskell Syntax here for convenience.  Will eventually delete everything below this line, and this comment.
