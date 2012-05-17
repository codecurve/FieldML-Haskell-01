import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Show.Functions

type Label = String
type SetOfLabels = Set.Set Label

labelsFromIntegerRange :: Int->Int->SetOfLabels
labelsFromIntegerRange a b =
  Set.fromList $ map show [a..b]

data RealExpression =
    Plus RealExpression RealExpression
  | Minus RealExpression RealExpression
  | Times RealExpression RealExpression
  | Divide RealExpression RealExpression
  -- Etc

data Map = 
    Map { domain :: Manifold, codomain :: Manifold }
  | RealExpression
  | Compose Map Map
  | Project { factor :: Int, domain :: Manifold, codomain :: Manifold }
  | BooleanMap { domain :: Manifold }
  deriving (Show)

data Manifold = 
    Reals
  | Labels SetOfLabels
  | Product [Manifold]
  | DisjointUnion SetOfLabels (Label->Manifold) 
  | SimpleSubset Map
  deriving (Show)

data FieldmlModel = FieldmlModel {
  namedManifolds :: Map.Map String Manifold,
  namedFields    :: Map.Map String Map
}

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
