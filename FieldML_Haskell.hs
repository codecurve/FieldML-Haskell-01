import qualified Data.Set as Set
import Text.Show.Functions

type Label = String
type SetOfLabels = Set.Set Label

data Manifold = 
  Reals |
  Labels SetOfLabels | 
  Product [Manifold] |
  DisjointUnion SetOfLabels (Label->Manifold)
  deriving (Show)

-- Tests
real2 = Product [Reals, Reals]
real3 = Product [Reals, Reals, Reals]

elementIds = Set.fromList ([ "1","3","2","4" ])

f :: Label->Manifold
f "1" = Reals
f _ = Reals

m1 = DisjointUnion elementIds f

m2 = Product [real2, Labels elementIds]
