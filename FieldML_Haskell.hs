import qualified Data.Set as Set

data SetOfLabels = 
  StringLabels (Set.Set String) | 
  IntegerLabels (Set.Set Integer) 
  deriving (Show)
  
data Manifold = 
  Reals | 
  Labels SetOfLabels | 
  Product [Manifold] |
  DisjointUnion [Manifold] SetOfLabels
  deriving (Show)

real2 = Product [Reals, Reals]
real3 = Product [Reals, Reals, Reals]

elementIds = Set.fromList ([ 1,3,2,4 ])

