module FieldML.Core
where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Show.Functions

-- By Randall Britten
-- Auckland Bioengineering Institute
-- 2012
-- University of Auckland
-- Permission granted to redistribute in source code or binary form.  No warranty of any kind is given.
--
-- The ideas here were strongly influenced by Andrew Miller's open source "ModML", with some code copied directly from ModML for some of the early versions.

-- This is under construction
-- Todo list:
-- - Validation:
-- --Validate Maps like "And" to check that both operands have consistent domains, and codomains are boolean.
-- - Connectivity
-- - Tensor product basis functions
-- - Coordinate system transformation
-- - represent derivative continuity intention at connected points
-- - "Versions"
-- - Generate discretised points in a TopologicalSpace.
-- - Partial evaluation, binding, composition, projection from a general variable.


-- Done:
-- - Disjoint Union


type Label = String
data SetOfLabels = 

  StringLabels (Set.Set Label) |
  
  IntegerRange Int Int |
  
  Union SetOfLabels SetOfLabels |
  
  Intersection SetOfLabels SetOfLabels
    
  deriving(Show, Eq)

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
  
  -- A free real variable...
  RealVariable String |  
  
  -- A variable that can represent any element from any TopologicalSpace
  GeneralVariable String |
  
  -- Assumes codomains of the two maps are the same, and that Plus has meaning on the codomain.  
  Plus Map Map |
  Minus Map Map |
  Times Map Map |
  Divide Map Map |

  Tuple [Map] |
  
  Lambda [Map] Map |
  
  -- If x {- then -} a {- else -} b, assumes codomain of "a" and "b" are the same, and that codomain of x is Booleans
  If Map Map Map |
  
  -- Compose f g = f(g(x)), assumes f::b->c, g::a->b (i.e. domain/codomain compatibility).
  -- Todo: some confusion here: most other operators are already compositions, what is this composition?  Decided to hide "Compose" for now.
  -- Compose Map Map |
  
  -- The domain must be the CartesianProduct of n discrete TopologicalSpaces, with a total cardinality equal to the number of parameters, 
  -- and n equal to the length of the parameter source.
  FromParameterSource [Double] TopologicalSpace |
  
  Project { factor :: Int, source :: Map } |

  -- The given topological space must be a simple subdomain of the domain of the given map.
  Restriction TopologicalSpace Map
  deriving (Show, Eq)
  

-- Todo: Andrew Miller proposed that we include spaces of functions.
data TopologicalSpace = 
  UnitSpace |
  Reals |
  Booleans |
  Labels SetOfLabels |
  CartesianProduct [TopologicalSpace] |
  CartesianPower Int TopologicalSpace |

  -- Todo: unit tests of DisjointUnion, and the design though here is probably incomplete.
  DisjointUnion SetOfLabels DomainMap |
  
  -- The Map have codomain = Booleans, the resulting TopologicalSpace is the subset of the BooleanMap's domain where the BooleanMap evaluates to True.
  SimpleSubset Map |
  
  -- Used for creating the quotient TopologicalSpace from the provided TopologicalSpaces. The map is required to be a boolean map.
  -- The resulting space is like the original space, but with points where the boolean map evaluates to True treated as a single point.
  -- It is assumed, and would need validation, that the boolean map meets the requirements of an equivalence relation.
  Quotient TopologicalSpace TopologicalSpace Map 
  
  -- If the given space is a smooth manifold then this constructs the tangent space at that point.
  -- Todo: perhaps tangent spaces are constructed by a method, rather than being a fundamental constructor.
--  TangetSpaceAtPoint TopologicalSpace Point
  deriving (Show, Eq)


-- Domain Maps are for constructing disjoint unions, they produce a domain for each input value, where the input value must be from a SetOfLabels
data DomainMap =

  -- This maps each label to the same TopologicalSpace
  DomainMapConstant TopologicalSpace |
  
  -- DomainMapIf is either embedded in another parent DomainMapIf constructor, or in a DisjointUnion parent constructor.
  -- Either way, the parent constructor specifies a SetOfLabels, called s1.  This constructor's set of labels is called s2.
  -- The semantics are that for each x in s1 if it is in s2, then the domain is the one produced by the first domain map else it is the one produced by the 
  -- second domain map.
  DomainMapIf SetOfLabels DomainMap DomainMap
  
  deriving (Show, Eq)
  
-- Focus here is on *processing* the "FieldML" data structures.  

listOfFreeRealVariables :: Map -> Set.Set String
listOfFreeRealVariables (RealConstant _ ) = Set.empty
listOfFreeRealVariables (RealVariable variableName ) = Set.singleton variableName
listOfFreeRealVariables (Tuple fs) = foldr Set.union Set.empty (map listOfFreeRealVariables fs) 
listOfFreeRealVariables (If x a b ) = listOfFreeRealVariables $ Tuple [ x, a, b ]
listOfFreeRealVariables (Plus a b) = listOfFreeRealVariables $ Tuple [ a, b ]
listOfFreeRealVariables (Minus a b) = listOfFreeRealVariables $ Tuple [ a, b ]
listOfFreeRealVariables (Times a b) = listOfFreeRealVariables $ Tuple [ a, b ]
listOfFreeRealVariables (Divide a b) = listOfFreeRealVariables $ Tuple [ a, b ]
-- listOfFreeRealVariables (Compose f g) = listOfFreeRealVariables g
listOfFreeRealVariables (FromParameterSource _ a) = Set.empty -- Todo: Ouch, this is not correct, but it seems Poul and Richard are right, factors have to be named, otherwise, where do the names come from?
listOfFreeRealVariables (Project n f) = listOfFreeRealVariables f
listOfFreeRealVariables (BooleanConstant _) = Set.empty
listOfFreeRealVariables (And a b) = listOfFreeRealVariables $ Tuple [ a, b ]
listOfFreeRealVariables (Or a b) = listOfFreeRealVariables $ Tuple [ a, b ]
listOfFreeRealVariables (Not a) = listOfFreeRealVariables a
listOfFreeRealVariables (LessThan a b) = listOfFreeRealVariables $ Tuple [ a, b ]
listOfFreeRealVariables (Equal a b) = listOfFreeRealVariables $ Tuple [ a, b ]
listOfFreeRealVariables (Lambda fs _) = listOfFreeRealVariables $ Tuple fs
listOfFreeRealVariables (Restriction _ f ) = listOfFreeRealVariables f -- Todo: What if the restriction fixes one of the variables?

  
domain :: Map -> TopologicalSpace
domain (RealConstant _ ) = UnitSpace
domain (RealVariable _ ) = Reals
domain (Tuple []) = UnitSpace
domain (t@(Tuple _)) = 
  let 
    varSet = listOfFreeRealVariables t
    n = Set.size varSet 
  in CartesianPower n Reals
domain (Lambda [] _) = UnitSpace 
domain (Lambda fs _) 
  | (length fs > 1) = CartesianProduct $ map domain fs
  | otherwise = domain (head fs) -- This is just to avoid getting Product[singleFactor]
domain (If x _ _ ) = domain x -- Todo: Should check somewhere that x, a and b have same domain, here?  Similarly for some other lines that follow.
domain (Plus a _) = domain a
domain (Minus a _) = domain a
domain (Times a _) = domain a
domain (Divide a _) = domain a
-- domain (Compose _ g) = domain g
domain (FromParameterSource _ a) = a
domain (Project n f) = domain f
domain (BooleanConstant _) = UnitSpace
domain (And a _) = domain a
domain (Or a _) = domain a
domain (Not a) = domain a
domain (LessThan a _) = domain a
domain (Equal a _) = domain a
domain (Restriction s _ ) = s

  
codomain :: Map ->TopologicalSpace
codomain (RealConstant _ ) = Reals
codomain (RealVariable _ ) = Reals
codomain (Tuple fs) = CartesianProduct (map codomain fs)
codomain (Lambda _ f ) = codomain f
codomain (If _ a _ ) = codomain a -- Should check somewhere that x, a and b have same domain, here?  Similarly for some other lines that follow.
codomain (Plus a _) = codomain a  -- Should check if Plus is valid operator on codomain. Here?  Similarly for some others that follow.
codomain (Minus a _) = codomain a
codomain (Times a _) = codomain a
codomain (Divide a _) = codomain a
-- codomain (Compose f _) = codomain f
codomain (FromParameterSource _ a) = Reals -- Not sure if vector, matrix (tensor) valued params would be useful?
codomain (Project n f) = getFactor n (domain f)
codomain (BooleanConstant _) = Booleans
codomain (And a _) = Booleans
codomain (Or a _) = Booleans
codomain (Not a) = Booleans
codomain (LessThan a _) = Booleans
codomain (Equal a _) = Booleans
codomain (Restriction _ f ) = codomain f


getFactor :: Int -> TopologicalSpace -> TopologicalSpace
getFactor n (CartesianProduct xs) = xs !! n


validate :: Map -> Bool
validate (RealConstant _ ) = True
validate (RealVariable _ ) = True
validate (If x a b ) = 
  validate a && 
  validate b && 
  validate x && 
  (domain x == domain a ) &&
  (domain x == domain b) && 
  (codomain a == codomain b) &&
  (codomain x == Booleans)

validate (Plus a b) = 
  validate a &&
  validate b &&
  (domain a == domain b ) &&
  (codomain a == codomain b ) &&
  codomain a == Reals
  
validate (Minus a b) =
  validate a &&
  validate b &&
  (domain a == domain b ) &&
  (codomain a == codomain b ) &&
  codomain a == Reals

validate (Times a b) =
  validate a &&
  validate b &&
  (domain a == domain b ) &&
  (codomain a == codomain b ) &&
  codomain a == Reals

validate (Divide a b) =
  validate a &&
  validate b &&
  (domain a == domain b ) &&
  (codomain a == codomain b ) &&
  codomain a == Reals

-- validate (Compose f g) = 
--  validate f &&
--  validate g &&
--  codomain g == domain f

validate (FromParameterSource _ _) = True -- Todo: Just taking a shortcut for now, must fix this.

validate (Project n f) = validate f -- Todo: check that codomain of f has at least n factors.

validate (Tuple fs) = foldr (&&) True (map validate fs)

validate (BooleanConstant _) = True

validate (And a b) =
  validate a &&
  validate b &&
  domain a == domain b &&
  codomain a == Booleans &&
  codomain b == Booleans
  
validate (Or a b) =
  validate a &&
  validate b &&
  domain a == domain b &&
  codomain a == Booleans &&
  codomain b == Booleans

validate (Not a) =
  validate a &&
  codomain a == Booleans

validate (LessThan a b) = 
  validate a &&
  validate b &&
  (domain a == domain b ) &&
  (codomain a == codomain b ) &&
  codomain a == Reals

validate (Equal a b) =
  validate a &&
  validate b &&
  (domain a == domain b ) &&
  (codomain a == codomain b )

validate (Lambda bs f ) = 
  foldr (&&) True (map validate bs) &&
  validate f

validate (Restriction (SimpleSubset a) f ) = 
  validate f &&
  validate a &&
  domain a == domain f
