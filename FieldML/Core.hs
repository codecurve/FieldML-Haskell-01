module FieldML.Core
where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Show.Functions

-- By Randall Britten
-- Auckland Bioengineering Institute
-- 2012
-- University of Auckland
-- Permission granted to redistribute in source code or binary form, attributing the contributors.  No warranty of any kind is given.
--
-- The ideas here were strongly influenced by Andrew Miller's open source "ModML", with some code copied directly from ModML for some of the early versions.

-- This is under construction



type Label = String
data SetOfLabels = 

  StringLabels (Set.Set Label) |
  
  IntegerRange Int Int |
  
  DiscreteSetUnion SetOfLabels SetOfLabels |
  
  Intersection SetOfLabels SetOfLabels
    
  deriving(Show, Eq)

data Map = 

  -- | The sole element of the UnitSpace.
  -- Note that this is equivalent to Tuple []
  UnitElement |

  -- | A constant true or false.
  BooleanConstant Bool |

  -- | Logical and of two expressions.
  And Map Map |

  -- | Logical not of an expression.
  Not Map |

  -- | Logical or of two expressions.
  Or Map Map |

  LessThan Map Map |

  Equal Map Map |

  -- | Any real value, as a constant.
  RealConstant Double |
  
  -- | A free real variable...
  RealVariable String |  
  
  -- | A variable that can represent any element from any TopologicalSpace
  GeneralVariable String |
  
  -- | Assumes codomains of the two maps are the same, and that Plus has meaning on the codomain.  
  Plus Map Map |
  Minus Map Map |
  Times Map Map |
  Divide Map Map |

  -- | The string refers to the relevant entry in an OpenMath content dictionary by URL.
  -- The Map provided must either be a real variable for OpenMath functions that are a function of a real variable, 
  -- or a Tuple for functions of more than one variable.
  CSymbol String Map |

  Tuple [Map] |

  -- | If x {- then -} a {- else -} b, assumes codomain of "a" and "b" are the same, and that codomain of x is Booleans
  If Map Map Map |

  -- | Indirection, refers to the map in the list of maps (not sure where that is yet).  
  NamedMap String |

  Lambda [Map] Map |
  
  -- | PartialApplication n f g results in a map h whose domain A cross B, 
  -- where A is the same as the domain as the domain of f but with the n-th factor removed from the domain, and the value from g used for that slot.
  -- and B is the domain of g as a single slot for the tuple that represents g's domain.
  -- Since any Map essentially is an expression in some variables, this is equivalent to using the value of g in place of the relevant variable.
  PartialApplication Int Map Map |
  
  -- Compose f g = f(g(x)), assumes f::b->c, g::a->b (i.e. domain/codomain compatibility).
  -- This is similar to PartialApplication in a way, except that the domain of f is treated as a single slot.
  Compose Map Map |
  
  -- | The domain must be the CartesianProduct of n discrete TopologicalSpaces, with a total cardinality equal to the number of parameters, 
  -- and n equal to the length of the parameter source.
  FromParameterSource [Double] TopologicalSpace |
  
  Project { factor :: Int, source :: Map } |

  -- | The given topological space must be a simple subdomain of the domain of the given map.
  Restriction TopologicalSpace Map
  deriving (Show, Eq)
  

-- Todo: Andrew Miller proposed that we include spaces of functions.
data TopologicalSpace = 

  -- | Note that this is equivalent to CartesianProduct []
  UnitSpace |
  
  Reals |
  Booleans |
  Labels SetOfLabels |
  CartesianProduct [TopologicalSpace] |
  CartesianPower Int TopologicalSpace |
  
  -- | Factors xs m creates the a topological space from a cartesian product m, omitting factors of m that are not in xs.
  Factors [Int] TopologicalSpace |

  -- Todo: unit tests of DisjointUnion, and the design though here is probably incomplete.
  DisjointUnion SetOfLabels DomainMap |
  
  -- | The Map have codomain = Booleans, the resulting TopologicalSpace is the subset of the BooleanMap's domain where the BooleanMap evaluates to True.
  SimpleSubset Map |
  
  -- | SubsetReUnion xs requires that each x in xs is directly or indirectly a subset of one common set.
  SubsetReUnion [TopologicalSpace] |
  
  -- | Quotient m f creates the quotient of the TopologicalSpaces, m.  The equivalence operator for the quotient is induced from
  -- the maps in f.  
  -- The m must be a subset of the domain of f.
  -- The Equivalence operator is induced as follows: all points in m that map to the same point in the codomain are deemed equivalent.
  Quotient TopologicalSpace Map 
  
  -- If the given space is a smooth manifold then this constructs the tangent space at that point.
  -- Todo: perhaps tangent spaces are constructed by a method, rather than being a fundamental constructor.
--  TangetSpaceAtPoint TopologicalSpace Point
  deriving (Show, Eq)


-- | Domain Maps are for constructing disjoint unions, they produce a domain for each input value, where the input value must be from a SetOfLabels
data DomainMap =

  -- | This maps each label to the same TopologicalSpace
  DomainMapConstant TopologicalSpace |
  
  -- | DomainMapIf is either embedded in another parent DomainMapIf constructor, or in a DisjointUnion parent constructor.
  -- Either way, the parent constructor specifies a SetOfLabels, called s1.  This constructor's set of labels is called s2.
  -- The semantics are that for each x in s1 if it is in s2, then the domain is the one produced by the first domain map else it is the one produced by the 
  -- second domain map.
  DomainMapIf SetOfLabels DomainMap DomainMap
  
  deriving (Show, Eq)
  
-- Focus here is on *processing* the "FieldML" data structures.  

-- | simplifyTopologicalSpace m will attempt to produce a new TopologicalSpace that is equivalent to m, but has a simpler definition.
simplifyTopologicalSpace :: TopologicalSpace -> TopologicalSpace
simplifyTopologicalSpace (Factors (xs (CartesianProduct ys))) = CartesianProduct ys using xs as indices
simplifyTopologicalSpace CartesianProduct [] = UnitSpace
simplifyTopologicalSpace CartesianProduct [m] = m

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
listOfFreeRealVariables (Restriction _ f ) = listOfFreeRealVariables f -- Todo: What if the restriction fixes one of the variables? Is it still free, but only valid if it has that value?

listOfFreeRealVariables (PartialApplication n f g) = 
 Set.union fvars gvars
 where
   (front, back) = (splitAt (n-1) (drop 1 (Set.elems (listOfFreeRealVariables f))))
   newList = front ++ back
   fvars = Set.fromList newList
   gvars = (listOfFreeRealVariables g)

listOfFreeRealVariables (CSymbol _ f) = listOfFreeRealVariables f

  
domain :: Map -> TopologicalSpace
domain (RealConstant _ ) = UnitSpace
domain (RealVariable _ ) = Reals
domain (Tuple []) = UnitSpace
domain (Tuple [f]) = domain f
domain (t@(Tuple _)) 
  | n > 1 = CartesianPower n Reals
  | otherwise = Reals
  where 
    varSet = listOfFreeRealVariables t
    n = Set.size varSet 
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

-- Todo: We will need to comprehensively go through OpenMath CDs that we want to support and fill this out. Likewise for codomain.
domain (CSymbol "openmath cd transc1 cos" _) = Reals
domain (CSymbol "openmath cd transc1 sin" _) = Reals

  
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
codomain (CSymbol "openmath cd transc1 cos" _) = Reals
codomain (CSymbol "openmath cd transc1 sin" _) = Reals


getFactor :: Int -> TopologicalSpace -> TopologicalSpace
getFactor n (CartesianProduct xs) = xs !! n


validateMap :: Map -> Bool
validateMap (RealConstant _ ) = True
validateMap (RealVariable _ ) = True
validateMap (If x a b ) = 
  validateMap a && 
  validateMap b && 
  validateMap x && 
  (domain x == domain a ) &&
  (domain x == domain b) && 
  (codomain a == codomain b) &&
  (codomain x == Booleans)

validateMap (Plus a b) = 
  validateMap a &&
  validateMap b &&
  (domain a == domain b ) &&
  (codomain a == codomain b ) &&
  codomain a == Reals
  
validateMap (Minus a b) =
  validateMap a &&
  validateMap b &&
  (domain a == domain b ) &&
  (codomain a == codomain b ) &&
  codomain a == Reals

validateMap (Times a b) =
  validateMap a &&
  validateMap b &&
  (domain a == domain b ) &&
  (codomain a == codomain b ) &&
  codomain a == Reals

validateMap (Divide a b) =
  validateMap a &&
  validateMap b &&
  (domain a == domain b ) &&
  (codomain a == codomain b ) &&
  codomain a == Reals

-- validateMap (Compose f g) = 
--  validateMap f &&
--  validateMap g &&
--  codomain g == domain f

validateMap (FromParameterSource _ _) = True -- Todo: Just taking a shortcut for now, must fix this.

validateMap (Project n f) = validateMap f -- Todo: check that codomain of f has at least n factors.

validateMap (Tuple fs) = foldr (&&) True (map validateMap fs)

validateMap (BooleanConstant _) = True

validateMap (And a b) =
  validateMap a &&
  validateMap b &&
  domain a == domain b &&
  codomain a == Booleans &&
  codomain b == Booleans
  
validateMap (Or a b) =
  validateMap a &&
  validateMap b &&
  domain a == domain b &&
  codomain a == Booleans &&
  codomain b == Booleans

validateMap (Not a) =
  validateMap a &&
  codomain a == Booleans

validateMap (LessThan a b) = 
  validateMap a &&
  validateMap b &&
  (domain a == domain b ) &&
  (codomain a == codomain b ) &&
  codomain a == Reals

validateMap (Equal a b) =
  validateMap a &&
  validateMap b &&
  (domain a == domain b ) &&
  (codomain a == codomain b )

validateMap (Lambda bs f ) = 
  foldr (&&) True (map validateMap bs) &&
  validateMap f

validateMap (Restriction (SimpleSubset a) f ) = 
  validateMap f &&
  validateMap a &&
  domain a == domain f

validateMap (CSymbol "openmath cd transc1 cos" f) = codomain f == Reals
validateMap (CSymbol "openmath cd transc1 sin" f) = codomain f == Reals
