module FieldML.Core
where

import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import Text.Show.Functions

-- Todo: put a more sensible license terms etc. statement.
--
-- By Randall Britten
-- Auckland Bioengineering Institute
-- 2012
-- University of Auckland
-- Permission granted to redistribute in source code or binary form, attributing the contributors.  No warranty of any kind is given.
--
-- The ideas here were strongly influenced by Andrew Miller's open source ModML. 

-- This is under construction



type Label = String

-- | SetOfLabels is like an ensemble in FieldML, integer labels and their decimal string equivalent are considered to be identical labels.
data SetOfLabels = 

  -- | A discrete set where the elements are labelled by strings.
  StringLabels (Set.Set Label) |
  
  -- | IntegerRange a b represents a discrete set whose elements consist of the range of integers from a to b, inclusive.
  IntegerRange Int Int |
  
  -- | All Integers
  Integers |
  
  -- | Traditional union of discrete set.
  DiscreteSetUnion SetOfLabels SetOfLabels |
  
  -- | Traditional Intersection of a discrete set.
  Intersection SetOfLabels SetOfLabels
    
  deriving(Show, Eq)


-- | A topological space is more general than a topological manifold.  FieldML domains qualify as topological spaces.
data TopologicalSpace = 

  -- | Note that this is equivalent to CartesianProduct []
  UnitSpace |
  
  Reals |
  Booleans |
  Labels SetOfLabels |
  CartesianProduct [TopologicalSpace] |
  
  
  -- | Factor n m creates the a topological space from a cartesian product m, consisting of the n'th factor, n=1 means the first factor.
  Factor Int TopologicalSpace |

  -- Todo: unit testing of DisjointUnion, and the design thinking here is probably incomplete.
  DisjointUnion SetOfLabels DomainMap |

  -- | SimpleSubset p represents set-builder notation to create a set which consists of all x in the domain of the predicate, p,  
  -- such that the predicate p x is True. 
  -- p must have codomain = Booleans.
  SimpleSubset Map |
  
  -- | Image f represents the subset of the codomain of f to which any of the points in the domain of f are mapped by f.
  -- Hint: for the image of a subset, use a restricted map.


  -- Todo: Considerered omitting Image, and relying just on SimpleSubset, but found it convoluted.  Might try again some time.
  Image Map |


  -- | Quotient f creates the quotient of the domain of f (Hint, use a Restriction if necessary).  
  -- The equivalence operator for the quotient is induced from f as follows: all points in the domain of f that map to the same point in the codomain are deemed equivalent.
  -- In other words, points in the codomain are deemed to be the equivalence classes.
  -- Points that map to Unspecified in the codomain are treated as if they are not connected to any other points in the new Quotient space.
  Quotient Map |
  
  -- | SignatureSpace m n represents the set of all functions f such that f::m->n
  -- Note that the special case where m is an Ensemble (i.e. SignatureSpace Labels _ ) is equivalent to a CartesianPower 
  -- where each of the factors is labelled.  In FieldML, this is treated as having the same topology as a CartesianPower.
  SignatureSpace TopologicalSpace TopologicalSpace
  
  --  Todo: Possibly a constructor something like TangetSpaceAtPoint TopologicalSpace Point
  -- If the given space is a smooth manifold then this constructs the tangent space at that point.
  -- Todo: perhaps tangent spaces are constructed by a method, rather than being a fundamental constructor.

  deriving (Show, Eq)


-- | A map relates each value in one topological space, called its domain, to one value in its codomain, which is another topological space.
-- Note that values themselves are sometimes treated as maps whose domain is the UnitSpace.
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

  -- | ElementOf x m represents a map that is true if x is in the set m, otherwise it is false.
  ElementOf Map TopologicalSpace |

  -- | Interior m assumes m is a subset of m1. The domain of Interior m is m1. Interior m evaluates to true for all values x in m1 that are within the part of m1 bounded by m, or on m.
  -- One application of interior is for specifying a region of interest by means of an outline, for example, a map whose image in the xy plane is a polygon can be used as the predicate for SimpleSubset.
  Interior TopologicalSpace |
  
  -- | Any real value, as a constant.
  RealConstant Double |
  
  -- | A variable that can represent any element from the specified TopologicalSpace
  GeneralVariable String TopologicalSpace |
  
  -- | Represents a possible result when the result of mapping a point is unknown, or left unspecified. 
  Unspecified TopologicalSpace |
  
  -- | Assumes codomains of the two maps are the same, and that Plus has meaning on the codomain.  Similarly for Minus, Times, Divide.
  
  -- Todo: use OpenMath csymbols for these.
  Plus Map Map |
  Minus Map Map |
  Times Map Map |
  Divide Map Map |

  -- | The string refers to the relevant entry in an OpenMath content dictionary by URL.
  -- The Map provided must either be a real variable for OpenMath functions that are a function of a real variable, 
  -- or a Tuple for functions of more than one variable.
  
  -- Todo: I think that this prototype is at a layer where the meaning of all tokens should be "known", so I think that CSymbol should be removed.
  CSymbol String Map |

  Tuple [Map] |

  -- | If x {- then -} a {- else -} b, assumes codomain of a and b are the same, and that codomain of x is Booleans
  If Map Map Map |

  -- | Indirection, refers to the map in the list of maps (not sure where that is yet).  
  NamedMap String |

  -- | Lambda f g declares explicitly the free variables of a Map.  
  -- x is either a free variable or a variable tuple.  
  -- A variable tuple is a tuple whose members are either free variables or variable tuples (note the recursive definition).
  -- The value produced by the map when a value for x is provided is described by g.   
  -- g must not have free variables that are not present in x
  --
  -- Note that the free variables of a map can be inferred, but a Lambda is useful for at least two reasons:
  --  1) It allows the order of the free variables and the variable tuple structure to be explicitly specified.
  --  2) It allows for free variables to be specified that may not be present in g, for example Lambda x 1.
  Lambda Map Map |
  
  -- | PartialApplication n f g results in a map h whose domain A cross B, 
  -- where A is the same as the domain of f but with the n-th factor removed from the domain, and the value from g used for that slot.
  -- and B is the domain of g as a single slot for the tuple that represents g's domain.
  -- Since any Map essentially is an expression in some variables, this is equivalent to using the value of g in place of the relevant variable.
  PartialApplication Int Map Map |
  
  -- Compose f g = f(g(x)), assumes f::b->c, g::a->b (i.e. domain/codomain compatibility).
  -- This is similar to PartialApplication in a way, except that the domain of f is treated as a single slot.
  Compose Map Map |
  
  -- | FromRealParameterSource xs f assumes that f is a GeneralVariable, or a Tuple of GeneralVariables, such that each GeneralVariable's TopologicalSpace is Labels. The codomain of f must thus be the CartesianProduct of n discrete TopologicalSpaces, with a total cardinality equal to length xs.
  FromRealParameterSource [Double] Map |
  
  -- | See documentation for FromRealParameter source.
  FromIntegerParameterSource [Int] Map |

  -- | Project n f assumes f is a Tuple, and represents the n'th factor of the tuple.
  Project Int Map |

  -- | The given topological space must be a simple subdomain of the domain of the given map.
  Restriction TopologicalSpace Map |
  
  -- | Max f Assumes codomain of f is Reals, and evaluates to maximum value that f attains over the domain of f.
  Max Map |
  
  -- | Same as Max, but evaluates to minimum value.
  Min Map |
  
  -- | Inverse f assumes that f is invertable, and represents the inverse function.
  Inverse Map |
  
  -- | h = KroneckerProduct f g requires f and g to be 'Tuple's of reals, i.e. each member of the Tuple must have Reals as its codomain.
  -- The result is a Tuple whose length is n times m, where n is the length of f and m is the length of g.
  -- h_i is f_j * g_k, where i = (j-1) * m + k, j=1..n, k=1..m and asterisk means scalar real multiplication).
  KroneckerProduct Map Map

  deriving (Show, Eq)


-- | Domain Maps are for the construction of disjoint unions, they produce a domain for each input value, where the input value must be from a SetOfLabels
data DomainMap =

  -- | This maps each label to the same TopologicalSpace
  DomainMapConstant TopologicalSpace |
  
  -- | DomainMapIf is either embedded in another parent DomainMapIf constructor, or in a DisjointUnion parent constructor.
  -- Either way, the parent constructor specifies a SetOfLabels, called s1.  This constructor's set of labels is called s2.
  -- The semantics are that for each x in s1 if it is in s2, then the domain is the one produced by the first domain map else it is the one produced by the 
  -- second domain map.
  DomainMapIf SetOfLabels DomainMap DomainMap
  
  deriving (Show, Eq)


-- Focus here is on *processing* the FieldML data structures.  

-- | simplifyTopologicalSpace m will attempt to produce a new TopologicalSpace that is equivalent to m, but has a simpler definition.
simplifyTopologicalSpace :: TopologicalSpace -> TopologicalSpace
simplifyTopologicalSpace (Factor n (CartesianProduct ys)) = ys !! n
simplifyTopologicalSpace (CartesianProduct []) = UnitSpace
simplifyTopologicalSpace (CartesianProduct [m]) = m
simplifyTopologicalSpace m = m

listOfFreeGeneralVariables :: Map -> [Map]
listOfFreeGeneralVariables (RealConstant _ ) = []
listOfFreeGeneralVariables f@(GeneralVariable _ _ ) = [f]
listOfFreeGeneralVariables (Unspecified m) = []
listOfFreeGeneralVariables (Tuple fs) = List.nub (concatMap listOfFreeGeneralVariables fs) 
listOfFreeGeneralVariables (If x a b ) = listOfFreeGeneralVariables $ Tuple [ x, a, b ]
listOfFreeGeneralVariables (Plus a b) = listOfFreeGeneralVariables $ Tuple [ a, b ]
listOfFreeGeneralVariables (Minus a b) = listOfFreeGeneralVariables $ Tuple [ a, b ]
listOfFreeGeneralVariables (Times a b) = listOfFreeGeneralVariables $ Tuple [ a, b ]
listOfFreeGeneralVariables (Divide a b) = listOfFreeGeneralVariables $ Tuple [ a, b ]
-- listOfFreeGeneralVariables (Compose f g) = listOfFreeGeneralVariables g
listOfFreeGeneralVariables (FromRealParameterSource _ a) = listOfFreeGeneralVariables a
listOfFreeGeneralVariables (FromIntegerParameterSource _ a) = listOfFreeGeneralVariables a
listOfFreeGeneralVariables (Project n f) = listOfFreeGeneralVariables f
listOfFreeGeneralVariables (BooleanConstant _) = []
listOfFreeGeneralVariables (And a b) = listOfFreeGeneralVariables $ Tuple [ a, b ]
listOfFreeGeneralVariables (Or a b) = listOfFreeGeneralVariables $ Tuple [ a, b ]
listOfFreeGeneralVariables (Not a) = listOfFreeGeneralVariables a
listOfFreeGeneralVariables (LessThan a b) = listOfFreeGeneralVariables $ Tuple [ a, b ]
listOfFreeGeneralVariables (Equal a b) = listOfFreeGeneralVariables $ Tuple [ a, b ]
listOfFreeGeneralVariables (ElementOf x m) = listOfFreeGeneralVariables x -- Todo: What if there are free variables in the definition of m? Assuming here and elsewhere that there are not.
listOfFreeGeneralVariables (Lambda (Tuple fs) _) = listOfFreeGeneralVariables $ Tuple fs
listOfFreeGeneralVariables (Lambda g@(GeneralVariable _ _) _ ) = [g]
listOfFreeGeneralVariables (Restriction _ f ) = listOfFreeGeneralVariables f -- Todo: What if the restriction fixes one of the variables? Is it still free, but only valid if it has that value?

listOfFreeGeneralVariables (PartialApplication n f g) = 
 List.nub ( fvars ++ gvars )
 where
   (front, back) = (splitAt (n-1) (listOfFreeGeneralVariables f) )
   newList = front ++ (drop 1 back)
   fvars = newList
   gvars = (listOfFreeGeneralVariables g)

listOfFreeGeneralVariables (CSymbol _ f) = listOfFreeGeneralVariables f

listOfFreeGeneralVariables (Max _) = []
listOfFreeGeneralVariables (Min _) = []

listOfFreeGeneralVariables (KroneckerProduct f g) = listOfFreeGeneralVariables $ Tuple [ f, g ]


domain :: Map -> TopologicalSpace
domain (RealConstant _ ) = UnitSpace
domain (GeneralVariable _ m) = m
domain (Unspecified _) = UnitSpace
domain (Tuple []) = UnitSpace
domain (Tuple [f]) = domain f
domain (Tuple fs) = CartesianProduct $ map spaceOfVariable (List.nub (concatMap listOfFreeGeneralVariables fs))
  where spaceOfVariable (GeneralVariable _ a) = a
domain (Lambda UnitElement _) = UnitSpace 
domain (Lambda (GeneralVariable _ m) _ ) = m
domain (Lambda (Tuple fs) _ ) = domain (Tuple fs)
domain (If x a b ) = domain (Tuple [x,a,b]) 
domain (Plus a b) = domain (Tuple [a,b])
domain (Minus a b) = domain (Tuple [a,b])
domain (Times a b) = domain (Tuple [a,b])
domain (Divide a b) = domain (Tuple [a,b])
-- domain (Compose _ g) = domain g
domain (FromRealParameterSource _ f) = codomain f 
domain (FromIntegerParameterSource _ f) = codomain f
domain (Project n f) = domain f
domain (BooleanConstant _) = UnitSpace
domain (And a b) = domain (Tuple [a,b])
domain (Or a b) = domain (Tuple [a,b])
domain (Not a) = domain a
domain (LessThan a b) = domain (Tuple [a,b])
domain (Equal a b) = domain (Tuple [a,b])
domain (ElementOf x _) = domain x
domain (Restriction s _ ) = s
domain (Max _) = UnitSpace
domain (Min _) = UnitSpace
domain (KroneckerProduct f g) = domain (Tuple [f,g])

-- Todo: Change to explicitly having Sin and Cos constructor.  Noted elsewhere as well.
domain (CSymbol "openmath cd transc1 cos" _) = Reals
domain (CSymbol "openmath cd transc1 sin" _) = Reals

  
codomain :: Map ->TopologicalSpace
codomain (RealConstant _ ) = Reals
codomain (GeneralVariable _ m) = m -- GeneralVariable is essentially an identity map, its domain and codomain are the same.
codomain (Unspecified m) = m
codomain (Tuple fs) = CartesianProduct (map codomain fs)
codomain (Lambda _ f ) = codomain f
codomain (If _ a _ ) = codomain a -- Should check somewhere that x, a and b have same domain, here?  Similarly for some other lines that follow.
codomain (Plus a _) = codomain a  -- Should check if Plus is valid operator on codomain. Here?  Similarly for some others that follow.
codomain (Minus a _) = codomain a
codomain (Times a _) = codomain a
codomain (Divide a _) = codomain a
-- codomain (Compose f _) = codomain f
codomain (FromRealParameterSource _ _) = Reals
codomain (FromIntegerParameterSource _ _) = Labels Integers
codomain (Project n f) = getFactor n (codomain f)
codomain (BooleanConstant _) = Booleans
codomain (And a _) = Booleans
codomain (Or a _) = Booleans
codomain (Not a) = Booleans
codomain (LessThan a _) = Booleans
codomain (Equal a _) = Booleans
codomain (ElementOf _ _) = Booleans
codomain (Restriction _ f ) = codomain f
codomain (CSymbol "openmath cd transc1 cos" _) = Reals
codomain (CSymbol "openmath cd transc1 sin" _) = Reals
codomain (Max _) = Reals
codomain (Min _) = Reals
codomain (KroneckerProduct (Tuple fs) (Tuple gs) ) = CartesianProduct (replicate mn Reals) where mn = (length fs) * (length gs)

getFactor :: Int -> TopologicalSpace -> TopologicalSpace
getFactor n (CartesianProduct xs) = xs !! n
getFactor 1 m = m


-- | Cardinality of discrete space. Zero if can't be easily determined, or has continuous components.

-- Todo: only some cases have been covered, i.e. only the bare minimum as required by existing unit tests.
cardinality :: TopologicalSpace -> Int
cardinality UnitSpace = 1
cardinality (Labels (IntegerRange a b) ) = b - a + 1
cardinality (CartesianProduct fs) = product (map cardinality fs)
cardinality _ = 0


validateMap :: Map -> Bool
validateMap (RealConstant _ ) = True
validateMap (GeneralVariable _ _) = True
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

validateMap (FromRealParameterSource xs f) = ((isDiscreteGvTuple f) || (isDiscreteGv f))  && (validateCardinality xs f)
  where
    validateCardinality xs f = (cardinality (codomain f) == length xs)    
    isDiscreteGvTuple (Tuple fs) = all isDiscreteGv fs
    isDiscreteGvTuple _ = False    
    isDiscreteGv (GeneralVariable _ (Labels _)) = True
    isDiscreteGv _ = False

validateMap (FromIntegerParameterSource xs f) = validateMap (FromRealParameterSource (replicate (length xs) 0.1) f)

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

validateMap (ElementOf _ _) = True

validateMap (Lambda a f ) = (isVariableTuple a) && (validateMap f)
  where 
    isVariableTuple (GeneralVariable _ _) = True
    isVariableTuple (Tuple fs) = all isVariableTuple fs
    isVariableTuple _ = False

validateMap (Restriction (SimpleSubset a) f ) = 
  validateMap f &&
  validateMap a &&
  domain a == domain f

validateMap (CSymbol "openmath cd transc1 cos" f) = codomain f == Reals
validateMap (CSymbol "openmath cd transc1 sin" f) = codomain f == Reals

validateMap (Max f) = codomain f == Reals
validateMap (Min f) = codomain f == Reals

validateMap (KroneckerProduct (Tuple fs) (Tuple gs) ) = 
  all realCodomain fs && all realCodomain gs 
  where realCodomain = (\x -> (codomain x) == Reals)

validateMap (PartialApplication n f g) =
  validateMap f &&
  validateMap g &&
  codomain g == getFactor n (domain f)
