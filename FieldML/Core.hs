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
data FSet = 

  -- | Note that this is equivalent to CartesianProduct []
  UnitSpace |
  
  Reals |
  Booleans |
  Labels SetOfLabels |
  CartesianProduct [FSet] |
  
  -- | Factor n m creates the a topological space from a cartesian product m, consisting of the n'th factor, n=1 means the first factor.
  Factor Int FSet |

  -- Todo: unit testing of DisjointUnion, and the design thinking here is probably incomplete.
  DisjointUnion SetOfLabels DomainMap |

  -- | SimpleSubset p represents set-builder notation to create a set which consists of all x in the domain of the predicate, p,  
  -- such that the predicate p x is True. 
  -- p must have codomain = Booleans.
  SimpleSubset Map |
  
  -- | Image f represents the subset of the codomain of f to which any of the points in the domain of f are mapped by f.
  -- Hint: for the image of a subset, use a restricted map.
  -- Equivalent: Exists x p(x,y).  p(x,y) is a boolean valued map: p(x,y) = (y == f(x) )
  Image Map |

  -- | Quotient f creates the quotient of the domain of f (Hint, use a Restriction if necessary).  
  -- The equivalence operator for the quotient is induced from f as follows: all points in the domain of f that map to the same point in the codomain are deemed equivalent.
  -- In other words, points in the codomain are deemed to be the equivalence classes.
  -- Points that map to Unspecified in the codomain are treated as if they are not connected to any other points in the new Quotient space.
  Quotient Map |
  
  --  Todo: Possibly a constructor something like TangetSpaceAtPoint FSet Point
  -- If the given space is a smooth manifold then this constructs the tangent space at that point.
  -- Todo: perhaps tangent spaces are constructed by a function, rather than being a fundamental constructor.
  
  -- | Represents the domain of the given map.
  Domain Map |
  
  -- | Represents the codomain of the given map.
  Codomain Map 

  deriving (Show, Eq)


-- | SignatureSpace m n represents the set of all functions f such that f::m->n
-- Note that the special case where m is an Ensemble (i.e. SignatureSpace Labels _ ) is equivalent to a CartesianPower 
-- where each of the factors is labelled.  In FieldML, this is treated as having the same topology as a CartesianPower.

-- Todo: previously had SignatureSpace as a constructor for Topological space, needs more thought.
data SignatureSpace = SignatureSpace FSet FSet deriving (Show, Eq)


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
  ElementOf Map FSet |

  -- | Exists x f means: there exists x such that f is true.  x must be a general variable, and it must also be one of the free variables of f.  
  -- The codomain of f must be booleans.
  -- Equivalent: MathML/OpenMath: <csymbol cd="quant1">exists</csymbol> 
  Exists Map Map |
  
  -- | Interior m assumes m is a subset of m1. The domain of Interior m is m1. Interior m evaluates to true for all values x in m1 that are within the part of m1 bounded by m, or on m.
  -- One application of interior is for specifying a region of interest by means of an outline, for example, a map whose image in the xy plane is a polygon can be used as the predicate for SimpleSubset.
  Interior FSet |
  
  -- | Any real value, as a constant.
  RealConstant Double |
  
  -- | A variable that can represent any element from the specified FSet
  GeneralVariable String FSet |
  
  -- | Represents a possible result when the result of mapping a point is unknown, or left unspecified. 
  Unspecified FSet |
  
  -- | Assumes codomains of the two maps are both Reals.  Similarly for Minus, Times, Divide, and for subsequent
  -- standard elementary functions (Power) and transcendental functions: Sin, Cos, Exp.  
  -- Note, this restriction might be relaxed in future, allowing for suitable algebras to be valid codomains of operands, and perhaps
  -- for vectorisation.
  Plus Map Map |
  Minus Map Map |
  Negate Map |
  Times Map Map |
  Divide Map Map |
  Modulus Map Map |
  Sin Map |
  Cos Map |
  Exp Map |
  Power Map Map |
  
  -- | Pi, ratio of circumference of circle to diameter in Euclidean plane geometry.
  Pi |

  -- | The string refers to the relevant entry in an OpenMath content dictionary by URL.
  -- The Map provided must either be a real variable for OpenMath functions that are a function of a real variable, 
  -- or a Tuple for functions of more than one variable.
  
  Tuple [Map] |

  -- | If x {- then -} a {- else -} b, assumes codomain of a and b are the same, and that codomain of x is Booleans
  If Map Map Map |

  -- | Indirection, refers to the map in the list of maps (not sure where that is yet).  C.f. creating an identity using SignatureSpace.
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
  -- Note that this equivalent to function composition if f's domain is a single factor domain.
  
  -- Todo: consider using the name of a general variable that is part of the expression for f, rather than specifying the n'th slot. 
  -- Note however that would allow "deeper" binding, whereas the current approach only allows binding to any of the members of the top level
  -- tuple structure of the domain of f.
  -- Todo: contrast this style of partial application with function application in general, function composition, and variable substitution. Clarify apparent confusion.
  PartialApplication Int Map Map |
  
  -- Compose f g = f(g(x)), assumes f::b->c, g::a->b (i.e. domain/codomain compatibility).
  -- This is similar to PartialApplication in a way, except that the domain of f is treated as a single slot.
  Compose Map Map |
  
  -- | FromRealParameterSource xs f assumes that f is a GeneralVariable, or a Tuple of GeneralVariables, such that each GeneralVariable's FSet is Labels. The codomain of f must thus be the CartesianProduct of n discrete FSets, with a total cardinality equal to length xs.
  FromRealParameterSource [Double] Map |
  
  -- | See documentation for FromRealParameter source.
  FromIntegerParameterSource [Int] Map |

  -- | Project n f assumes f is a Tuple, and represents the n'th factor of the tuple.
  Project Int Map |

  -- | The given topological space must be a simple subdomain of the domain of the given map.
  Restriction FSet Map |
  
  -- | Max f Assumes codomain of f is Reals, and evaluates to maximum value that f attains over the domain of f.
  Max Map |
  
  -- | Same as Max, but evaluates to minimum value.
  Min Map |
  
  -- | Inverse f assumes that f is invertable, and represents the inverse function.
  Inverse Map |

  -- | h = KroneckerProduct fs requires each f in fs to be a 'Tuple's of reals, i.e. each member of the Tuple must have Reals as its codomain.
  -- The result is a Tuple whose length is the product of the lengths of of each f.
  -- For example, for the case where fs = [f,g], 
  -- h_i is f_j * g_k, where i = (j-1) * m + k, j=1..n, k=1..m and asterisk means scalar real multiplication.
  KroneckerProduct [Map] |
  
  -- | DistributedAccordingTo f g is true if f is distributed according to g, where g meets the requirements to serve 
  -- as a probability distribution for f.
  -- Informally, these requirements are:
  -- * g is real valued, i.e. the codomain of g is Reals.
  -- * The domain of g is the codomain of f, 
  -- * The domain of g must be a valid measure space. Note: canonical measure is assumed for Euclidean space and continuous subsets of Euclidian space.
  -- * The values taken by g are in the closed interval [0,1].
  -- * The integral of g over its domain is 1.
  DistributedAccordingTo Map Map |
  
  -- | DistributionFromRealisations fs requires that all Maps in fs have the same codomain as each other.
  -- It represents a Map whose domain is the codomain of f (where f is any member of fs), and whose codomain is Reals.
  -- Thus, if g = DistributionFromRealisations fs, g x is zero if x is not present in fs, and g x is equal to n/m if x is in fs, where 
  -- n is the number of times x occurs in fs, and m is length fs.
  -- This is analogous to distributionFromRealisations suggested by Andrew Miller and other designers of CellML uncertainty specification draft
  -- (see http://www.cellml.org/Members/miller/draft-secondary-spec-uncertainty/ July 2012)
  DistributionFromRealisations [Map]

  deriving (Show, Eq)


-- | Domain Maps are for the construction of disjoint unions, they produce a domain for each input value, where the input value must be from a SetOfLabels
data DomainMap =

  -- | This maps each label to the same FSet
  DomainMapConstant FSet |
  
  -- | DomainMapIf is either embedded in another parent DomainMapIf constructor, or in a DisjointUnion parent constructor.
  -- Either way, the parent constructor specifies a SetOfLabels, called s1.  This constructor's set of labels is called s2.
  -- The semantics are that for each x in s1 if it is in s2, then the domain is the one produced by the first domain map else it is the one produced by the 
  -- second domain map.
  DomainMapIf SetOfLabels DomainMap DomainMap
  
  deriving (Show, Eq)


-- Focus here is on *processing* the FieldML data structures.  

-- | simplifyFSet m will attempt to produce a new FSet that is equivalent to m, but has a simpler definition.
simplifyFSet :: FSet -> FSet
simplifyFSet (Factor n (CartesianProduct ys)) = ys !! n
simplifyFSet (CartesianProduct []) = UnitSpace
simplifyFSet (CartesianProduct [m]) = m
simplifyFSet m = m

listOfFreeGeneralVariables :: Map -> [Map]
listOfFreeGeneralVariables UnitElement = []
listOfFreeGeneralVariables (RealConstant _ ) = []
listOfFreeGeneralVariables f@(GeneralVariable _ _ ) = [f]
listOfFreeGeneralVariables (Unspecified m) = []
listOfFreeGeneralVariables (Tuple fs) = List.nub (concatMap listOfFreeGeneralVariables fs) 
listOfFreeGeneralVariables (If x a b ) = listOfFreeGeneralVariables $ Tuple [ x, a, b ]
listOfFreeGeneralVariables (Plus a b) = listOfFreeGeneralVariables $ Tuple [ a, b ]
listOfFreeGeneralVariables (Minus a b) = listOfFreeGeneralVariables $ Tuple [ a, b ]
listOfFreeGeneralVariables (Negate a) = listOfFreeGeneralVariables a
listOfFreeGeneralVariables (Times a b) = listOfFreeGeneralVariables $ Tuple [ a, b ]
listOfFreeGeneralVariables (Divide a b) = listOfFreeGeneralVariables $ Tuple [ a, b ]
listOfFreeGeneralVariables (Modulus a b) = listOfFreeGeneralVariables $ Tuple [ a, b ]
listOfFreeGeneralVariables (Sin x) = listOfFreeGeneralVariables x
listOfFreeGeneralVariables (Cos x) = listOfFreeGeneralVariables x
listOfFreeGeneralVariables (Exp x) = listOfFreeGeneralVariables x
listOfFreeGeneralVariables (Power x y) = listOfFreeGeneralVariables $ Tuple [ x, y ]
listOfFreeGeneralVariables Pi = []
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
listOfFreeGeneralVariables (Exists x@(GeneralVariable _ _) f) = List.delete x (listOfFreeGeneralVariables f)
-- Todo: Fix this: Lambda binds, i.e. Lambda x f means that x is no longer a free variable (assuming it was a free variable in f in the first place). Similarly probably for validate, codomain etc.
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

listOfFreeGeneralVariables (Max _) = []
listOfFreeGeneralVariables (Min _) = []

listOfFreeGeneralVariables (KroneckerProduct fs) = listOfFreeGeneralVariables $ Tuple fs
listOfFreeGeneralVariables (DistributedAccordingTo f g) = listOfFreeGeneralVariables $ Tuple [ f, g ]
listOfFreeGeneralVariables (DistributionFromRealisations fs) = listOfFreeGeneralVariables $ Tuple fs

spaceOfVariable :: Map -> FSet
spaceOfVariable (GeneralVariable _ a) = a

-- Todo: make "return type" "Either FSet or InvalidMap" so that validation can be built in.
domain :: Map -> FSet
domain UnitElement = UnitSpace
domain (RealConstant _ ) = UnitSpace
domain (GeneralVariable _ m) = m
domain (Unspecified _) = UnitSpace
domain (Tuple []) = UnitSpace
domain (Tuple [f]) = domain f
domain (Tuple fs) = simplifyFSet $ CartesianProduct $ map spaceOfVariable (List.nub (concatMap listOfFreeGeneralVariables fs))
domain (Lambda UnitElement _) = UnitSpace 
domain (Lambda (GeneralVariable _ m) _ ) = m -- Todo: assumes that there are no free variables in the lambda definition, since we aren't handling closures.
domain (Lambda t@(Tuple fs) _ ) = domain t
domain (If x a b ) = domain (Tuple [x, a, b])
domain (Plus a b) = domain (Tuple [ a, b ])
domain (Minus a b) = domain (Tuple [ a, b ])
domain (Negate a) = domain a
domain (Times a b) = domain (Tuple [ a, b ])
domain (Divide a b) = domain (Tuple [ a, b ])
domain (Modulus a b) = domain (Tuple [ a, b ])
domain (Sin x) = domain x
domain (Cos x) = domain x
domain (Exp x) = domain x
domain (Power x y) = domain (Tuple [ x, y])
domain Pi = UnitSpace
-- domain (Compose _ g) = domain g
domain (FromRealParameterSource _ f) = codomain f 
domain (FromIntegerParameterSource _ f) = codomain f
domain (Project n f) = domain f
domain (BooleanConstant _) = UnitSpace
domain (And a b) = domain (Tuple [ a, b ])
domain (Or a b) = domain (Tuple [ a, b ])
domain (Not a) = domain a
domain (LessThan a b) = domain (Tuple [ a, b ])
domain (Equal a b) = domain (Tuple [ a, b ])
domain (ElementOf x _) = domain x
domain (Restriction s _ ) = s
domain (Max _) = UnitSpace
domain (Min _) = UnitSpace
domain (KroneckerProduct fs) = domain (Tuple fs)
domain f@(Exists _ _)               = simplifyFSet $ CartesianProduct (map spaceOfVariable (listOfFreeGeneralVariables f))
domain f@(PartialApplication _ _ _) = simplifyFSet $ CartesianProduct (map spaceOfVariable (listOfFreeGeneralVariables f))
domain (DistributedAccordingTo f g ) = domain (Tuple [ f, g])
domain (DistributionFromRealisations fs ) = domain (Tuple fs)


-- Todo: make "return type" "Either FSet or InvalidMap" so that validation can be built in.  
codomain :: Map ->FSet
codomain UnitElement = UnitSpace
codomain (RealConstant _ ) = Reals
codomain (GeneralVariable _ m) = m -- GeneralVariable is essentially an identity map, its domain and codomain are the same.
codomain (Unspecified m) = m
codomain (Tuple fs) = CartesianProduct (map codomain fs)
codomain (Lambda _ f ) = codomain f
codomain (If _ a _ ) = codomain a
codomain (Plus a _) = codomain a
codomain (Minus a _) = codomain a
codomain (Negate a) = codomain a
codomain (Times a _) = codomain a
codomain (Divide a _) = codomain a
codomain (Modulus a _) = codomain a
codomain (Sin x) = codomain x -- Usually codomain x is Reals, if it was say complex or Square matrix, this would still make sense.  Also for vectorised interpretation.  Todo: needs more thought.
codomain (Cos x) = codomain x
codomain (Exp x) = codomain x -- Todo: This might work for codomain x Reals, complex, square matrix.  Different structure, in general, from the exponential map of a Riemannian manifold.
codomain (Power x y) = codomain x
codomain Pi = UnitSpace
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
codomain (Exists _ _) = Booleans
codomain (Restriction _ f ) = codomain f
codomain (Max _) = Reals
codomain (Min _) = Reals
codomain (KroneckerProduct fs ) = CartesianProduct (replicate m Reals)
  where
    m = product ( map tupleLength fs )
    tupleLength (Tuple gs) = length gs

codomain (PartialApplication _ f _) = codomain f
codomain (DistributedAccordingTo _ _ ) = Booleans
codomain (DistributionFromRealisations _) = Reals

getFactor :: Int -> FSet -> FSet
getFactor n (CartesianProduct xs) = xs !! (n-1)
getFactor 1 m = m


-- | Cardinality of discrete space. Zero if can't be easily determined, or has continuous components.

-- Todo: only some cases have been covered, i.e. only the bare minimum as required by existing unit tests.
cardinality :: FSet -> Int
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
  codomain a == codomain b &&
  codomain x == Booleans

validateMap (Plus a b) = 
  validateMap a &&
  validateMap b &&
  canonicalSuperset (codomain b) == Reals &&
  canonicalSuperset (codomain a) == Reals
  
validateMap (Minus a b) =
  validateMap a &&
  validateMap b &&
  canonicalSuperset (codomain b) == Reals &&
  canonicalSuperset (codomain a) == Reals

validateMap (Negate a) =
  validateMap a &&
  canonicalSuperset (codomain a) == Reals

validateMap (Times a b) =
  validateMap a &&
  validateMap b &&
  canonicalSuperset (codomain b) == Reals &&
  canonicalSuperset (codomain a) == Reals

validateMap (Divide a b) =
  validateMap a &&
  validateMap b &&
  canonicalSuperset (codomain b) == Reals &&
  canonicalSuperset (codomain a) == Reals

-- validateMap (Compose f g) = 
--  validateMap f &&
--  validateMap g &&
--  codomain g == domain f

validateMap (Sin x) =  
  validateMap x && 
  canonicalSuperset (codomain x) == Reals

validateMap (Cos x) =
  validateMap x && 
  canonicalSuperset (codomain x) == Reals
  
validateMap (Exp x) =
  validateMap x && 
  canonicalSuperset (codomain x) == Reals

validateMap (Power x y) =  validateMap x && validateMap y
validateMap Pi =  True

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
  codomain a == Booleans &&
  codomain b == Booleans
  
validateMap (Or a b) =
  validateMap a &&
  validateMap b &&
  codomain a == Booleans &&
  codomain b == Booleans

validateMap (Not a) =
  validateMap a &&
  codomain a == Booleans

validateMap (LessThan a b) = 
  validateMap a &&
  validateMap b &&
  canonicalSuperset (codomain a) == canonicalSuperset (codomain b) &&
  canonicalSuperset (codomain a) == Reals

validateMap (Equal a b) =
  validateMap a &&
  validateMap b &&
  canonicalSuperset (codomain a) == canonicalSuperset (codomain b)

validateMap (ElementOf _ _) = True

validateMap (Exists (GeneralVariable _ _) f) = 
  codomain f ==  Booleans &&
  validateMap f

validateMap (Lambda a f ) = (isVariableTuple a) && (validateMap f)
  where 
    isVariableTuple (GeneralVariable _ _) = True
    isVariableTuple (Tuple fs) = all isVariableTuple fs
    isVariableTuple _ = False

validateMap (Restriction (SimpleSubset a) f ) = 
  validateMap f &&
  validateMap a &&
  domain a == domain f

validateMap (Max f) = codomain f == Reals
validateMap (Min f) = codomain f == Reals

validateMap ( KroneckerProduct fs ) = 
  all validateMap fs &&
  all essentiallyTupleOfReals fs  
  where 
    essentiallyTupleOfReals g = all realCodomain gs
      where
        Tuple gs = unwrapTuple g        
        realCodomain = (\x -> (canonicalSuperset . codomain) x  == Reals)

validateMap (PartialApplication n f g) =
  validateMap f &&
  validateMap g &&
  canonicalSuperset (codomain g) == getFactor n (domain f)

validateMap (DistributedAccordingTo f g) = 
  canonicalSuperset (codomain g) == Reals &&
  domain g == codomain f

validateMap (DistributionFromRealisations fs) =
  all validateMap fs &&
  length (List.nub (map codomain fs)) == 1


-- | canonicalSuperset m returns n where m is a simple subset of n, or factors of m are subsets of factors of n.
canonicalSuperset :: FSet -> FSet

canonicalSuperset (CartesianProduct ms) = CartesianProduct (map canonicalSuperset ms)
canonicalSuperset (SimpleSubset f) = canonicalSuperset (domain f)
canonicalSuperset (Image f) = canonicalSuperset (codomain f)
canonicalSuperset (Factor n (CartesianProduct ms)) = canonicalSuperset (ms!!n)
canonicalSuperset m = m


-- | unwrapTuple f is used to extract the tuple structure, since some maps wrap around a tuple but the result retains the underlying tuple structure.
-- Currently useful for validation of KroneckerProduct. 

-- Todo: comprehensively match patterns for all Map constructors, currently only sufficient for existing tests.
unwrapTuple :: Map -> Map
unwrapTuple t@(Tuple _) = t
unwrapTuple (PartialApplication _ f _ ) = unwrapTuple f

