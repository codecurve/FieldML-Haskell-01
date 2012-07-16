module FieldML.Core
where

import qualified Data.Set as Set
import qualified Data.List as List
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


-- | An FSet (for FieldML set) is more general than a topological space or topological manifold for that matter.  FieldML domains qualify as topological spaces.
-- Essentially, an FSet is an object that can be represented by means of its constructors, i.e. broader than topological space, and not as broad as a set a la set theory.
data FSet = 

  -- | Note that this is equivalent to CartesianProduct []
  UnitSpace |
  
  Reals |
  Booleans |
  Labels SetOfLabels |
  CartesianProduct [FSet] |
  
  -- | Factor n m represents the FSet which is the n'th factor of a Cartesian product, n=1 means the first factor.
  Factor Int FSet |

  -- | DisjointUnion represents the set resulting from forming the disjoint union of other sets.
  
  -- Todo: unit testing of DisjointUnion, and the design thinking here is probably incomplete.
  -- Todo: expand on description of disjoint union.
  DisjointUnion SetOfLabels DomainMap |

  -- | SimpleSubset p represents set-builder notation to create a set which consists of all x in the domain of the predicate, p,  
  -- such that the predicate p x is True. 
  -- p must have codomain = Booleans.
  SimpleSubset Expression |
  
  -- | Image f represents the subset of the codomain of f to which any of the points in the domain of f are mapped by f.
  -- Hint: for the image of a subset, use a restricted map.
  -- Equivalent: Exists x p(x,y).  p(x,y) is a boolean valued map: p(x,y) = (y == f(x) )
  Image Expression |

  -- | Quotient f creates the quotient of the domain of f (Hint, use a Restriction if necessary).  
  -- The equivalence operator for the quotient is induced from f as follows: all points in the domain of f that map to the same point in the codomain are deemed equivalent.
  -- In other words, points in the codomain are deemed to be the equivalence classes.
  -- Points that map to Unspecified in the codomain are treated as if they are not connected to any other points in the new Quotient space.
  Quotient Expression |
  
  --  Todo: Possibly a constructor something like TangetSpaceAtPoint FSet Point
  -- If the given space is a smooth manifold then this constructs the tangent space at that point.
  -- Todo: perhaps tangent spaces are constructed by a function, rather than being a fundamental constructor.
  
  -- | Represents the domain of the given expression, if the expression is a lambda.
  Domain Expression |

  -- | Represents the codomain of the given expression, if the expression is a lambda.
  Codomain Expression |

  -- | SignatureSpace m n represents the set of all functions f whose domain is m and whose codomain is n.
  -- Note that the special case where m is an Ensemble (i.e. SignatureSpace Labels _ ) is equivalent to a CartesianPower 
  -- where each of the factors is labelled.  In FieldML, this is treated as having the same topology as a CartesianPower.
  SignatureSpace FSet FSet

  deriving (Show, Eq)



-- | An expression relates each value in one FSet, called its domain, to one value in its codomain, which is another FSet.
data Expression = 

  -- | The sole element of the UnitSpace.
  -- Note that this is equivalent to Tuple []
  UnitElement |

  -- | A constant true or false.
  BooleanConstant Bool |

  -- | Any real value, as a constant.
  RealConstant Double |

  -- | A variable that can represent any element from the specified FSet
  GeneralVariable String FSet |

  -- | Indirection, refers to the map in the list of maps (not sure where that is yet).
  
  -- Todo: is this needed, since Equals x expr1 being added to a 'List of assertions' would be equivalent.
  NamedExpression String |
  
  -- | Represents a possible result when the result of mapping a point is unknown, or left unspecified. 
  Unspecified FSet |

  Tuple [Expression] |

  -- | Project n x assumes x is a Tuple, and represents the n'th factor of the tuple.
  Project Int Expression |

  -- | Lambda x expr1 represents a lambda, binding x in the expression represented by expr1. 
  -- Thus, if g = Lambda x expr1, x has been bound, and if x was a free variable in expr1, it is not a free variable in g.
  -- g will now be an object in SignatureSpace m n, m is the domain of expr1, and n is the codomain of expr1.  In other words, the Lambda does 
  -- not affect the domain and codomain. However, it does alter the free variables, since it binds x, i.e. x is removed from the free variables.  
  --
  -- x must be either a free variable or a variable tuple.
  -- A variable tuple is a tuple whose members are either free variables or variable tuples (note the recursive definition).
  -- The value produced by the map when a value for x is provided is described by g.   
  -- g must not have free variables that are not present in x
  --
  -- A simple example of where a Lambda is useful: 
  -- it allows for free variables to be specified that may not be present in f, for example Lambda x (RealConstant 1).
  Lambda Expression Expression |

  -- | Inverse f assumes that f is invertable, and represents the inverse function. f must be a Lambda.
  Inverse Expression |

  -- | Lambdify expr1 is the same as Lambda x expr1, where x is the Tuple created from the list of free variables of expr1.
  -- This is a convenience for making lambdas from any expression.
  
  -- Todo: very much still just an experiment.
  Lambdify Expression |
  
  -- | Apply f x represents the application of a function f whose domain is m to a value represented by the expression x, 
  -- x must be an element of m.
  -- Typically, f is declared as f = Lambda x1 expr1.
  Apply Expression Expression |
  
  -- | h = Compose f g means that h(x) = f(g(x)). It is only valid if f::b->c, g::a->b (i.e. domain/codomain compatibility).
  -- This is similar to PartialApplication in a way, except that the domain of f is treated as a single slot.
  Compose Expression Expression |
  
  -- | PartialApplication n f g results in a map h whose domain A cross B, 
  -- where A is the same as the domain of f but with the n-th factor removed from the domain, and the value from g used for that slot.
  -- and B is the domain of g as a single slot for the tuple that represents g's domain.
  -- Note that this equivalent to function composition if f's domain is a single factor domain.
  
  -- Todo: consider using the name of a general variable that is part of the expression for f, rather than specifying the n'th slot. 
  -- Note however that would allow "deeper" binding, whereas the current approach only allows binding to any of the members of the top level
  -- tuple structure of the domain of f.
  -- Todo: contrast this style of partial application with function application in general, function composition, and variable substitution. Clarify apparent confusion.
  PartialApplication Int Expression Expression |

  -- | Logical and of two expressions.
  And Expression Expression |

  -- | Logical or of two expressions.
  Or Expression Expression |

  -- | Logical not of an expression.
  Not Expression |

  LessThan Expression Expression |

  Equal Expression Expression |

  -- | Assumes codomains of the two maps are both Reals.  Similarly for Minus, Times, Divide, and for subsequent
  -- standard elementary functions (Power) and transcendental functions: Sin, Cos, Exp.  
  -- Note, this restriction might be relaxed in future, allowing for suitable algebras to be valid codomains of operands, and perhaps
  -- for vectorisation.
  Plus Expression Expression |
  Minus Expression Expression |
  Negate Expression |
  Times Expression Expression |
  Divide Expression Expression |
  Modulus Expression Expression |
  Sin Expression |
  Cos Expression |
  Exp Expression |
  Power Expression Expression |

  -- | Pi, ratio of circumference of circle to diameter in Euclidean plane geometry.
  Pi |
  
  -- | If x {- then -} a {- else -} b, assumes codomain of a and b are the same, and that codomain of x is Booleans
  If Expression Expression Expression |

  -- | Max f Assumes codomain of f is Reals, and evaluates to maximum value that f attains over the domain of f. f must be a Lambda.
  Max Expression |
  
  -- | Same as Max, but evaluates to minimum value.
  Min Expression |
  
  -- | ElementOf x m represents a map that is true if x is in the set m, otherwise it is false.
  ElementOf Expression FSet |

  -- | Exists x f means: there exists x such that f is true.  x must be a general variable, and it must also be one of the free variables of f.  
  -- The codomain of f must be booleans.
  -- Equivalent: MathML/OpenMath: <csymbol cd="quant1">exists</csymbol> 
  Exists Expression Expression |
  
  -- | Interior m assumes m is a subset of m1. The domain of Interior m is m1. Interior m evaluates to true for all values x in m1 that are within the part of m1 bounded by m, or on m.
  -- One application of interior is for specifying a region of interest by means of an outline, for example, a map whose image in the xy plane is a polygon can be used as the predicate for SimpleSubset.

  -- Todo: Documentation above mentions x (as if it is an argument, but constructor doesn't have a slot for an argument).
  Interior FSet |
  
  -- | The given FSet must be a simple subdomain of the domain of the given expression.
  Restriction FSet Expression |
  
  -- | FromRealParameterSource xs y assumes that y is a GeneralVariable, or a Tuple of GeneralVariables, such that each GeneralVariable's FSet is Labels. 
  -- The type of y must thus be the CartesianProduct of n discrete FSets, with a total cardinality equal to length xs.
  FromRealParameterSource [Double] Expression |

  -- | See documentation for FromRealParameter source.
  FromIntegerParameterSource [Int] Expression |

  -- | y = KroneckerProduct xs requires each x in xs to be a 'Tuple's of real valued functions, 
  -- i.e. each member of the Tuple must be a Lambda that has Reals as its codomain.
  -- The represented result is a Tuple whose length is the product of the lengths of of each x.
  -- For example, for the case where xs = [x1,x2], and x1 has m members, x2 has n members, then
  -- y_i is x1_j * x2_k, where i = (j-1) * n + k, j=1..m, k=1..n and asterisk means scalar real multiplication, and _ precedes the index.
  KroneckerProduct [Expression] |

  -- | DistributedAccordingTo x f is true if x is distributed according to f, where f meets the requirements to serve 
  -- as a probability distribution for x.
  -- Informally, these requirements are:
  -- * f is a Lambda whose domain is the same as x's FSet (i.e. x is an element of the domain of f)
  -- * f is real valued, i.e. the codomain of f is Reals.
  -- * The domain of f must be a valid measure space. Note: canonical measure is assumed for Euclidean space and continuous subsets of Euclidian space.
  -- * The values taken by f are in the closed interval [0,1].
  -- * The Lebesgue integral of f over its domain is 1.
  DistributedAccordingTo Expression Expression |
  
  -- | DistributionFromRealisations xs requires that all Expressions in xs are values on the same FSet which we will refer to as m.
  -- It represents a Lambda Expression whose domain m, and whose codomain is Reals.
  -- Thus, if g = DistributionFromRealisations xs, Apply g x is zero if x is not present in xs, otherwise it is equal to p/q, where 
  -- p is the number of times x occurs in xs, and q is length xs.
  -- This is analogous to distributionFromRealisations suggested by Andrew Miller and other designers of CellML uncertainty specification draft
  -- (see http://www.cellml.org/Members/miller/draft-secondary-spec-uncertainty/ July 2012)
  DistributionFromRealisations [Expression]

  deriving (Show, Eq)


-- | Domain Expressions are for the construction of disjoint unions, they produce a domain for each input value, where the input value must be from a SetOfLabels
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

listOfFreeGeneralVariables :: Expression -> [Expression]
listOfFreeGeneralVariables UnitElement = []
listOfFreeGeneralVariables (BooleanConstant _) = []
listOfFreeGeneralVariables (RealConstant _ ) = []
listOfFreeGeneralVariables f@(GeneralVariable _ _ ) = [f]
listOfFreeGeneralVariables (Unspecified m) = []
listOfFreeGeneralVariables (Tuple xs) = List.nub (concatMap listOfFreeGeneralVariables xs) 
listOfFreeGeneralVariables (Project n x) = listOfFreeGeneralVariables x

-- Note, could have used more general pattern for Lambda, but the lack of exhaustive pattern matching is serving in the interim as poor man's validation.
listOfFreeGeneralVariables (Lambda t@(Tuple _) expr1) =  (listOfFreeGeneralVariables expr1) List.\\ (listOfFreeGeneralVariables t)
listOfFreeGeneralVariables (Lambda x@(GeneralVariable _ _) expr1 ) = List.delete x (listOfFreeGeneralVariables expr1)
listOfFreeGeneralVariables (Inverse f) = listOfFreeGeneralVariables f
listOfFreeGeneralVariables (Lambdify _) = []
listOfFreeGeneralVariables (Apply f x) = List.nub ((listOfFreeGeneralVariables f) ++ (listOfFreeGeneralVariables x) )
listOfFreeGeneralVariables (Compose f g) = listOfFreeGeneralVariables $ Tuple [ f, g ]
listOfFreeGeneralVariables (PartialApplication n f g) = List.nub ( (listOfFreeGeneralVariables f) ++ (listOfFreeGeneralVariables g) )

listOfFreeGeneralVariables (And a b) = listOfFreeGeneralVariables $ Tuple [ a, b ]
listOfFreeGeneralVariables (Or a b) = listOfFreeGeneralVariables $ Tuple [ a, b ]
listOfFreeGeneralVariables (Not a) = listOfFreeGeneralVariables a
listOfFreeGeneralVariables (LessThan a b) = listOfFreeGeneralVariables $ Tuple [ a, b ]
listOfFreeGeneralVariables (Equal a b) = listOfFreeGeneralVariables $ Tuple [ a, b ]

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
listOfFreeGeneralVariables (If x a b ) = listOfFreeGeneralVariables $ Tuple [ x, a, b ]
listOfFreeGeneralVariables (Max f) = listOfFreeGeneralVariables f
listOfFreeGeneralVariables (Min f) = listOfFreeGeneralVariables f

listOfFreeGeneralVariables (ElementOf x m) = listOfFreeGeneralVariables x -- Todo: What if there are free variables in the definition of m? Assuming here and elsewhere that there are not. Could merge FSet and Expression, so that an expression may represent an FSet?
listOfFreeGeneralVariables (Exists x@(GeneralVariable _ _) f) = List.delete x (listOfFreeGeneralVariables f)
listOfFreeGeneralVariables (Restriction _ f) = listOfFreeGeneralVariables f -- Todo: What if the restriction fixes one of the variables? Is it still free, but only valid if it has that value?
listOfFreeGeneralVariables (Interior _) = [] -- Todo: definition of m in Interior m may have free variables, but we aren't yet processing defintions of FSet.

listOfFreeGeneralVariables (FromRealParameterSource _ a) = listOfFreeGeneralVariables a
listOfFreeGeneralVariables (FromIntegerParameterSource _ a) = listOfFreeGeneralVariables a

listOfFreeGeneralVariables (KroneckerProduct fs) = listOfFreeGeneralVariables $ Tuple fs
listOfFreeGeneralVariables (DistributedAccordingTo x f) = listOfFreeGeneralVariables $ Tuple [ x, f ]
listOfFreeGeneralVariables (DistributionFromRealisations xs) = listOfFreeGeneralVariables $ Tuple xs

fSetOfVariable :: Expression -> FSet
fSetOfVariable (GeneralVariable _ a) = a

-- Todo: make "return type" "Either FSet or InvalidExpression" so that validation can be built in.
domain :: Expression -> FSet
domain UnitElement = UnitSpace
domain (RealConstant _ ) = UnitSpace
domain (GeneralVariable _ m) = m
domain (Unspecified _) = UnitSpace
domain (Tuple []) = UnitSpace
domain (Tuple [f]) = domain f
domain (Tuple fs) = simplifyFSet $ CartesianProduct $ map fSetOfVariable (List.nub (concatMap listOfFreeGeneralVariables fs))
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
domain f@(Exists _ _)               = simplifyFSet $ CartesianProduct (map fSetOfVariable (listOfFreeGeneralVariables f))
domain f@(PartialApplication _ _ _) = simplifyFSet $ CartesianProduct (map fSetOfVariable (listOfFreeGeneralVariables f))
domain (DistributedAccordingTo f g ) = domain (Tuple [ f, g])
domain (DistributionFromRealisations fs ) = domain (Tuple fs)


-- Todo: make "return type" "Either FSet or InvalidExpression" so that validation can be built in.  
codomain :: Expression ->FSet
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


validateExpression :: Expression -> Bool
validateExpression (RealConstant _ ) = True
validateExpression (GeneralVariable _ _) = True
validateExpression (If x a b ) = 
  validateExpression a && 
  validateExpression b && 
  validateExpression x && 
  codomain a == codomain b &&
  codomain x == Booleans

validateExpression (Plus a b) = 
  validateExpression a &&
  validateExpression b &&
  canonicalSuperset (codomain b) == Reals &&
  canonicalSuperset (codomain a) == Reals
  
validateExpression (Minus a b) =
  validateExpression a &&
  validateExpression b &&
  canonicalSuperset (codomain b) == Reals &&
  canonicalSuperset (codomain a) == Reals

validateExpression (Negate a) =
  validateExpression a &&
  canonicalSuperset (codomain a) == Reals

validateExpression (Times a b) =
  validateExpression a &&
  validateExpression b &&
  canonicalSuperset (codomain b) == Reals &&
  canonicalSuperset (codomain a) == Reals

validateExpression (Divide a b) =
  validateExpression a &&
  validateExpression b &&
  canonicalSuperset (codomain b) == Reals &&
  canonicalSuperset (codomain a) == Reals

-- validateExpression (Compose f g) = 
--  validateExpression f &&
--  validateExpression g &&
--  codomain g == domain f

validateExpression (Sin x) =  
  validateExpression x && 
  canonicalSuperset (codomain x) == Reals

validateExpression (Cos x) =
  validateExpression x && 
  canonicalSuperset (codomain x) == Reals
  
validateExpression (Exp x) =
  validateExpression x && 
  canonicalSuperset (codomain x) == Reals

validateExpression (Power x y) =  validateExpression x && validateExpression y
validateExpression Pi =  True

validateExpression (FromRealParameterSource xs f) = ((isDiscreteGvTuple f) || (isDiscreteGv f))  && (validateCardinality xs f)
  where
    validateCardinality xs f = (cardinality (codomain f) == length xs)    
    isDiscreteGvTuple (Tuple fs) = all isDiscreteGv fs
    isDiscreteGvTuple _ = False    
    isDiscreteGv (GeneralVariable _ (Labels _)) = True
    isDiscreteGv _ = False

validateExpression (FromIntegerParameterSource xs f) = validateExpression (FromRealParameterSource (replicate (length xs) 0.1) f)

validateExpression (Project n f) = validateExpression f -- Todo: check that codomain of f has at least n factors.

validateExpression (Tuple fs) = foldr (&&) True (map validateExpression fs)

validateExpression (BooleanConstant _) = True

validateExpression (And a b) =
  validateExpression a &&
  validateExpression b &&
  codomain a == Booleans &&
  codomain b == Booleans
  
validateExpression (Or a b) =
  validateExpression a &&
  validateExpression b &&
  codomain a == Booleans &&
  codomain b == Booleans

validateExpression (Not a) =
  validateExpression a &&
  codomain a == Booleans

validateExpression (LessThan a b) = 
  validateExpression a &&
  validateExpression b &&
  canonicalSuperset (codomain a) == canonicalSuperset (codomain b) &&
  canonicalSuperset (codomain a) == Reals

validateExpression (Equal a b) =
  validateExpression a &&
  validateExpression b &&
  canonicalSuperset (codomain a) == canonicalSuperset (codomain b)

validateExpression (ElementOf _ _) = True

validateExpression (Exists (GeneralVariable _ _) f) = 
  codomain f ==  Booleans &&
  validateExpression f

validateExpression (Lambda a f ) = (isVariableTuple a) && (validateExpression f)
  where 
    isVariableTuple (GeneralVariable _ _) = True
    isVariableTuple (Tuple fs) = all isVariableTuple fs
    isVariableTuple _ = False

validateExpression (Restriction (SimpleSubset a) f ) = 
  validateExpression f &&
  validateExpression a &&
  domain a == domain f

validateExpression (Max f) = codomain f == Reals
validateExpression (Min f) = codomain f == Reals

validateExpression ( KroneckerProduct fs ) = 
  all validateExpression fs &&
  all essentiallyTupleOfReals fs  
  where 
    essentiallyTupleOfReals g = all realCodomain gs
      where
        Tuple gs = unwrapTuple g        
        realCodomain = (\x -> (canonicalSuperset . codomain) x  == Reals)

validateExpression (PartialApplication n f g) =
  validateExpression f &&
  validateExpression g &&
  canonicalSuperset (codomain g) == getFactor n (domain f)

validateExpression (DistributedAccordingTo f g) = 
  canonicalSuperset (codomain g) == Reals &&
  domain g == codomain f

validateExpression (DistributionFromRealisations fs) =
  all validateExpression fs &&
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

-- Todo: comprehensively match patterns for all Expression constructors, currently only sufficient for existing tests.
unwrapTuple :: Expression -> Expression
unwrapTuple t@(Tuple _) = t
unwrapTuple (PartialApplication _ f _ ) = unwrapTuple f


--  LocalWords:  CartesianPower FSet
