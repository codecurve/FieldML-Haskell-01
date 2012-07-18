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

  -- | Lambda x expr represents a lambda, binding x in the expression represented by expr.
  -- Thus, if g = Lambda x expr, x has been bound, and if x was a free variable in expr, it is not a free variable in g.
  -- g will now be an object in SignatureSpace m n, where m is the codomain of the free variables of x, and n is the codomain of expr.  
  -- Lambda alters the free variables, since it binds x, i.e. the free variables of g are the free variables of expr with x removed.  
  --
  -- x must be either a free variable or a variable tuple.
  -- A variable tuple is a tuple whose members are either free variables or variable tuples (note the recursive definition).
  -- The value produced by the map when a value for x is provided (i.e. Apply g x) is described by expr.
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
  
  -- | If h = PartialApplication n f x then the domain of h is the same as the domain of f 
  -- but with the n-th factor removed from the domain, and the value from x used for that slot.
  -- Note that this equivalent to function application if f's domain is a single factor domain.
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
  
  -- | The given FSet must be a simple subdomain of the domain of the given expression.
  Restriction FSet Expression |
  
  -- | Interior m assumes m is a subset of m1. The domain of Interior m is m1. f = Interior m represents a lambda i.e. f(x) evaluates to true for all values x in m1 that are within the part of m1 bounded by m, or on m, otherwise false.
  -- One application of interior is for specifying a region of interest by means of an outline, for example, a map whose image in the xy plane is a polygon can be used as the predicate for SimpleSubset.
  Interior FSet |
  
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
simplifyFSet (SignatureSpace UnitSpace m) = m
simplifyFSet m = m

freeVariables :: Expression -> [Expression]
freeVariables UnitElement = []
freeVariables (BooleanConstant _) = []
freeVariables (RealConstant _ ) = []
freeVariables f@(GeneralVariable _ _ ) = [f]
freeVariables (Unspecified m) = []
freeVariables (Tuple xs) = List.nub (concatMap freeVariables xs) 
freeVariables (Project n x) = freeVariables x

-- Note, could have used more general pattern for Lambda, but the lack of exhaustive pattern matching is serving in the interim as poor man's validation.
freeVariables (Lambda t@(Tuple _) expr1) =  (freeVariables expr1) List.\\ (freeVariables t)
freeVariables (Lambda x@(GeneralVariable _ _) expr1 ) = List.delete x (freeVariables expr1)
freeVariables (Inverse f) = freeVariables f
freeVariables (Lambdify _) = []
freeVariables (Apply f x) = List.nub ((freeVariables f) ++ (freeVariables x) )
freeVariables (Compose f g) = freeVariables $ Tuple [ f, g ]
freeVariables (PartialApplication n f x) = List.nub ( (freeVariables f) ++ (freeVariables x) )

freeVariables (And a b) = freeVariables $ Tuple [ a, b ]
freeVariables (Or a b) = freeVariables $ Tuple [ a, b ]
freeVariables (Not a) = freeVariables a
freeVariables (LessThan a b) = freeVariables $ Tuple [ a, b ]
freeVariables (Equal a b) = freeVariables $ Tuple [ a, b ]

freeVariables (Plus a b) = freeVariables $ Tuple [ a, b ]
freeVariables (Minus a b) = freeVariables $ Tuple [ a, b ]
freeVariables (Negate a) = freeVariables a
freeVariables (Times a b) = freeVariables $ Tuple [ a, b ]
freeVariables (Divide a b) = freeVariables $ Tuple [ a, b ]
freeVariables (Modulus a b) = freeVariables $ Tuple [ a, b ]
freeVariables (Sin x) = freeVariables x
freeVariables (Cos x) = freeVariables x
freeVariables (Exp x) = freeVariables x
freeVariables (Power x y) = freeVariables $ Tuple [ x, y ]
freeVariables Pi = []
freeVariables (If x a b ) = freeVariables $ Tuple [ x, a, b ]
freeVariables (Max f) = freeVariables f
freeVariables (Min f) = freeVariables f

freeVariables (ElementOf x m) = freeVariables x -- Todo: What if there are free variables in the definition of m? Assuming here and elsewhere that there are not. Could merge FSet and Expression, so that an expression may represent an FSet?
freeVariables (Exists x@(GeneralVariable _ _) f) = List.delete x (freeVariables f)
freeVariables (Restriction _ f) = freeVariables f -- Todo: What if the restriction fixes one of the variables? Is it still free, but only valid if it has that value?
freeVariables (Interior _) = [] -- Todo: definition of m in Interior m may have free variables, but we aren't yet processing defintions of FSet.

freeVariables (FromRealParameterSource _ a) = freeVariables a
freeVariables (FromIntegerParameterSource _ a) = freeVariables a

freeVariables (KroneckerProduct fs) = freeVariables $ Tuple fs
freeVariables (DistributedAccordingTo x f) = freeVariables $ Tuple [ x, f ]
freeVariables (DistributionFromRealisations xs) = freeVariables $ Tuple xs

fSetOfVariable :: Expression -> FSet
fSetOfVariable (GeneralVariable _ a) = a

-- | Returns the FSet from which a function maps values. Unless it is actually a function, the expression is treated as a value, which is treated as a function from UnitSpace.

-- Todo: make "return type" "Either FSet or InvalidExpression" so that validation can be built in.
-- Todo: Explicit patterns all mentioned at this stage there is no 'catch-all', still using this to provide rudimentary debugging, but would look prettier with the UnitSpace case handled by a catch-all.
domain :: Expression -> FSet

domain UnitElement = UnitSpace
domain (BooleanConstant _) = UnitSpace
domain (RealConstant _ ) = UnitSpace
domain (GeneralVariable _ (SignatureSpace m _)) = m
domain (GeneralVariable _ _) = UnitSpace
domain (Unspecified _) = UnitSpace
domain (Tuple _) = UnitSpace
domain (Project n (Tuple fs)) = domain (fs!!n)
domain (Lambda UnitElement _) = UnitSpace 
domain (Lambda x@(GeneralVariable _ _) _ ) = codomain x
domain (Lambda t@(Tuple _) _ ) = codomain t
domain (Inverse f) = codomain f
domain (Lambdify expr) = simplifyFSet $ CartesianProduct $ map fSetOfVariable (freeVariables expr)
domain (Apply _ _) = UnitSpace
domain (Compose _ g) = domain g
domain (PartialApplication n f _) = simplifyFSet $ CartesianProduct ((take (n-1) fFactors) ++ (drop n fFactors))
  where
    fFactors = getFactors (domain f)
    getFactors (CartesianProduct ms) = ms
    getFactors m = [m]

domain (And _ _) = UnitSpace
domain (Or _ _) = UnitSpace
domain (Not _) = UnitSpace
domain (LessThan _ _) = UnitSpace
domain (Equal _ _) = UnitSpace
domain (Plus _ _) = UnitSpace
domain (Minus _ _) = UnitSpace
domain (Negate _) = UnitSpace
domain (Times _ _) = UnitSpace
domain (Divide _ _) = UnitSpace
domain (Modulus _ _) = UnitSpace
domain (Sin _) = UnitSpace
domain (Cos _) = UnitSpace
domain (Exp _) = UnitSpace
domain (Power _ _) = UnitSpace
domain Pi = UnitSpace
domain (If _ _ _ ) = UnitSpace
domain (Max _) = UnitSpace
domain (Min _) = UnitSpace
domain (ElementOf _ _) = UnitSpace
domain (Exists _ _) = UnitSpace
domain (Restriction m _ ) = m
domain (Interior (SimpleSubset l@(Lambda _ _)) ) = domain l
domain (FromRealParameterSource _ _) = UnitSpace
domain (FromIntegerParameterSource _ _) = UnitSpace
domain (KroneckerProduct _) = UnitSpace
domain (DistributedAccordingTo _ _ ) = UnitSpace
domain (DistributionFromRealisations xs ) = codomain (head xs) -- Note: this Assumes all xs also have codomain same as head xs.  This is checked by validateExpression.


-- | Returns the FSet to which a function maps values. Even if it is actually just a value expression, rather than a function, the expression is treated as a function from UnitSpace, and then the codomain is the 'type' of the value.

-- Todo: make "return type" "Either FSet or InvalidExpression" so that validation can be built in.  
-- Todo: make it so that it can be assumed that codomain has simplified the FSet before returning it.
codomain :: Expression -> FSet

codomain UnitElement = UnitSpace
codomain (BooleanConstant _) = Booleans
codomain (RealConstant _ ) = Reals
codomain (GeneralVariable _ m) = m
codomain (Unspecified m) = m
codomain (Tuple fs) = CartesianProduct (map codomain fs)
codomain (Project n f) = getFactor n (codomain f)
codomain (Lambda _ expr ) = codomain expr
codomain (Inverse f) = domain f
codomain (Lambdify expr) = codomain expr
codomain (Apply f _) = codomain f
codomain (Compose f _) = codomain f
codomain (PartialApplication _ f _) = codomain f
codomain (And _ _) = Booleans
codomain (Or _ _) = Booleans
codomain (Not _) = Booleans
codomain (LessThan _ _) = Booleans
codomain (Equal _ _) = Booleans
codomain (Plus _ _) = Reals
codomain (Minus _ _) = Reals
codomain (Negate _) = Reals
codomain (Times _ _) = Reals
codomain (Divide _ _) = Reals
codomain (Modulus _ _) = Reals
codomain (Sin _) = Reals
codomain (Cos _) = Reals
codomain (Exp _) = Reals
codomain (Power _ _) = Reals
codomain Pi = Reals
codomain (If _ a _ ) = codomain a
codomain (Max _) = Reals
codomain (Min _) = Reals
codomain (ElementOf _ _) = Booleans
codomain (Exists _ _) = Booleans
codomain (Restriction _ f ) = codomain f
codomain (Interior _) = Booleans
codomain (FromRealParameterSource _ _) = Reals
codomain (FromIntegerParameterSource _ _) = Labels Integers
codomain (KroneckerProduct fs ) = CartesianProduct (replicate m Reals)
  where
    m = product ( map tupleLength fs )
    tupleLength (Tuple gs) = length gs

codomain (DistributedAccordingTo _ _ ) = Booleans
codomain (DistributionFromRealisations _) = Reals

-- | True if expression passes a limited set of tests.  Note: this is under construction, so sometimes an expression is reported as valid, even if it is not valid.
validExpression :: Expression -> Bool

validExpression UnitElement = True
validExpression (BooleanConstant _) = True
validExpression (RealConstant _ ) = True
validExpression (GeneralVariable _ _) = True -- Todo: Could validate the name of the variable according to some rules for identifier names.
validExpression (Unspecified _) = True
validExpression (Tuple xs) = all validExpression xs
validExpression (Project n x) = 
  validExpression x  && 
  factorCount (codomain x) >= n

validExpression (Lambda x expr ) = (isVariableTuple x) && (validExpression expr)
  where 
    isVariableTuple (GeneralVariable _ _) = True
    isVariableTuple (Tuple xs) = all isVariableTuple xs
    isVariableTuple _ = False

-- Todo: Other expressions are lambda like, and can be inverted, add their cases.  Probably will treat inverse of values that are not lambda-like as invalid though.
validExpression (Inverse f) = 
  validExpression f &&
  lambdaLike f

validExpression (Lambdify expr) = validExpression expr && not (lambdaLike expr) -- Todo: Not sure if the restriction that expr is "not lambda-like" is necessary.

validExpression (Apply f x) = 
  lambdaLike f &&
  codomain x == domain f &&
  validExpression f &&
  validExpression x

validExpression (Compose f g) = 
  lambdaLike f &&
  lambdaLike g &&
  validExpression f &&
  validExpression g &&
  codomain g == domain f

validExpression (PartialApplication n f x) =
  lambdaLike f &&
  factorCount (domain f) >= n &&
  canonicalSuperset (codomain x) == getFactor n (domain f) &&
  validExpression f &&
  validExpression x

validExpression (And a b) = validBinaryOp Booleans a b

validExpression (Or a b) = validBinaryOp Booleans a b

validExpression (Not a) =
  validExpression a &&
  codomain a == Booleans &&
  not (lambdaLike a)

validExpression (LessThan a b) = validBinaryOp Reals a b

validExpression (Equal a b) =
  validExpression a &&
  validExpression b &&
  canonicalSuperset (codomain a) == canonicalSuperset (codomain b) &&
  not (lambdaLike a) &&
  not (lambdaLike b)

validExpression (Plus a b) = validBinaryOp Reals a b

validExpression (Minus a b) = validBinaryOp Reals a b

validExpression (Negate a) = validUnaryOp Reals a

validExpression (Times a b) = validBinaryOp Reals a b

validExpression (Divide a b) = validBinaryOp Reals a b

validExpression (Modulus a b) = validBinaryOp Reals a b

validExpression (Sin x) = validUnaryOp Reals x

validExpression (Cos x) = validUnaryOp Reals x
  
validExpression (Exp x) = validUnaryOp Reals x

validExpression (Power x y) = validBinaryOp Reals x y

validExpression Pi =  True

validExpression (If x a b ) = 
  validExpression a && 
  validExpression b && 
  validExpression x && 
  codomain a == codomain b &&
  codomain x == Booleans &&
  not (lambdaLike x)

validExpression (FromRealParameterSource xs f) = ((isDiscreteGvTuple f) || (isDiscreteGv f))  && (validateCardinality xs f)
  where
    validateCardinality xs f = (cardinality (codomain f) == length xs)    
    isDiscreteGvTuple (Tuple fs) = all isDiscreteGv fs
    isDiscreteGvTuple _ = False    
    isDiscreteGv (GeneralVariable _ (Labels _)) = True
    isDiscreteGv _ = False

validExpression (FromIntegerParameterSource xs f) = validExpression (FromRealParameterSource (replicate (length xs) 0.1) f)

validExpression (ElementOf _ _) = True

validExpression (Exists (GeneralVariable _ _) f) = 
  codomain f ==  Booleans &&
  validExpression f

validExpression (Restriction (SimpleSubset p) f ) = 
  validExpression f &&
  validExpression p &&
  domain p == domain f

validExpression (Max f) = realCodomain f
validExpression (Min f) = realCodomain f

validExpression ( KroneckerProduct xs ) = all validTupleOfRealValues xs
  where validTupleOfRealValues (Tuple ys) = all validRealValue ys

validExpression (DistributedAccordingTo expr f) = 
  realCodomain f &&
  domain f == codomain expr

validExpression (DistributionFromRealisations xs) =
  all validExpression xs &&
  length (List.nub (map (simplifyFSet . codomain) xs)) == 1


-- Utility methods follow

-- Todo: more comprehensive handling of FSet types, e.g. subset of Cartesian product.
getFactor :: Int -> FSet -> FSet
getFactor n (CartesianProduct xs) = xs !! (n-1)
getFactor 1 m = m


-- Todo: more comprehensive handling of other FSet types, e.g. subset of cartesian power, currently this would come out as 1.
factorCount :: FSet -> Int
factorCount (CartesianProduct ms) = length ms
factorCount _ = 1


-- | Cardinality of discrete space. Zero if can't be easily determined, or has continuous components.

-- Todo: only some cases have been covered, i.e. only the bare minimum as required by existing unit tests.
cardinality :: FSet -> Int
cardinality UnitSpace = 1
cardinality (Labels (IntegerRange a b) ) = b - a + 1
cardinality (CartesianProduct fs) = product (map cardinality fs)
cardinality _ = 0


lambdaLike :: Expression -> Bool
lambdaLike x = not (domain x == UnitSpace)


-- | canonicalSuperset m returns n where m is a simple subset of n, or factors of m are subsets of factors of n.
canonicalSuperset :: FSet -> FSet
canonicalSuperset (CartesianProduct ms) = CartesianProduct (map canonicalSuperset ms)
canonicalSuperset (SimpleSubset f) = canonicalSuperset (domain f)
canonicalSuperset (Image f) = canonicalSuperset (codomain f)
canonicalSuperset (Factor n (CartesianProduct ms)) = canonicalSuperset (ms!!n)
canonicalSuperset m = m

-- | Checks that both expressions are of the same codomain, and are each valid, and are each value-like, not lambda-like.

-- Todo: add a flag to indicate whether lambda's are considered valid or not.
validBinaryOp :: FSet -> Expression -> Expression -> Bool
validBinaryOp m a b =
  validUnaryOp m a &&
  validUnaryOp m b


validUnaryOp :: FSet -> Expression -> Bool
validUnaryOp m x = 
  validExpression x && 
  (canonicalSuperset . simplifyFSet . codomain) x == m &&
  not (lambdaLike x)


realCodomain :: Expression -> Bool
realCodomain x = (canonicalSuperset . simplifyFSet . codomain) x  == Reals


validRealValue :: Expression -> Bool
validRealValue x = validUnaryOp Reals x

