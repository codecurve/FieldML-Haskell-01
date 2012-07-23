module FieldML.Core
where

import qualified Data.Set as Set

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
  -- In general, a Signature space is not a topological space, but some special cases are, 
  -- for example a Banach space (i.e. a complete normed vector space) is a topological space.
  SignatureSpace FSet FSet |

  -- | To allow for recursive declarations, assumes that mapping from names to FSet's exists somewhere

  -- Todo: implement list of names that are 'in scope'.
  FSetVariable String

  deriving (Show, Eq)



-- | Expressions represent algebraic expressions, lambdas, and function applications. Expressions that are not "lambdaLike" are treated as values.
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
  
  -- | If h = PartialApplication f n x then the domain of h is the same as the domain of f 
  -- but with the n-th factor removed from the domain, and the value from x used for that slot.
  -- Note that this equivalent to function application if f's domain is a single factor domain.
  PartialApplication Expression Int Expression |

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
  
  -- | Represents a multidimensional array, reshaped so that it is indexed by the factors of the FSet.
  -- f = MultiDimArray v m assumes that m is an discrete FSet, or the CartesianProduct of n discrete FSets,
  -- with a total cardinality equal to length v.
  -- f is a lambda from m to the value type of v's elements.
  MultiDimArray SimpleVector FSet |

  -- | Contraction x1 i1 x2 i2 requires x1 and x2 to be "MultiDimArray"s. 
  -- It represents the sum over i of x1_i * x2_i, where x1 is indexed by its i1'th index, and x2 by its i2'th index.
  -- The result is indexed by the remaining indices of x1 and x2.
  -- I.e. the result is a lambda from CartesianProduct m1 m2, where m1 is the domain of x1 with the i1'th factor removed, and similarly for m2.
  -- x1 and x2 are expected to be "MultiDimArray"s.
  Contraction Expression Int Expression Int |

  -- | y = KroneckerProduct xs requires each x in xs to be a MultiDimArray with a single index (essentially a vector).
  -- For AlgebraicVector, each member of the Tuple must have Reals as its codomain.
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


-- | A simple vector here is a prototype for how data sources will be wrapped. It is used as the basis for a MultiDimArray. 
-- It also allows Tuples to be represented as a vector.

-- Todo: Consider just having these 3 constructors under Expression.
data SimpleVector = 
  -- | A prototype placeholder for the numerical data that will be available from external sources, e.g. HDF5 etc.
  RealParameterVector [Double] | 

  -- | Similar to RealParameterSource
  IntegerParameterVector [Int] |

  -- | AlgebraicVector (Tuple xs) represents a vector whose length is the same as the length of xs, 
  -- with each member of the tuple being the corresponding element of the vector.
  -- Otherwise, AlgebraicVector x represents a vector with a single element

  -- Todo: Consider just having Tuple for this, and validation checks for homogeneity.
  AlgebraicVector Expression

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


