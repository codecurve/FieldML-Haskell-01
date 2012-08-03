module FieldML.Utility01 (
  freeVariables,
  domain,
  codomain,
  expressionType,
  canonicalSuperset,
  simplifyFSet,
  validExpression
)
where

import FieldML.Core

import Data.List ( delete, nub, (\\) )
import qualified Data.Set as Set

-- Focus here is on *processing* the FieldML data structures.  

-- | simplifyFSet m will attempt to produce a new FSet that is equivalent to m, but has a simpler definition.
simplifyFSet :: FSet -> FSet
simplifyFSet (Factor n (CartesianProduct ys)) = ys !! (n-1)
simplifyFSet (CartesianProduct []) = UnitSpace
simplifyFSet (CartesianProduct [m]) = m
simplifyFSet (SignatureSpace UnitSpace m) = m
simplifyFSet m = m


freeVariables :: Expression -> [Expression]
freeVariables UnitElement = []
freeVariables (BooleanConstant _) = []
freeVariables (RealConstant _ ) = []
freeVariables (LabelValue _) = []
freeVariables f@(GeneralVariable _ _ ) = [f]
freeVariables (Unspecified _) = []
freeVariables (Tuple xs) = nub (concatMap freeVariables xs) 
freeVariables (Project _ x) = freeVariables x

-- Note, could have used more general pattern for Lambda, but the lack of exhaustive pattern matching is serving in the interim as poor man's validation.
freeVariables (Lambda t@(Tuple _) expr1) =  (freeVariables expr1) \\ (freeVariables t)
freeVariables (Lambda x@(GeneralVariable _ _) expr1 ) = delete x (freeVariables expr1)
freeVariables x@(Lambda _ _) = error ("freeVariables not implemented yet for Lambda for case where bound variable is anything other than variable Tuple. Args:" ++ show x)
freeVariables (Inverse f) = freeVariables f
freeVariables (Lambdify _) = []
freeVariables (Apply f x) = nub ((freeVariables f) ++ (freeVariables x) )
freeVariables (Compose f g) = freeVariables $ Tuple [ f, g ]
freeVariables (PartialApplication f n x) = nub ( (freeVariables f) ++ (freeVariables x) )
freeVariables (Where expr xs) = (freeVariables expr) \\ (localVars xs)
  where
    localVars ((local `Equal` _):x1s) = (freeVariables local) ++ (localVars x1s)
    localVars [] = []

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
freeVariables (Exists x@(GeneralVariable _ _) f) = delete x (freeVariables f)
freeVariables (Restriction _ f) = freeVariables f -- Todo: What if the restriction fixes one of the variables? Is it still free, but only valid if it has that value?
freeVariables (Interior _) = [] -- Todo: definition of m in Interior m may have free variables, but we aren't yet processing defintions of FSet.

freeVariables (MultiDimArray (AlgebraicVector x) _) = freeVariables x
freeVariables (MultiDimArray _ _) = []
freeVariables (Contraction a1 _ a2 _) = (freeVariables a1) ++ (freeVariables a2) -- Todo: This assumes that the index selector is always "hard coded", we will possibly in future want to support using an integer expression for the index.
freeVariables (KroneckerProduct xs) = freeVariables $ Tuple xs
freeVariables (DistributedAccordingTo x f) = freeVariables $ Tuple [ x, f ]
freeVariables (DistributionFromRealisations xs) = freeVariables $ Tuple xs

freeVariables x = error ("freeVariables not implemented yet for this constructor. Args:" ++ show x)

-- | Returns the FSet from which a function maps values. Unless it is actually a function, the expression is treated as a value, which is treated as a function from UnitSpace.

-- Todo: make "return type" "Either FSet or InvalidExpression" so that validation can be built in.
-- Todo: Explicit patterns all mentioned at this stage there is no 'catch-all', still using this to provide rudimentary debugging, but would look prettier with the UnitSpace case handled by a catch-all.
domain :: Expression -> FSet

domain UnitElement = UnitSpace
domain (BooleanConstant _) = UnitSpace
domain (RealConstant _ ) = UnitSpace
domain (LabelValue _ ) = UnitSpace
domain (GeneralVariable _ (SignatureSpace m _)) = simplifyFSet m
domain (GeneralVariable _ _) = UnitSpace
domain (Unspecified _) = UnitSpace
domain (Tuple _) = UnitSpace
domain (Project n (Tuple fs)) = simplifyFSet $ domain (fs!!(n-1))
domain x@(Project _ _) = error ("domain not implemented yet for Project from anything other than Tuple. Args:" ++ show x)

domain (Lambda UnitElement _) = UnitSpace 
domain (Lambda x@(GeneralVariable _ _) _ ) = simplifyFSet $ codomain x
domain (Lambda t@(Tuple _) _ ) = simplifyFSet $ codomain t
domain x@(Lambda _ _ ) = error ("domain not implemented yet for Lambda for case where bound variable is anything other than variable Tuple. Args:" ++ show x)
domain (Inverse f) = simplifyFSet (codomain f)
domain (Lambdify expr) = simplifyFSet $ CartesianProduct $ map fSetOfVariable (freeVariables expr)
domain (Apply f _) = effectiveResultingDomain $ (simplifyFSet (codomain f))
  where
    effectiveResultingDomain (SignatureSpace m _) = m
    effectiveResultingDomain _ = UnitSpace

domain (Compose _ g) = simplifyFSet $ domain g
domain (PartialApplication f n _) = simplifyFSet $ CartesianProduct ((take (n-1) fFactors) ++ (drop n fFactors))
  where
    fFactors = getFactors (domain f)
    getFactors (CartesianProduct ms) = ms
    getFactors m = [m]

domain (Where expr _) = simplifyFSet $ domain expr
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
domain (Restriction m _ ) = simplifyFSet m
domain (Interior (SimpleSubset l@(Lambda _ _)) ) = simplifyFSet $ domain l
domain x@(Interior _) = error ("domain not implemented yet for Interior for anything other than SimpleSubset of a Lambda. Args:" ++ show x)
domain (MultiDimArray _ m) = simplifyFSet m
domain (Contraction a1 n1 a2 n2) = 
  CartesianProduct [
    domain (PartialApplication a1 n1 boundIndexVariable),
    domain (PartialApplication a2 n2 boundIndexVariable)
  ]
  where
    boundIndexVariable = GeneralVariable "boundIndexVariable" (getFactor n1 (domain a1)) --Todo: Assumes this is the same as (getFactor n2 (domain a2)).
    
domain (KroneckerProduct _) = UnitSpace
domain (DistributedAccordingTo _ _ ) = UnitSpace
domain (DistributionFromRealisations xs ) = simplifyFSet $ codomain (head xs) -- Note: this Assumes all xs also have codomain same as head xs.  This is checked by validExpression.
-- Todo: breaks if xs is []

domain x = error ("domain not implemented yet for this constructor. Args:" ++ show x)


-- | Returns the FSet to which a function maps values. Even if it is actually just a value expression, rather than a function, the expression is treated as a function from UnitSpace, and then the codomain is the 'type' of the value.

-- Todo: make "return type" "Either FSet or InvalidExpression" so that validation can be built in.  
-- Todo: make it so that it can be assumed that codomain has simplified the FSet before returning it, same for domain
codomain :: Expression -> FSet

codomain UnitElement = UnitSpace
codomain (BooleanConstant _) = Booleans
codomain (RealConstant _ ) = Reals
codomain (LabelValue (StringLabel _ m)) = Labels m
codomain (LabelValue (IntegerLabel _ m)) = Labels m

codomain (GeneralVariable _ m) = m
codomain (Unspecified m) = m
codomain (Tuple fs) = CartesianProduct (map codomain fs)
codomain (Project n f) = getFactor n (codomain f)
codomain (Lambda _ expr ) 
  | lambdaLike expr = SignatureSpace (domain expr) (codomain expr)
  | otherwise = codomain expr
codomain (Inverse f) = domain f
codomain (Lambdify expr) = codomain expr

codomain (Apply f _) = effectiveResultingCodomain $ (simplifyFSet (codomain f))
  where
    effectiveResultingCodomain (SignatureSpace _ n) = n
    effectiveResultingCodomain _ = simplifyFSet $ codomain f

codomain (Compose f _) = simplifyFSet $ codomain f
codomain (PartialApplication f _ _) = simplifyFSet $ codomain f
codomain (Where expr _) = simplifyFSet $ codomain expr

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
codomain (MultiDimArray (RealParameterVector _) _) = Reals
codomain (MultiDimArray (IntegerParameterVector _ m) _) = m
codomain (MultiDimArray (AlgebraicVector (Tuple (x:xs))) _) = expressionType x
codomain (MultiDimArray (AlgebraicVector x) _) = expressionType x
codomain (Contraction _ _ _ _) = Reals
codomain (KroneckerProduct fs ) = CartesianProduct (replicate m Reals)
  where
    m = product ( map tupleLength fs )
    tupleLength (Tuple gs) = length gs
    tupleLength (Apply (Lambda _ (Tuple gs)) _ ) = length gs
    tupleLength x = error ("codomain.tupleLength not implemented yet for this constructor. Args:" ++ show x)
    -- Todo: Should consider perhaps having an expression simplifier that performs the substitution that an Apply represents. See also validTupleOfRealValues.

codomain (DistributedAccordingTo _ _ ) = Booleans
codomain (DistributionFromRealisations _) = Reals

codomain x = error ("codomain not implemented yet for this constructor. Args:" ++ show x)


-- | True if expression passes a limited set of tests.  Note: this is under construction, so sometimes an expression is reported as valid, even if it is not valid.
validExpression :: Expression -> Bool

validExpression UnitElement = True
validExpression (BooleanConstant _) = True
validExpression (RealConstant _ ) = True
validExpression (LabelValue (StringLabel  x  (StringLabels xs) )) = x `elem` (Set.toList xs)
validExpression (LabelValue (IntegerLabel x (IntegerRange a b) )) = a <= x && x <= b
validExpression (LabelValue (IntegerLabel _ Integers )) = True
validExpression (LabelValue (IntegerLabel x (DiscreteSetUnion n1 n2))) = 
  validExpression (LabelValue (IntegerLabel x n1))  ||
  validExpression (LabelValue (IntegerLabel x n2))
validExpression (LabelValue (IntegerLabel x (Intersection n1 n2))) = 
  validExpression (LabelValue (IntegerLabel x n1))  &&
  validExpression (LabelValue (IntegerLabel x n2))

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

validExpression (PartialApplication f n x) =
  lambdaLike f &&
  factorCount (domain f) >= n &&
  canonicalSuperset (codomain x) == getFactor n (domain f) &&
  validExpression f &&
  validExpression x

validExpression (Where expr locals) = 
  validExpression expr &&
  all validExpression locals &&
  all localVarAssignment locals
    where 
      localVarAssignment (Equal (GeneralVariable _ _) _) = True
      localVarAssignment _ = False
  
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

validExpression (Max f) = realCodomain f
validExpression (Min f) = realCodomain f

validExpression (ElementOf _ _) = True

validExpression (Exists (GeneralVariable _ _) f) = 
  codomain f ==  Booleans &&
  validExpression f

validExpression (Restriction (SimpleSubset p) f ) = 
  lambdaLike f &&
  validExpression f &&
  validExpression p &&
  domain p == domain f

validExpression (Interior _) = True -- Todo: validate the FSet operand.

validExpression (MultiDimArray v m) = ((isDiscreteFSet m) || (isProductOfDFSs m))  && validateCardinality && validVector v
  where
    validateCardinality = (cardinality m == vectorLength v)    
    isDiscreteFSet (Labels _) = True
    isDiscreteFSet _ = False
    isProductOfDFSs (CartesianProduct ms) = all isDiscreteFSet ms
    isProductOfDFSs _ = False

validExpression (Contraction a1 n1 a2 n2) = 
  lambdaLike a1 &&
  lambdaLike a2 &&
  validExpression a1 &&
  validExpression a2 &&
  codomain a1 == Reals &&
  codomain a2 == Reals &&
  n1 <= factorCount m1 &&
  n2 <= factorCount m2 &&
  getFactor n1 m1 == getFactor n2 m2
  where
    m1 = domain a1 
    m2 = domain a2 

validExpression ( KroneckerProduct xs ) = all validTupleOfRealValues xs
  where 
    validTupleOfRealValues (Tuple ys) = all validRealValue ys
    validTupleOfRealValues (Apply (Lambda _ expr) _) = validTupleOfRealValues expr
    -- Todo: see comment made at codomain for KroneckerProduct

validExpression (DistributedAccordingTo expr f) = 
  realCodomain f &&
  domain f == codomain expr

validExpression (DistributionFromRealisations xs) =
  all validExpression xs &&
  length (nub (map (simplifyFSet . codomain) xs)) == 1


-- Secondary utility methods follow
-- | Returns True if vector is valid.

--Todo: Vector construction is just a kind of expression, this really hints at using typeclasses, and having a "isValid" function for Vectors, Expressions and FSets.
validVector :: SimpleVector -> Bool

validVector (IntegerParameterVector xs (Labels intLabels)) =
  all (inLabels intLabels) xs
  where
    inLabels (IntegerRange x1 x2) x = x >= x1 && x <= x2
    inLabels Integers x = True
    inLabels (DiscreteSetUnion n1 n2) x = inLabels n1 x || inLabels n2 x
    inLabels (Intersection n1 n2) x = inLabels n1 x && inLabels n2 x
    inLabels _ _ = False

validVector _ = True

-- | Returns the length of the various types of vectors.
vectorLength :: SimpleVector -> Int
vectorLength (AlgebraicVector (Tuple xs)) = length xs

vectorLength (AlgebraicVector (Apply (Lambda _ x1) _) ) = vectorLength (AlgebraicVector x1) -- Todo: This is along the lines of algebraic manipulation of the expression, and should probably be extracted.

vectorLength (AlgebraicVector _) = 1
vectorLength (RealParameterVector xs) = length xs
vectorLength (IntegerParameterVector xs _) = length xs


fSetOfVariable :: Expression -> FSet
fSetOfVariable (GeneralVariable _ a) = a

-- | Simply wraps a lambda like expression's domain and codomain in SignatureSpace, and for all others, the codomain FSet is returned directly.
expressionType :: Expression -> FSet
expressionType x
  | lambdaLike x = SignatureSpace (domain x) (codomain x)
  | otherwise = simplifyFSet (codomain x)
    

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
canonicalSuperset (Factor n (CartesianProduct ms)) = canonicalSuperset (ms!!(n-1))
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

