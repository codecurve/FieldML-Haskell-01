module FieldML.Utility01 (
  freeVariables,
  domain,
  codomain,
  expressionType,
  canonicalSuperset,
  simplifyFSet,
  applyVisitor
--  validExpression
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


freeVariables :: (Show a) => Expression a -> [Expression a]
freeVariables (UnitElement _) = []
freeVariables (BooleanConstant _ _) = []
freeVariables (RealConstant _ _ ) = []
freeVariables (LabelValue _ _) = []
freeVariables f@(GeneralVariable _ _ _ ) = [f]
freeVariables (Unspecified _ _) = []
freeVariables (Cast _ x _) = freeVariables x -- Todo: currently ignoring free variables in definition of FSet.
-- freeVariables (Tuple _ xs) = nub (concatMap freeVariables xs) 
freeVariables (Project _ _ x) = freeVariables x

-- Note, could have used more general pattern for Lambda, but the lack of exhaustive pattern matching is serving in the interim as poor man's validation.
-- freeVariables (Lambda _ t@(Tuple _ _) expr1) =  (freeVariables expr1) \\ (freeVariables t)
-- freeVariables (Lambda _ x@(GeneralVariable a _ _) expr1 ) = delete x (freeVariables expr1)
freeVariables x@(Lambda _ _ _) = error ("freeVariables not implemented yet for Lambda for case where bound variable is anything other than variable Tuple. Args:" ++ show x)
freeVariables (Inverse _ f) = freeVariables f
freeVariables (Lambdify _ _) = []
-- freeVariables (Apply _ f x) = nub ((freeVariables f) ++ (freeVariables x) )
freeVariables (Compose a f g) = freeVariables $ Tuple a [ f, g ]
-- freeVariables (PartialApplication _ f n x) = nub ( (freeVariables f) ++ (freeVariables x) )
{-freeVariables (Where a expr xs) = (freeVariables expr) \\ (localVars xs)
  where
    localVars ((Equal a local _):x1s) = (freeVariables local) ++ (localVars x1s)
    localVars [] = []
-}
freeVariables (And a x y) = freeVariables $ Tuple a [ x, y ]
freeVariables (Or a x y) = freeVariables $ Tuple a [ x, y ]
freeVariables (Not _ x) = freeVariables x
freeVariables (LessThan a x y) = freeVariables $ Tuple a [ x, y ]
freeVariables (Equal a x y) = freeVariables $ Tuple a [ x, y ]

freeVariables (Plus a x y) = freeVariables $ Tuple a [ x, y ]
freeVariables (Minus a x y) = freeVariables $ Tuple a [ x, y ]
freeVariables (Negate _ x) = freeVariables x
freeVariables (Times a x y) = freeVariables $ Tuple a [ x, y ]
freeVariables (Divide a x y) = freeVariables $ Tuple a [ x, y ]
freeVariables (Modulus a x y) = freeVariables $ Tuple a [ x, y ]
freeVariables (Sin _ x) = freeVariables x
freeVariables (Cos _ x) = freeVariables x
freeVariables (Exp _ x) = freeVariables x
freeVariables (Power a x y) = freeVariables $ Tuple a [ x, y ]
freeVariables (Pi _) = []
freeVariables (If a x vt vf ) = freeVariables $ Tuple a [ x, vt, vf ]
freeVariables (Max _ f) = freeVariables f
freeVariables (Min _ f) = freeVariables f

freeVariables (ElementOf _ x m) = freeVariables x -- Todo: What if there are free variables in the definition of m? Assuming here and elsewhere that there are not. Could merge FSet and Expression, so that an expression may represent an FSet?
-- freeVariables (Exists _ x@(GeneralVariable _ _ _) f) = delete x (freeVariables f)
freeVariables (Restriction _ _ f) = freeVariables f -- Todo: What if the restriction fixes one of the variables? Is it still free, but only valid if it has that value?
freeVariables (Interior _ _) = [] -- Todo: definition of m in Interior m may have free variables, but we aren't yet processing defintions of FSet.

freeVariables (MultiDimArray _ (AlgebraicVector x) _) = freeVariables x
freeVariables (MultiDimArray _ _ _) = []
freeVariables (Contraction _ a1 _ a2 _) = (freeVariables a1) ++ (freeVariables a2) -- Todo: This assumes that the index selector is always "hard coded", we will possibly in future want to support using an integer expression for the index.
freeVariables (KroneckerProduct a xs) = freeVariables $ Tuple a xs
freeVariables (DistributedAccordingTo a x f) = freeVariables $ Tuple a [ x, f ]
freeVariables (DistributionFromRealisations a xs) = freeVariables $ Tuple a xs

freeVariables x = error ("freeVariables not implemented yet for this constructor. Args:" ++ show x)

-- | Returns the FSet from which a function maps values. Unless it is actually a function, the expression is treated as a value, which is treated as a function from UnitSpace.

-- Todo: make "return type" "Either FSet or InvalidExpression" so that validation can be built in.
-- Todo: Explicit patterns all mentioned at this stage there is no 'catch-all', still using this to provide rudimentary debugging, but would look prettier with the UnitSpace case handled by a catch-all.
domain :: (Show a) => Expression a -> FSet

domain (UnitElement _) = UnitSpace
domain (BooleanConstant _ _) = UnitSpace
domain (RealConstant _ _ ) = UnitSpace
domain (LabelValue _ _ ) = UnitSpace
domain (GeneralVariable _ _ (SignatureSpace m _)) = simplifyFSet m
domain (GeneralVariable _ _ _) = UnitSpace
domain (Unspecified _ _) = UnitSpace
domain (Cast _ _ (SignatureSpace m _)) = m
domain (Cast _ _ _) = UnitSpace
domain (Tuple _ _) = UnitSpace
domain (Project _ n (Tuple _ fs)) = simplifyFSet $ domain (fs!!(n-1))
domain x@(Project _ _ _) = error ("domain not implemented yet for Project from anything other than Tuple. Args:" ++ show x)

domain (Lambda _ (UnitElement _) _) = UnitSpace 
domain (Lambda _ x@(GeneralVariable _ _ _) _ ) = simplifyFSet $ codomain x
domain (Lambda _ t@(Tuple _ _) _ ) = simplifyFSet $ codomain t
domain x@(Lambda _ _ _ ) = error ("domain not implemented yet for Lambda for case where bound variable is anything other than variable Tuple. Args:" ++ show x)
domain (Inverse _ f) = simplifyFSet (codomain f)
domain (Lambdify _ expr) = simplifyFSet $ CartesianProduct $ map fSetOfVariable (freeVariables expr)
domain (Apply _ f _) = effectiveResultingDomain $ (simplifyFSet (codomain f))
  where
    effectiveResultingDomain (SignatureSpace m _) = m
    effectiveResultingDomain _ = UnitSpace

domain (Compose _ _ g) = simplifyFSet $ domain g
domain (PartialApplication _ f n _) = simplifyFSet $ CartesianProduct ((take (n-1) fFactors) ++ (drop n fFactors))
  where
    fFactors = getFactors (domain f)
    getFactors (CartesianProduct ms) = ms
    getFactors m = [m]

domain (Where _ expr _) = simplifyFSet $ domain expr
domain (And _ _ _) = UnitSpace
domain (Or _ _ _) = UnitSpace
domain (Not _ _) = UnitSpace
domain (LessThan _ _ _) = UnitSpace
domain (Equal _ _ _) = UnitSpace
domain (Plus _ _ _) = UnitSpace
domain (Minus _ _ _) = UnitSpace
domain (Negate _ _) = UnitSpace
domain (Times _ _ _) = UnitSpace
domain (Divide _ _ _) = UnitSpace
domain (Modulus _ _ _) = UnitSpace
domain (Sin _ _) = UnitSpace
domain (Cos _ _) = UnitSpace
domain (Exp _ _) = UnitSpace
domain (Power _ _ _) = UnitSpace
domain (Pi _) = UnitSpace
domain (If _ _ _ _ ) = UnitSpace
domain (Max _ _) = UnitSpace
domain (Min _ _) = UnitSpace
domain (ElementOf _ _ _) = UnitSpace
domain (Exists _ _ _) = UnitSpace
domain (Restriction _ m _ ) = simplifyFSet m
domain (Interior _ (SimpleSubset l@(Lambda _ _ _)) ) = simplifyFSet $ domain l
domain x@(Interior _ _) = error ("domain not implemented yet for Interior for anything other than SimpleSubset of a Lambda. Args:" ++ show x)
domain (MultiDimArray _ _ m) = simplifyFSet m
domain (Contraction a a1 n1 a2 n2) = 
  CartesianProduct [
    domain (PartialApplication a a1 n1 boundIndexVariable),
    domain (PartialApplication a a2 n2 boundIndexVariable)
  ]
  where
    boundIndexVariable = GeneralVariable a "boundIndexVariable" (getFactor n1 (domain a1)) --Todo: Assumes this is the same as (getFactor n2 (domain a2)).
    
domain (KroneckerProduct _ _) = UnitSpace
domain (DistributedAccordingTo _ _ _ ) = UnitSpace
domain (DistributionFromRealisations _ xs ) = simplifyFSet $ codomain (head xs) -- Note: this Assumes all xs also have codomain same as head xs.  This is checked by validateExpression.
-- Todo: breaks if xs is []

domain x = error ("domain not implemented yet for this constructor. Args:" ++ show x)


-- | Returns the FSet to which a function maps values. Even if it is actually just a value expression, rather than a function, the expression is treated as a function from UnitSpace, and then the codomain is the 'type' of the value.

-- Todo: make "return type" "Either FSet or InvalidExpression" so that validation can be built in.  
-- Todo: make it so that it can be assumed that codomain has simplified the FSet before returning it, same for domain
codomain :: (Show a) => (Expression a) -> FSet

codomain (UnitElement _) = UnitSpace
codomain (BooleanConstant _ _) = Booleans
codomain (RealConstant _ _ ) = Reals
codomain (LabelValue _ (StringLabel _ m)) = Labels m
codomain (LabelValue _ (IntegerLabel _ m)) = Labels m

codomain (GeneralVariable _ _ m) = m
codomain (Unspecified _ m) = m
codomain (Cast _ _ (SignatureSpace _ m)) = m
codomain (Cast _ _ m) = m
codomain (Tuple _ fs) = CartesianProduct (map codomain fs)
codomain (Project _ n f) = getFactor n (codomain f)
codomain (Lambda _ _ expr ) 
  | lambdaLike expr = SignatureSpace (domain expr) (codomain expr)
  | otherwise = codomain expr
codomain (Inverse _ f) = domain f
codomain (Lambdify _ expr) = codomain expr

codomain (Apply _ f _) = effectiveResultingCodomain $ (simplifyFSet (codomain f))
  where
    effectiveResultingCodomain (SignatureSpace _ n) = n
    effectiveResultingCodomain _ = simplifyFSet $ codomain f

codomain (Compose _ f _) = simplifyFSet $ codomain f
codomain (PartialApplication _ f _ _) = simplifyFSet $ codomain f
codomain (Where _ expr _) = simplifyFSet $ codomain expr

codomain (And _ _ _) = Booleans
codomain (Or _ _ _) = Booleans
codomain (Not _ _) = Booleans
codomain (LessThan _ _ _) = Booleans
codomain (Equal _ _ _) = Booleans
codomain (Plus _ _ _) = Reals
codomain (Minus _ _ _) = Reals
codomain (Negate _ _) = Reals
codomain (Times _ _ _) = Reals
codomain (Divide _ _ _) = Reals
codomain (Modulus _ _ _) = Reals
codomain (Sin _ _) = Reals
codomain (Cos _ _) = Reals
codomain (Exp _ _) = Reals
codomain (Power _ _ _) = Reals
codomain (Pi _) = Reals
codomain (If _ _ x _ ) = codomain x
codomain (Max _ _) = Reals
codomain (Min _ _) = Reals
codomain (ElementOf _ _ _) = Booleans
codomain (Exists _ _ _) = Booleans
codomain (Restriction _ _ f ) = codomain f
codomain (Interior _ _) = Booleans
codomain (MultiDimArray _ (RealParameterVector _) _) = Reals
codomain (MultiDimArray _ (IntegerParameterVector _ m) _) = m
codomain (MultiDimArray _ (AlgebraicVector (Tuple _ (x:xs))) _) = expressionType x
codomain (MultiDimArray _ (AlgebraicVector x) _) = expressionType x
codomain (Contraction _ _ _ _ _) = Reals
codomain (KroneckerProduct a fs) = CartesianProduct (replicate m Reals)
  where
    m = product ( map tupleLength fs )
    tupleLength (Tuple a gs) = length gs
    tupleLength (Apply a (Lambda _ _ (Tuple _ gs)) _ ) = length gs
    tupleLength x = error ("codomain.tupleLength not implemented yet for this constructor. Args:" ++ show x)
    -- Todo: Should consider perhaps having an expression simplifier that performs the substitution that an Apply represents. See also validTupleOfRealValues.

codomain (DistributedAccordingTo _ _ _ ) = Booleans
codomain (DistributionFromRealisations _ _) = Reals

codomain x = error ("codomain not implemented yet for this constructor. Args:" ++ show x)


-- | True if expression passes a limited set of tests.  Note: this is under construction, so sometimes an expression is reported as valid, even if it is not valid.

-- | Apply a visitor to an expression tree to get a new expression tree with the results of visiting each node of the expressin tree stored at the corresponding node in the new Expression tree.
applyVisitor :: (Expression a -> b) -> Expression a -> Expression b

applyVisitor v x1@(UnitElement _) = UnitElement (v x1)
applyVisitor v x1@(BooleanConstant _ x) = BooleanConstant (v x1) x
applyVisitor v x1@(RealConstant _ x) = RealConstant (v x1) x



applyVisitor v t@(Tuple _ xs) = Tuple (v t) (map (applyVisitor v) xs)

{-
applyVisitor (Project _ n x) = Project validity n (applyVisitor x)
  where
    validity = (validExpression x) && factorCount (codomain x) >= n

applyVisitor (Lambda _ x expr ) = Lambda validity (applyVisitor x) (applyVisitor expr)
  where 
    validity = 
      (isVariableTuple x) && 
      (validExpression x) && 
      (validExpression expr)

    isVariableTuple (GeneralVariable _ _) = True
    isVariableTuple (Tuple xs) = all isVariableTuple xs
    isVariableTuple _ = False

-- Todo: Other expressions are lambda like, and can be inverted, add their cases.  Probably will treat inverse of values that are not lambda-like as invalid though.
applyVisitor (Inverse _ f) = Inverse v (applyVisitor f)
  where v = validExpression f && lambdaLike f

applyVisitor (Lambdify _ expr) = Lambdify v (applyVisitor expr)
  where v = validExpression expr && not (lambdaLike expr) -- Todo: Not sure if the restriction that expr is "not lambda-like" is necessary.

applyVisitor (Apply f x) = Apply v (applyVisitor f) (applyVisitor x)
  where v = 
    lambdaLike f &&
    codomain x == domain f &&
    validExpression f &&
    validExpression x

applyVisitor (Compose f g) = Compose v (applyVisitor f) (applyVisitor g)
  where v = 
    lambdaLike f &&
    lambdaLike g &&
    validExpression f &&
    validExpression g &&
    codomain g == domain f

applyVisitor (PartialApplication _ f n x) = PartialApplication v (Validatedexpression f) n (Validatedexpression x)
  where v =
    lambdaLike f &&
    factorCount (domain f) >= n &&
    canonicalSuperset (codomain x) == getFactor n (domain f) &&
    validExpression f &&
    validExpression x

applyVisitor (Where _ expr locals) = Where v (applyVisitor expr) (map applyVisitor locals)
  where 
    v = 
      validExpression expr && 
      all validExpression locals &&
      all localVarAssignment locals

    localVarAssignment (Equal _ (GeneralVariable _ _ _) _) = True
    localVarAssignment _ = False

applyVisitor (And _ x y) = And v (applyVisitor x) (applyVisitor y)
  where v = validBinaryOp Booleans x y

applyVisitor (Or  _ x y) = Or  v (applyVisitor x) (applyVisitor y)
  where v = validBinaryOp Booleans x y

applyVisitor (Not _ x) = Not v (applyVisitor x)
  where v = 
    validExpression x &&
    codomain x == Booleans &&
    not (lambdaLike x)

applyVisitor (LessThan _ x y) = LessThan v (applyVisitor x) (applyVisitor y)
  where v = validBinaryOp Reals x y

applyVisitor (Equal _ x y) = Equal v (applyVisitor x) (applyVisitor y)
  where v = 
    validExpression x &&
    validExpression y &&
    canonicalSuperset (codomain x) == canonicalSuperset (codomain y) &&
    not (lambdaLike x) &&
    not (lambdaLike y)

applyVisitor (Plus    _ x y) = Plus    (validBinaryOp Reals x y) (applyVisitor x) (applyVisitor y)
applyVisitor (Minus   _ x y) = Minus   (validBinaryOp Reals x y) (applyVisitor x) (applyVisitor y)
applyVisitor (Times   _ x y) = Times   (validBinaryOp Reals x y) (applyVisitor x) (applyVisitor y)
applyVisitor (Divide  _ x y) = Divide  (validBinaryOp Reals x y) (applyVisitor x) (applyVisitor y)
applyVisitor (Modulus _ x y) = Modulus (validBinaryOp Reals x y) (applyVisitor x) (applyVisitor y)
applyVisitor (Power   _ x y) = Power   (validBinaryOp Reals x y) (applyVisitor x) (applyVisitor y)

applyVisitor (Negate _ x) = Negate (validUnaryOp Reals x) (applyVisitor x)
applyVisitor (Sin    _ x) = Sin    (validUnaryOp Reals x) (applyVisitor x)
applyVisitor (Cos    _ x) = Cos    (validUnaryOp Reals x) (applyVisitor x)
applyVisitor (Exp    _ x) = Exp    (validUnaryOp Reals x) (applyVisitor x)

applyVisitor Pi _ =  Pi True

applyVisitor (If _ x vt vf) = If v (applyVisitor x) (applyVisitor vt) (applyVisitor vf)
  where v = 
    validExpression x && 
    validExpression vt && 
    validExpression vf && 
    codomain vt == codomain vf &&
    codomain x == Booleans &&
    not (lambdaLike x)

applyVisitor (Max _ f) = Max (realCodomain f) (applyVisitor f)
applyVisitor (Min _ f) = Min (realCodomain f) (applyVisitor f)

applyVisitor (ElementOf _ x y) = ElementOf True (applyVisitor x) (applyVisitor y)

applyVisitor (Exists _ x@(GeneralVariable _ _ _) f) = Exists v (applyVisitor x) (applyVisitor f)
  where v = 
    codomain f ==  Booleans &&
    validExpression f
  
validExpression x@(Exists _ _ _) = error ("applyVisitor not implemented yet for Exists for case where bound variable is anything other than GeneralVariable. Args:" ++ show x)

applyVisitor (Restriction _ s@(SimpleSubset p) f ) = Restriction v s (applyVisitor f)
  where v = 
    lambdaLike f &&
    validExpression f &&
    validExpression p &&
    domain p == domain f
  
validExpression x@(Restriction _ _ _) = error ("applyVisitor not implemented yet for Restriction for case where restriction FSet is anything other than SimpleSubset. Args:" ++ show x)

applyVisitor (Interior _ m) = Interior True m -- Todo: validate the FSet operand.

applyVisitor (MultiDimArray _ v m) = MultiDimArray validity (applyVisitor v) m
  where
    validity = ((isDiscreteFSet m) || (isProductOfDFSs m))  && validateCardinality && validVector v
    validateCardinality = (cardinality m == vectorLength v)    
    isDiscreteFSet (Labels _) = True
    isDiscreteFSet _ = False
    isProductOfDFSs (CartesianProduct ms) = all isDiscreteFSet ms
    isProductOfDFSs _ = False

applyVisitor (Contraction _ a1 n1 a2 n2) = Contraction v (applyVisitor a1) n1 (applyVisitor a2) n2
  where v =
    lambdaLike a1 &&
    lambdaLike a2 &&
    validExpression a1 &&
    validExpression a2 &&
    codomain a1 == Reals &&
    codomain a2 == Reals &&
    n1 <= factorCount m1 &&
    n2 <= factorCount m2 &&
    getFactor n1 m1 == getFactor n2 m2
    m1 = domain a1 
    m2 = domain a2 

applyVisitor (KroneckerProduct _ xs) = KroneckerProduct v (map applyVisitor xs)
  where 
    v = all validTupleOfRealValues xs
    validTupleOfRealValues (Tuple ys) = all validRealValue ys
    validTupleOfRealValues (Apply (Lambda _ expr) _) = validTupleOfRealValues expr
    -- Todo: see comment made at codomain for KroneckerProduct

applyVisitor (DistributedAccordingTo _ expr f) = DistributedAccordingTo v (applyVisitor expr) (applyVisitor f)
  where v = 
    realCodomain f && 
    domain f == codomain expr

applyVisitor (DistributionFromRealisations _ xs) = DistributionFromRealisations v (map applyVisitor xs)
  where v =  
    all validExpression xs &&
    length (nub (map (simplifyFSet . codomain) xs)) == 1

applyVisitor v x = v x 
-}
-- Secondary utility methods follow
validatingVisitor :: (Expression a) -> Bool
validatingVisitor (UnitElement _) = True
validatingVisitor (BooleanConstant _ x) = True
validatingVisitor (RealConstant _ x) = True
{-
validatingVisitor (LabelValue _ c@(StringLabel  x  (StringLabels xs) )) = LabelValue validity c
  where
    validity = x `elem` (Set.toList xs)

validatingVisitor (LabelValue c@(IntegerLabel x (IntegerRange a b) )) = LabelValue validity c
  where
    validity = a <= x && x <= b

validatingVisitor (LabelValue _ c@(IntegerLabel _ Integers )) = LabelValue True c
validatingVisitor (LabelValue _ c@(IntegerLabel x (DiscreteSetUnion n1 n2))) = LabelValue validity c
  where
    validity = 
      validExpression (LabelValue () (IntegerLabel x n1)) ||
      validExpression (LabelValue () (IntegerLabel x n2))

validatingVisitor (LabelValue c@(IntegerLabel x (Intersection n1 n2))) = LabelValue validity c
  where
    validity =
      validExpression (LabelValue () (IntegerLabel x n1)) &&
      validExpression (LabelValue () (IntegerLabel x n2))
-}
validatingVisitor (GeneralVariable _ _ x) = True -- Todo: Could validate the name of the variable according to some rules for identifier names.
validatingVisitor (Unspecified _ m) = True -- Todo: Could validate the FSet m when FSet validation is implemented one day.

validatingVisitor (Tuple _ xs) = all validatingVisitor xs

-- validatingVisitor (Cast _ x s@(SignatureSpace m n)) = Cast (validExpression x) (applyVisitor x) s -- Todo: Major omission here: a lot of work is probably required to validate all possibilities.




-- | Given an expression, return True if the expression is valid, false otherwise.
{-
validExpression :: Expression _ -> Bool
validExpression x = v
  where
   Expression v = validExpression x

-}

-- | Returns True if vector is valid.

--Todo: Vector construction is just a kind of expression, this really hints at using typeclasses, and having a "isValid" function for Vectors, Expressions and FSets.
validVector :: SimpleVector a -> Bool

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
vectorLength :: SimpleVector a -> Int
vectorLength (AlgebraicVector (Tuple _ xs)) = length xs

vectorLength (AlgebraicVector (Apply _ (Lambda _ _ x1) _) ) = vectorLength (AlgebraicVector x1) -- Todo: This is along the lines of algebraic manipulation of the expression, and should probably be extracted.

vectorLength (AlgebraicVector _) = 1
vectorLength (RealParameterVector xs) = length xs
vectorLength (IntegerParameterVector xs _) = length xs


fSetOfVariable :: Expression a -> FSet
fSetOfVariable (GeneralVariable _ _ m) = m

-- | Simply wraps a lambda like expression's domain and codomain in SignatureSpace, and for all others, the codomain FSet is returned directly.
expressionType :: (Show a) => Expression a -> FSet
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


lambdaLike :: (Show a) => Expression a -> Bool
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
{-
validBinaryOp :: FSet -> Expression -> Expression -> Bool
validBinaryOp m a b =
  validUnaryOp m a &&
  validUnaryOp m b


validUnaryOp :: FSet -> Expression -> Bool
validUnaryOp m x = 
  validExpression x && 
  (canonicalSuperset . simplifyFSet . codomain) x == m &&
  not (lambdaLike x)

-}
realCodomain :: (Show a) => Expression a -> Bool
realCodomain x = (canonicalSuperset . simplifyFSet . codomain) x  == Reals

{-
validRealValue :: Expression -> Bool
validRealValue x = validUnaryOp Reals x
-}
