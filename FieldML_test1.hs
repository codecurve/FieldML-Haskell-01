{-# LANGUAGE CPP, TemplateHaskell #-}

module FieldML_test1
where

import FieldML.Core
import Control.Monad (unless)
import Data.List (stripPrefix)
import qualified Data.Set as Set
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)

testMain = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure


-- Tests
real2 = CartesianProduct [Reals, Reals]
real3 = CartesianProduct [Reals, Reals, Reals]

elementIds = IntegerRange 1 4
  
m2 = CartesianProduct [real2, Labels elementIds]

x = GeneralVariable "x" Reals

expression1 :: Expression
expression1 =  Lambda x
  (  (x `LessThan` (RealConstant 1) )
     `And` 
     ( (RealConstant 0) `LessThan` x)
  )   
  

-- Todo: get a chart for a topological space, and name the coordinates in the chart so that they can be mapped to the free variables of a real expression.
-- But perhaps the chart is just the tuple that represents a value in the topological space?  Tuples can consist of named variables.

unitLineSegment = SimpleSubset expression1

prop_test_BooleanExpression1a = (validExpression expression1)
prop_test_BooleanExpression1b = (freeVariables expression1 == [])

Lambda _ expression1_lambdaRhs = expression1
prop_test_BooleanExpression1c = (freeVariables expression1_lambdaRhs == [GeneralVariable "x" Reals])

prop_test_BooleanExpression1d = (domain expression1 == Reals)
prop_test_BooleanExpression1e = (codomain expression1 == Booleans)

prop_test_Subset1a = (canonicalSuperset unitLineSegment == Reals)

-- As above, but more inline:
unitLineSegment' = 
  SimpleSubset (
    Lambda x ((x `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` x))
  )

prop_test_match1a = (unitLineSegment == unitLineSegment')

xy = Tuple [GeneralVariable "x" Reals, GeneralVariable "y" Reals]
  
expression2 :: Expression
expression2 = Lambda xy
  (  
    ((Project 1 xy) `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` (Project 1 xy)) 
    `And`
    ((Project 2 xy) `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` (Project 2 xy))
  )  

prop_test_2dTupleMapDomain1a = (domain expression2 == CartesianProduct [Reals,Reals] )
prop_test_2dTupleMapDomain1b = (freeVariables expression2 == [] )

Lambda _ expression2_lambdaRhs = expression2
prop_test_2dTupleMapDomain1c = (freeVariables expression2_lambdaRhs == [GeneralVariable "x" Reals,GeneralVariable "y" Reals] )
  
prop_test_2dTupleMapDomain1d = (validExpression expression2)

xi1 = GeneralVariable "ξ1" unitLineSegment

expression3a :: Expression
expression3a = (RealConstant 1) `Minus` xi1

expression3b :: Expression
expression3b =  xi1
  
-- By the way, this is a 1D linear lagrange interpolation basis.
expression3c = Lambda xi1 (Tuple [expression3a, expression3b])

prop_test_LambdaTuple_domain = ( domain expression3c == unitLineSegment )
prop_test_LambdaTuple_codomain = ( canonicalSuperset (codomain expression3c) == CartesianProduct [Reals,Reals] )
prop_test_LambdaTuple_freeVariables = ( freeVariables expression3c == [] )
prop_test_LambdaTuple_valid = ( validExpression expression3c )

Lambda _ expression3c_lambdaRhs = expression3c
prop_test_Tuple_freeVariables = ( freeVariables expression3c_lambdaRhs == [ xi1 ] )

expression4 :: Expression
expression4 = 
  Lambda xy (
    ( (RealConstant 0) `LessThan` GeneralVariable "x" Reals )
    `And`
    ( (RealConstant 0) `LessThan` GeneralVariable "y" Reals )
    `And`
    ( ( GeneralVariable "x" Reals `Plus` GeneralVariable "y" Reals ) `LessThan` (RealConstant 1) )
  )

simplex2d = SimpleSubset expression4

prop_test_Simplex2dPredicate = (domain expression4 == CartesianProduct[Reals, Reals])
  
expression5 :: Expression
expression5 = 
  Lambda xy (
    (GeneralVariable "x" Reals `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` GeneralVariable "x" Reals) 
    `And`
    (GeneralVariable "y" Reals `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` GeneralVariable "y" Reals)
  )

prop_test_UnitSquarePredicate_domain = (domain expression5 == CartesianProduct [Reals,Reals] )
prop_test_UnitSquarePredicate_valid = (validExpression expression5)

unitSquare = SimpleSubset expression5


-- Validate that lambda's do not need the RHS to contain the bound variables
prop_testValidate_lambdaRhs_noCommonVars = (validExpression (Lambda (GeneralVariable "x" Reals) (RealConstant 1)) )
    
-- Disjoint union
labels1to10 = IntegerRange 1 10
labels1to5 = IntegerRange 1 5
d1 = DisjointUnion labels1to10 (DomainMapConstant unitSquare)

d2 = 
  DisjointUnion 
    (IntegerRange 1 10) 
    (DomainMapIf 
      (IntegerRange 1 5) 
      (DomainMapConstant unitSquare) 
      (DomainMapConstant simplex2d)
    )

d3 = 
  DisjointUnion 
    (IntegerRange 1 10) 
    (DomainMapIf 
      (IntegerRange 1 5) 
      (DomainMapConstant unitSquare) 
      (DomainMapConstant d2)
    )

-- Partial application
polarToCartesian = 
  Lambda (Tuple [GeneralVariable "radius" Reals, GeneralVariable "θ" Reals]) (
    Tuple
      [
        (Cos (GeneralVariable "θ" Reals))
        `Times`
        (GeneralVariable "radius" Reals)
        ,
        (Sin (GeneralVariable "θ" Reals))
        `Times`        
        (GeneralVariable "radius" Reals)
      ]
    )

prop_test_DomainPolarToCartesian = (domain polarToCartesian == CartesianProduct [Reals, Reals])

polarToCartesianFixedRadius = 
  PartialApplication 2 (polarToCartesian) (RealConstant 1)

prop_test_Domain_PartialApplication = ((domain polarToCartesianFixedRadius) == Reals )
  
-- Circle from unit line    

circleConnectionMap =
  Restriction
  unitLineSegment
-- Todo: get CD, and add to known lists.
  (Modulus (GeneralVariable "theta" Reals) Pi )

circle = Quotient circleConnectionMap

-- Some simplification
prop_testResult8 = ( simplifyFSet (Factor 3 (CartesianProduct  [Reals, Booleans, Reals] )) == Reals  )

-- Parameter map test
-- 4 5 6
-- 1 2 3

elementIdFSet = Labels (IntegerRange 1 2)
localNodeFSet = Labels (IntegerRange 1 4)
elementId = GeneralVariable "elementId" elementIdFSet
localNode = GeneralVariable "localNode" localNodeFSet

localToGlobalNodes = FromIntegerParameterSource
  [ 1, 2, 4, 5, 
    2, 3, 5, 6 ]
  (CartesianProduct [ elementIdFSet, localNodeFSet ])

prop_test_IntParam_01a = (domain localToGlobalNodes == CartesianProduct [ Labels (IntegerRange 1 2), Labels (IntegerRange 1 4) ] )

prop_test_IntParam_01b = (validExpression localToGlobalNodes)

brokenParamTest = 
  FromIntegerParameterSource
    [ 1, 2, 3, 4, 5 ]
  (CartesianProduct [ elementIdFSet, localNodeFSet ])

prop_test_IntParam_01c = ( not (validExpression brokenParamTest))

-- Todo: perhaps we want the parameters to the IntegerRange constructor to be variables that can be e.g. Map types.
globalNodeFSet = Labels (IntegerRange 1 6)
globalNode = GeneralVariable "globalNode" globalNodeFSet

pressureAtNodes = 
  FromRealParameterSource 
    [  0.1,      0.5,  55.9, 
      -0.4,   -100.9,  19.0 ] 
    globalNodeFSet

prop_test_IntParam_01d = ( validExpression pressureAtNodes )

-- Demonstrating equations.  For now, this is just a Map to Boolean, but an extra construct could be added that means that this is asserted to be true.
xy1 = GeneralVariable "xy" (CartesianProduct [Reals, Reals])
g1 = Tuple [ GeneralVariable "x" Reals, RealConstant 1.93 ]

-- - This one has free variables xy and x, and whether it is true depends on values for xy and x. If there were a way of asserting that it must be true, then that constrains what valid values of xy and x are.
equation1Style1 = xy1 `Equal` g1

-- Demonstrating function space
-- Todo: commented out example of use of Signature space, while trying to decide what to do regarding relationship to topological space.
-- f1 = GeneralVariable "f" (SignatureSpace unitSquare Reals)

-- Inverse of non-invertible function produces a set.
y = GeneralVariable "y" Reals

f2 = x `Times` x

predicate2a = ( y `Equal` f2 )

predicate2b = PartialApplication 1 predicate2a (RealConstant 1.0)

levelSet1 = SimpleSubset predicate2b

-- Tensor like product (i.e. Kronecker product to get what is commonly misleadingly called "Tensor product basis functions")

basis1dLinearLagrange_xi1 = Apply expression3c (GeneralVariable "ξ1" unitLineSegment)
basis1dLinearLagrange_xi2 = Apply expression3c (GeneralVariable "ξ2" unitLineSegment)
basis1dLinearLagrange_xi3 = Apply expression3c (GeneralVariable "ξ3" unitLineSegment)

prop_test_PartialApplication = (validExpression basis1dLinearLagrange_xi1)

basis2dLinearLagrange_a = Lambda 
  (Tuple [(GeneralVariable "ξ1" unitLineSegment), (GeneralVariable "ξ2" unitLineSegment)]) 
  (KroneckerProduct [basis1dLinearLagrange_xi1, basis1dLinearLagrange_xi2])

prop_test_KroneckerProduct = (validExpression basis2dLinearLagrange_a)

basis3dLinearLagrange_a = Lambda
  (Tuple [(GeneralVariable "ξ1" unitLineSegment), (GeneralVariable "ξ2" unitLineSegment), (GeneralVariable "ξ3" unitLineSegment)])   
  (KroneckerProduct [basis1dLinearLagrange_xi1, basis1dLinearLagrange_xi2, basis1dLinearLagrange_xi3 ])

prop_test_KroneckerProduct3 = (validExpression basis3dLinearLagrange_a)

-- Interior. Todo: Use FEM to describe boundary mesh.

-- Todo: it would be better to do this as a 1d mesh representing a polygonal boundary embedded in 2d, the key difference being the use of parameter stores.

l1Map = Lambda xi1 (
  Tuple [
    xi1, 
    RealConstant 0 ]
  )
l2Map = Lambda xi1 (
  Tuple [ 
    RealConstant 1, 
    xi1 ]
  )
l3Map = Lambda xi1 (
  Tuple [ 
    (RealConstant 1 ) `Minus` xi1, 
    RealConstant 1 ]
  )
l4Map = Lambda xi1 (
  Tuple [ 
    RealConstant 0, 
    (RealConstant 1 ) `Minus` xi1 ]
  )

l1SpaceXY = Image l1Map
l2SpaceXY = Image l2Map
l3SpaceXY = Image l3Map
l4SpaceXY = Image l4Map

unionPredicate = Lambda xy (
  (xy `ElementOf` l1SpaceXY) `Or`
  (xy `ElementOf` l2SpaceXY) `Or`
  (xy `ElementOf` l3SpaceXY) `Or`
  (xy `ElementOf` l4SpaceXY)
  )

squareBoundary = SimpleSubset unionPredicate

squareFromBoundary = Interior squareBoundary

-- Equivalent of Image, using Exists
l1SpaceXY' = SimpleSubset ( Lambda xy1
    (Exists xi1 (xy1 `Equal` (Apply l1Map xi1)))
  )

SimpleSubset p1a = l1SpaceXY'

prop_test_Exists1a = (validExpression p1a)

prop_test_Exists1b = (freeVariables p1a == [] )

prop_test_Exists1c = (domain p1a == CartesianProduct[Reals, Reals])

Lambda _ p1b = p1a

prop_test_Exists1d = (freeVariables p1b == [GeneralVariable "xy" (CartesianProduct [Reals,Reals])] )

prop_test_Exists1e = (canonicalSuperset (domain p1b) == UnitSpace )

-- Todo: validation is too strict, and not correct. Currently validation of Equal requires that both operands have the same codomain, whereas what should be checked is that there is a conversion that allows values from one to be compared with the other, even if the codomains are not identical.
prop_test_Exists1f = (validExpression p1b)

Exists p1c1 p1c2 = p1b

prop_test_Exists1g1 = (validExpression p1c1)
prop_test_Exists1g2 = (validExpression p1c2)

-- Uncertainty 
normalDistribution = 
  Lambda 
  (GeneralVariable "x" Reals)
  (
    (
      (RealConstant 1.0) 
      `Divide` 
      ( (GeneralVariable "variance" Reals) 
        `Times` 
        ( ((RealConstant 2.0) `Times` Pi) `Power` ((RealConstant 1.0) `Divide` (RealConstant 2.0)) )
      )
    )
    `Times`
    ( Exp  
      ( (Negate ((RealConstant 1) `Divide` (RealConstant 2)) )
        `Times`
        (Power 
          (
            ((GeneralVariable "x" Reals) `Minus` (GeneralVariable "mean" Reals))
            `Divide`
            (GeneralVariable "variance" Reals)
          )
          (RealConstant 2)
        ) 
      )
    )
  )
  
statement1 = (GeneralVariable "xr" Reals) `DistributedAccordingTo` normalDistribution 
