{-# LANGUAGE CPP, TemplateHaskell #-}

module FieldML_test1
where

import FieldML.Core
import FieldML.Library01
import FieldML.Library02
import Control.Monad (unless)
import Data.List (stripPrefix)
import qualified Data.Set as Set
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)

testMain = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure


-- Tests

elementIds = IntegerRange 1 4
  
m2 = CartesianProduct [real2, Labels elementIds]

x = GeneralVariable "x" Reals
  
SimpleSubset expression1 = unitLineSegment

-- Todo: get a chart for a topological space, and name the coordinates in the chart so that they can be mapped to the free variables of a real expression.
-- But perhaps the chart is just the tuple that represents a value in the topological space (Tuples can consist of named variables).

-- Todo: This is an abuse of QuickCheck, it is being used in the style of JUnit/XUnit testing.  It is also painful to use because each test is simply repeated.
-- Todo: separate validation of library items from general validation.
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

prop_test_LambdaTuple_domain = ( domain basis1dLinearLagrange == unitLineSegment )
prop_test_LambdaTuple_codomain = ( canonicalSuperset (codomain basis1dLinearLagrange) == real2 )
prop_test_LambdaTuple_freeVariables = ( freeVariables basis1dLinearLagrange == [] )
prop_test_LambdaTuple_valid = ( validExpression basis1dLinearLagrange )

expression3c = basis1dLinearLagrange

Lambda _ expression3c_lambdaRhs = expression3c
prop_test_Tuple_freeVariables = ( freeVariables expression3c_lambdaRhs == [ xi1 ] )

SimpleSubset expression4 = simplex2d

prop_test_Simplex2dPredicate = (domain expression4 == CartesianProduct[Reals, Reals])
  
SimpleSubset expression5 = unitSquare

prop_test_UnitSquarePredicate_domain = (domain expression5 == CartesianProduct [Reals,Reals] )
prop_test_UnitSquarePredicate_valid = (validExpression expression5)

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

localToGlobalNodes = MultiDimArray  
  (IntegerParameterVector
    [ 1, 2, 4, 5, 
      2, 3, 5, 6 ]
  )
  (CartesianProduct [ elementIdFSet, localNodeFSet ])

prop_test_IntParam_01a = (domain localToGlobalNodes == CartesianProduct [ Labels (IntegerRange 1 2), Labels (IntegerRange 1 4) ] )

prop_test_IntParam_01b = (validExpression localToGlobalNodes)

brokenParamTest = MultiDimArray
  (IntegerParameterVector  
    [ 1, 2, 3, 4, 5 ]
  )  
  (CartesianProduct [ elementIdFSet, localNodeFSet ])

prop_test_IntParam_01c = ( not (validExpression brokenParamTest))

-- Todo: perhaps we want the parameters to the IntegerRange constructor to be variables that can be e.g. Map types.
globalNodeFSet = Labels (IntegerRange 1 6)
globalNode = GeneralVariable "globalNode" globalNodeFSet

pressureAtNodes = MultiDimArray 
  (RealParameterVector
     [  0.1,      0.5,  55.9, 
        -0.4,   -100.9,  19.0 ] 
  )
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

f2 = (x `Times` x)

predicate2a = Lambda y ( y `Equal` f2 )

predicate2b = Lambda x (Apply predicate2a (RealConstant 1.0))

prop_test_Predicate2b = (validExpression predicate2b)

levelSet1 = SimpleSubset predicate2b

-- Tensor like product (i.e. Kronecker product to get what is commonly misleadingly called "Tensor product basis functions")

basis1dLinearLagrange_xi1 = Apply basis1dLinearLagrange (GeneralVariable "ξ1" unitLineSegment)

prop_test_PartialApplication = (validExpression basis1dLinearLagrange_xi1)

prop_test_KroneckerProduct2d = (validExpression basis2dLinearLagrange)

prop_test_KroneckerProduct3d = (validExpression basis3dLinearLagrange)

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

mean1 = RealConstant 1.3
variance1 = RealConstant 0.2

statement1 = 
  (GeneralVariable "xr" Reals) 
  `DistributedAccordingTo` 
  (Apply normalDistribution (Tuple [mean1, variance1]))


