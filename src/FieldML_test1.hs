module Main where

import FieldML.Core
import FieldML.Utility01
import qualified FieldML.Library01
import qualified FieldML.Library02

import qualified FieldML_test_mesh01

import Test.HUnit

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

main = defaultMain tests
--main :: IO Counts
--main = {-forever $ -} runTestTT tests
-- main = testMain

tests = [ testGroup "Sorting Group 1" [
  testCase "testBooleanExpression1a" testBooleanExpression1a,
  testCase "testBooleanExpression1b" testBooleanExpression1b,
  testCase "testBooleanExpression1c" testBooleanExpression1c,
  testCase "testBooleanExpression1d" testBooleanExpression1d,
  testCase "testBooleanExpression1e" testBooleanExpression1e,
  testCase "testSubset1a" testSubset1a,
  testCase "testmatch1a" testmatch1a,
  testCase "test2dTupleMapDomain1a" test2dTupleMapDomain1a,
  testCase "test2dTupleMapDomain1b" test2dTupleMapDomain1b,
  testCase "test2dTupleMapDomain1c" test2dTupleMapDomain1c,
  testCase "test2dTupleMapDomain1d" test2dTupleMapDomain1d,
  testCase "testLambdaTuple_domain" testLambdaTuple_domain,
  testCase "testLambdaTuple_codomain" testLambdaTuple_codomain,
  testCase "testLambdaTuple_freeVariables" testLambdaTuple_freeVariables,
  testCase "testLambdaTuple_valid" testLambdaTuple_valid,
--  testCase "testTuple_freeVariables" testTuple_freeVariables,
  testCase "testSimplex2dPredicate" testSimplex2dPredicate,
  testCase "testValidate_lambdaRhs_noCommonVars" testValidate_lambdaRhs_noCommonVars,
  testCase "testCast_TupleToDisjointUnion1" testCast_TupleToDisjointUnion1,
  testCase "testCast_TupleToDisjointUnion2" testCast_TupleToDisjointUnion2,
  testCase "testDomainPolarToCartesian" testDomainPolarToCartesian,
  testCase "testDomain_PartialApplication" testDomain_PartialApplication,
  testCase "testRestrictionForCircle" testRestrictionForCircle,
  testCase "testResult8" testResult8,
  testCase "testIntParam_01a" testIntParam_01a,
  testCase "testIntParam_01b" testIntParam_01b,
  testCase "testIntParam_01c" testIntParam_01c,
  testCase "testIntParam_01d" testIntParam_01d,
  testCase "testPredicate2b" testPredicate2b,
  testCase "testPartialApplication" testPartialApplication,
  testCase "testKroneckerProduct2d" testKroneckerProduct2d,
  testCase "testKroneckerProduct3d" testKroneckerProduct3d,
  testCase "testExists1a" testExists1a,
  testCase "testExists1b" testExists1b,
  testCase "testExists1c" testExists1c,
  testCase "testExists1d" testExists1d,
  testCase "testExists1e" testExists1e,
  testCase "testExists1f" testExists1f,
  testCase "testExists1g1" testExists1g1,
  testCase "testExists1g2" testExists1g2,
  testCase "testnormallyDistributedVariable1" testnormallyDistributedVariable1,
  testCase "testWhere" testWhere,
  testCase "testTuples_And_DisjointUnionValue" testTuples_And_DisjointUnionValue,
  testCase "testProject_NonTuple" testProject_NonTuple,
  testCase "testSimpleContraction" testSimpleContraction,
  testCase "testpressureFieldViaTemplate2" testpressureFieldViaTemplate2,
  testCase "testgeometricFieldViaTemplate" testgeometricFieldViaTemplate,
--  testCase "testSubset_CommutesWith_CartesianProduct_Case" testSubset_CommutesWith_CartesianProduct_Case,
--  testCase "testLibrary_basis2dLinearLagrange_Apply_In_unitSquare1" testLibrary_basis2dLinearLagrange_Apply_In_unitSquare1,
  testCase "testLibrary_basis2dLinearLagrange_Apply_In_unitSquare2" testLibrary_basis2dLinearLagrange_Apply_In_unitSquare2,
  testCase "testLinearLagrange1_valid" testLinearLagrange1_valid,
  testCase "testLinearLagrange2_valid" testLinearLagrange2_valid,
  testCase "testLinearLagrange3_valid" testLinearLagrange3_valid,
  testCase "testCubicHermite1_valid" testCubicHermite1_valid,
  testCase "testCubicHermite2_valid" testCubicHermite2_valid,
  testCase "testCubicHermite3_valid" testCubicHermite3_valid,
  testCase "testCustomMixedBasis_valid" testCustomMixedBasis_valid
  ]]



-- Tests

--TODO: get a chart for a topological space, and name the coordinates in the chart so that they can be mapped to the free variables of a real expression.
-- But perhaps the chart is just the tuple that represents a value in the topological space (Tuples can consist of named variables).

--TODO: separate validation of library items from general validation.

x :: Expression
x = GeneralVariable "x" Reals

expression1 :: Expression
SimpleSubset expression1 = FieldML.Library01.unitLineSegment

testBooleanExpression1a =  assertBool "valid expression 1a" (validExpression expression1)

testBooleanExpression1b =  assertEqual "valid expression 1b" (freeVariables expression1) []


expression1_lambdaRhs :: Expression
Lambda _ expression1_lambdaRhs = expression1

testBooleanExpression1c =  assertEqual "valid expression 1c" (freeVariables expression1_lambdaRhs) [GeneralVariable "x" Reals]

testBooleanExpression1d =  assertEqual "unit line predicate's domain is Reals" (domain expression1) Reals

testBooleanExpression1e =  assertEqual "unit predicate's codomain is Booleans" (codomain expression1) Booleans

testSubset1a :: Assertion
testSubset1a =  assertEqual "message" (canonicalSuperset FieldML.Library01.unitLineSegment) Reals

-- As above, but more inline:
unitLineSegment' :: FSet
unitLineSegment' = 
  SimpleSubset (
    Lambda x ((x `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` x))
  )

testmatch1a =  assertEqual "message" (FieldML.Library01.unitLineSegment) (unitLineSegment')

xy :: Expression
xy = Tuple [GeneralVariable "x" Reals, GeneralVariable "y" Reals]
  
expression2 :: Expression
expression2 = Lambda xy
  (  
    ((Project 1 xy) `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` (Project 1 xy)) 
    `And`
    ((Project 2 xy) `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` (Project 2 xy))
  )  

test2dTupleMapDomain1a =  assertEqual "message" (domain expression2) (CartesianProduct [Reals,Reals] )

test2dTupleMapDomain1b =  assertEqual "message" (freeVariables expression2) ([] )

Lambda _ expression2_lambdaRhs = expression2
test2dTupleMapDomain1c =  assertEqual "message" (freeVariables expression2_lambdaRhs) ([GeneralVariable "x" Reals,GeneralVariable "y" Reals] )
  
test2dTupleMapDomain1d =  assertBool "message" (validExpression expression2)

xi1 = GeneralVariable "��1" FieldML.Library01.unitLineSegment  

testLambdaTuple_domain =  assertEqual "message" ( domain FieldML.Library01.basis1dLinearLagrange) (FieldML.Library01.unitLineSegment )
testLambdaTuple_codomain =  assertEqual "message" ( canonicalSuperset (codomain FieldML.Library01.basis1dLinearLagrange)) (FieldML.Library01.real2 )
testLambdaTuple_freeVariables =  assertEqual "message" ( freeVariables FieldML.Library01.basis1dLinearLagrange) ([] )
testLambdaTuple_valid =  assertBool "message" ( validExpression FieldML.Library01.basis1dLinearLagrange )

expression3c = FieldML.Library01.basis1dLinearLagrange

Lambda _ expression3c_lambdaRhs = expression3c
testTuple_freeVariables =  assertEqual "message" ( freeVariables expression3c_lambdaRhs) ([ xi1 ] )

SimpleSubset expression4 = FieldML.Library01.simplex2d

testSimplex2dPredicate =  assertEqual "message" (domain expression4) (CartesianProduct[Reals, Reals])
  
-- Validate that lambda's do not need the RHS to contain the bound variables
testValidate_lambdaRhs_noCommonVars =  assertBool "message" (validExpression (Lambda (GeneralVariable "x" Reals) (RealConstant 1)) )
    

-- Disjoint union
labels1to10 = IntegerRange 1 10
labels1to5 = IntegerRange 1 5

d1 = DisjointUnion labels1to10 (DomainMapConstant Reals)

testCast_TupleToDisjointUnion1 =  assertBool "message" (validExpression (Cast (Tuple [LabelValue (IntegerLabel 1 labels1to10), RealConstant 1]) d1))

d2 = DisjointUnion
  labels1to10
  (DomainMapConstant FieldML.Library01.unitSquare)

d3 = 
  DisjointUnion 
    (IntegerRange 1 10) 
    (DomainMapIf 
      (IntegerRange 1 5) 
      (DomainMapConstant FieldML.Library01.unitSquare) 
      (DomainMapConstant FieldML.Library01.simplex2d)
    )

testCast_TupleToDisjointUnion2 = 
   assertBool "message" (validExpression 
    (Cast 
      (Tuple [
        LabelValue (IntegerLabel 6 labels1to10), 
        Cast 
          (Tuple [RealConstant 1, RealConstant 0]) 
          FieldML.Library01.simplex2d
      ])
      d3
    )
  )
    

d4 = 
  DisjointUnion 
    (IntegerRange 1 10)
    (DomainMapIf 
      (IntegerRange 1 5) 
      (DomainMapConstant FieldML.Library01.unitSquare) 
      (DomainMapConstant d3)
    )


-- Partial application
-- Todo: place in library
polarToCartesian = 
  Lambda (Tuple [GeneralVariable "radius" Reals, GeneralVariable "��" Reals]) (
    Tuple
      [
        (Cos (GeneralVariable "��" Reals))
        `Times`
        (GeneralVariable "radius" Reals)
        ,
        (Sin (GeneralVariable "��" Reals))
        `Times`        
        (GeneralVariable "radius" Reals)
      ]
    )

testDomainPolarToCartesian =  assertEqual "message" (domain polarToCartesian) (CartesianProduct [Reals, Reals])

polarToCartesianFixedRadius = 
  PartialApplication (polarToCartesian) 2 (RealConstant 1)

testDomain_PartialApplication =  assertEqual "message" ((domain polarToCartesianFixedRadius)) (Reals )
  

-- Topology, connectivity using Quotient: Circle topology from unit line    

circleConnectionMap =
  Restriction
  FieldML.Library01.unitLineSegment
  (Lambdify (Modulus (GeneralVariable "theta" Reals) (RealConstant 1.0) ))

testRestrictionForCircle =  assertBool "message" (validExpression circleConnectionMap)

circle = Quotient circleConnectionMap


-- Some simplification
testResult8 =  assertEqual "message" ( simplifyFSet (Factor 3 (CartesianProduct  [Reals, Booleans, Reals] ))) (Reals  )

testIntParam_01a =  assertEqual "message" (domain FieldML_test_mesh01.localToGlobalNodes) (CartesianProduct [ Labels (IntegerRange 1 2), Labels (IntegerRange 1 4) ] )

testIntParam_01b =  assertBool "message" (validExpression FieldML_test_mesh01.localToGlobalNodes)

brokenParamTest = MultiDimArray
  (IntegerParameterVector  
    [ 1, 2, 3, 4, 5 ]
    FieldML_test_mesh01.globalNodesFSet
  )  
  (CartesianProduct [ FieldML_test_mesh01.elementIdFSet, FieldML_test_mesh01.localNodeFSet ])

testIntParam_01c =  assertBool "message" ( not (validExpression brokenParamTest))

testIntParam_01d =  assertBool "message" ( validExpression FieldML_test_mesh01.pressureAtGlobalNodes )

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

testPredicate2b =  assertBool "message" (validExpression predicate2b)

levelSet1 = SimpleSubset predicate2b


-- Tensor like product (i.e. Kronecker product to get what is commonly misleadingly called "Tensor product basis functions")

basis1dLinearLagrange_xi1 = Apply FieldML.Library01.basis1dLinearLagrange (GeneralVariable "��1" FieldML.Library01.unitLineSegment)

testPartialApplication =  assertBool "message" (validExpression basis1dLinearLagrange_xi1)

testKroneckerProduct2d =  assertBool "message" (validExpression FieldML.Library01.basis2dLinearLagrange)

testKroneckerProduct3d =  assertBool "message" (validExpression FieldML.Library01.basis3dLinearLagrange)


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

testExists1a =  assertBool "message" (validExpression p1a)

testExists1b =  assertEqual "message" (freeVariables p1a) ([] )

testExists1c =  assertEqual "message" (domain p1a) (CartesianProduct[Reals, Reals])

Lambda _ p1b = p1a

testExists1d =  assertEqual "message" (freeVariables p1b) ([GeneralVariable "xy" (CartesianProduct [Reals,Reals])] )

testExists1e =  assertEqual "message" (canonicalSuperset (domain p1b)) (UnitSpace )

-- Todo: validation is too strict, and not correct. Currently validation of Equal requires that both operands have the same codomain, whereas what should be checked is that there is a conversion that allows values from one to be compared with the other, even if the codomains are not identical.
testExists1f =  assertBool "message" (validExpression p1b)

Exists p1c1 p1c2 = p1b

testExists1g1 =  assertBool "message" (validExpression p1c1)
testExists1g2 =  assertBool "message" (validExpression p1c2)

mean1 = RealConstant 1.3
variance1 = RealConstant 0.2

statement1 = 
  (GeneralVariable "xr" Reals) 
  `DistributedAccordingTo` 
  (Apply FieldML.Library02.normalDistribution (Tuple [mean1, variance1]))

testnormallyDistributedVariable1 =  assertBool "message" (validExpression statement1)




-- Mesh connectivity


-- Local element edge numbering
--  +-4-+
--  |   |
--  2   3
--  |   |
--  +-1-+

localEdgeLabels = IntegerRange 1 4

localEdgeFSet = Labels localEdgeLabels

-- Global mesh element edge numbering, but first example below collapses edge number 4 to a single point.
--  +-6-+-7-+
--  |   |   |
--  3   4   5
--  |   |   |
--  +-1-+-2-+
--
-- The mesh with the collapsed edge is shown here:
--
--  |\     /|
--  | 6   7 |
--  |  \ /  |
--  3   4   5
--  |  / \  |
--  | 1   2 |
--  |/     \|


globalEdgeFSet = Labels (IntegerRange 1 7)

localToGlobalEdges = MultiDimArray  
  (IntegerParameterVector
    [ 1, 3, 4, 6, 
      2, 4, 5, 7 ]
    globalEdgeFSet
  )
  (CartesianProduct [ FieldML_test_mesh01.elementIdFSet, localEdgeFSet ])

loc1 = (GeneralVariable "loc1" FieldML_test_mesh01.mesh_SansConnectivity)

xi = (GeneralVariable "��" FieldML.Library01.unitSquare) 
   
edge2Predicate = Lambda
  xi
  ((Project 1 (GeneralVariable "��" FieldML.Library01.unitSquare)) `Equal` (RealConstant 0.0))

edge3Predicate = Lambda
  xi
  ((Project 1 (GeneralVariable "��" FieldML.Library01.unitSquare)) `Equal` (RealConstant 1.0))

edge1Predicate = Lambda
  xi
  ((Project 2 (GeneralVariable "��" FieldML.Library01.unitSquare)) `Equal` (RealConstant 0.0))

edge4Predicate = Lambda
  xi
  ((Project 2 (GeneralVariable "��" FieldML.Library01.unitSquare)) `Equal` (RealConstant 1.0))

-- Todo: This belongs in the library.
-- Todo: This has an problem: each corner is mapped to only one of the two edges.
unitSquareXiToLocalEdgeId = 
  Lambda 
  xi
  (If (xi `ElementOf` (SimpleSubset edge1Predicate)) (LabelValue (IntegerLabel 1 localEdgeLabels))
  (If (xi `ElementOf` (SimpleSubset edge2Predicate)) (LabelValue (IntegerLabel 2 localEdgeLabels))
  (If (xi `ElementOf` (SimpleSubset edge3Predicate)) (LabelValue (IntegerLabel 3 localEdgeLabels))
  (If (xi `ElementOf` (SimpleSubset edge4Predicate)) (LabelValue (IntegerLabel 4 localEdgeLabels))
  (Unspecified localEdgeFSet)
  ))))

equivalenceInducer1 =
  (Lambda
    (GeneralVariable "mesh_SansConnectivity_location" FieldML_test_mesh01.mesh_SansConnectivity)
    (Apply 
      (PartialApplication localToGlobalEdges 1 (GeneralVariable "elementId" FieldML_test_mesh01.elementIdFSet) ) 
      (Apply unitSquareXiToLocalEdgeId xi)
    )
  )
  `Where` [
    ( (GeneralVariable "elementId" FieldML_test_mesh01.elementIdFSet) 
      `Equal` 
      (Project 1
        (GeneralVariable "mesh_SansConnectivity_location" FieldML_test_mesh01.mesh_SansConnectivity)
      )
    ),
    
    ( xi                                          
      `Equal` 
      (Project 2 
        (GeneralVariable "mesh_SansConnectivity_location" FieldML_test_mesh01.mesh_SansConnectivity)
      )
    )
  ]
  
testWhere =  assertEqual "message" ((freeVariables equivalenceInducer1)) ([])

mesh_WithConnectivity = Quotient equivalenceInducer1

Where a xs = equivalenceInducer1
x1a:x1b:x1s = xs
Equal x2a x2b = x1a
Equal x3a x3b = x1b
Project n x4a = x3b

testTuples_And_DisjointUnionValue =  assertBool "message" (validExpression x3b)

testProject_NonTuple =  assertBool "message" (validExpression equivalenceInducer1)


-- Simple contraction

v1 = GeneralVariable "v1" FieldML.Library01.real2'
v2 = GeneralVariable "v2" FieldML.Library01.real2'
simpleContraction = Contraction v1 1 v2 1

testSimpleContraction =  assertBool "message" (validExpression simpleContraction)
    

-- Template for scalar field

testpressureFieldViaTemplate2 =  assertBool "message" (validExpression FieldML_test_mesh01.pressureViaTemplate)

-- Simple-ish geometric field.
testgeometricFieldViaTemplate =  assertBool "message" (validExpression FieldML_test_mesh01.geometricFieldExpression)

-- This might be useful for debugging this complicated expressions.
-- putStrLn (Data.Tree.drawTree (fmap (\x -> show (validExpression x, x)) (expressionTree FieldML_test_mesh01.fieldTemplate)))
-- putStrLn (Data.Tree.drawTree (fmap (\x -> Data.List.intercalate "\n" [show (validExpression x), show x, show (domain x), show (codomain x)]) (expressionTree FieldML_test_mesh01.fieldTemplate)))

unitSquarePredicate' = 
  Lambda xy (
    (GeneralVariable "x" Reals `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` GeneralVariable "x" Reals) 
    `And`
    (GeneralVariable "y" Reals `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` GeneralVariable "y" Reals)
  )

unitSquare' = SimpleSubset unitSquarePredicate' -- Todo: need to somehow have symbolic analysis that can deduce that unitSquare' is equivalent to unitSquare?

testSubset_CommutesWith_CartesianProduct_Case =  assertEqual "message" (unitSquare') (FieldML.Library01.unitSquare)

testLibrary_basis2dLinearLagrange_Apply_In_unitSquare1 =  assertBool "message" (validExpression (Apply FieldML.Library01.basis2dLinearLagrange (Tuple [xi,xi])))

testLibrary_basis2dLinearLagrange_Apply_In_unitSquare2 =  assertBool "message" (validExpression 
    (Apply 
      FieldML.Library01.basis2dLinearLagrange 
      (GeneralVariable "��" FieldML.Library01.unitSquare)
    )
  )


testLinearLagrange1_valid =  assertBool "message" ( validExpression FieldML.Library01.basis1dLinearLagrange )
testLinearLagrange2_valid =  assertBool "message" ( validExpression FieldML.Library01.basis2dLinearLagrange )
testLinearLagrange3_valid =  assertBool "message" ( validExpression FieldML.Library01.basis3dLinearLagrange )

testCubicHermite1_valid =  assertBool "message" ( validExpression FieldML.Library01.basis1dCubicHermite )
testCubicHermite2_valid =  assertBool "message" ( validExpression FieldML.Library01.basis2dCubicHermite )
testCubicHermite3_valid =  assertBool "message" ( validExpression FieldML.Library01.basis3dCubicHermite )

testCustomMixedBasis_valid =  assertBool "message" ( validExpression FieldML_test_mesh01.basis3d_CubicHermite_BiLinearLagrange )
