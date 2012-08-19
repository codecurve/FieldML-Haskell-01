{-# LANGUAGE CPP, TemplateHaskell #-}

module FieldML_test1
where

import FieldML.Core
import FieldML.Utility.ExpressionTree
import qualified FieldML.Library01
import qualified FieldML.Library02
import FieldML.Utility01

import qualified FieldML_test_mesh01


import Control.Monad (unless)
import Data.List (stripPrefix)
import qualified Data.Set as Set
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)

main = testMain

testMain = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure


-- Tests
x = GeneralVariable "x" Reals
  
SimpleSubset expression1 = FieldML.Library01.unitLineSegment

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

prop_test_Subset1a = (canonicalSuperset FieldML.Library01.unitLineSegment == Reals)

-- As above, but more inline:
unitLineSegment' = 
  SimpleSubset (
    Lambda x ((x `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` x))
  )

prop_test_match1a = (FieldML.Library01.unitLineSegment == unitLineSegment')

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

xi1 = GeneralVariable "ξ1" FieldML.Library01.unitLineSegment  

prop_test_LambdaTuple_domain = ( domain FieldML.Library01.basis1dLinearLagrange == FieldML.Library01.unitLineSegment )
prop_test_LambdaTuple_codomain = ( canonicalSuperset (codomain FieldML.Library01.basis1dLinearLagrange) == FieldML.Library01.real2 )
prop_test_LambdaTuple_freeVariables = ( freeVariables FieldML.Library01.basis1dLinearLagrange == [] )
prop_test_LambdaTuple_valid = ( validExpression FieldML.Library01.basis1dLinearLagrange )

expression3c = FieldML.Library01.basis1dLinearLagrange

Lambda _ expression3c_lambdaRhs = expression3c
prop_test_Tuple_freeVariables = ( freeVariables expression3c_lambdaRhs == [ xi1 ] )

SimpleSubset expression4 = FieldML.Library01.simplex2d

prop_test_Simplex2dPredicate = (domain expression4 == CartesianProduct[Reals, Reals])
  
-- Validate that lambda's do not need the RHS to contain the bound variables
prop_testValidate_lambdaRhs_noCommonVars = (validExpression (Lambda (GeneralVariable "x" Reals) (RealConstant 1)) )
    

-- Disjoint union
labels1to10 = IntegerRange 1 10
labels1to5 = IntegerRange 1 5

d1 = DisjointUnion labels1to10 (DomainMapConstant Reals)

prop_test_Cast_TupleToDisjointUnion1 = (validExpression (Cast (Tuple [LabelValue (IntegerLabel 1 labels1to10), RealConstant 1]) d1))

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

prop_test_Cast_TupleToDisjointUnion2 = 
  validExpression 
    (Cast 
      (Tuple [
        LabelValue (IntegerLabel 6 labels1to10), 
        Cast 
          (Tuple [RealConstant 1, RealConstant 0]) 
          FieldML.Library01.simplex2d
      ])
      d3
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
  PartialApplication (polarToCartesian) 2 (RealConstant 1)

prop_test_Domain_PartialApplication = ((domain polarToCartesianFixedRadius) == Reals )
  

-- Topology, connectivity using Quotient: Circle topology from unit line    

circleConnectionMap =
  Restriction
  FieldML.Library01.unitLineSegment
  (Lambdify (Modulus (GeneralVariable "theta" Reals) (RealConstant 1.0) ))

prop_test_RestrictionForCircle = (validExpression circleConnectionMap)

circle = Quotient circleConnectionMap


-- Some simplification
prop_testResult8 = ( simplifyFSet (Factor 3 (CartesianProduct  [Reals, Booleans, Reals] )) == Reals  )

prop_test_IntParam_01a = (domain FieldML_test_mesh01.localToGlobalNodes == CartesianProduct [ Labels (IntegerRange 1 2), Labels (IntegerRange 1 4) ] )

prop_test_IntParam_01b = (validExpression FieldML_test_mesh01.localToGlobalNodes)

brokenParamTest = MultiDimArray
  (IntegerParameterVector  
    [ 1, 2, 3, 4, 5 ]
    FieldML_test_mesh01.globalNodesFSet
  )  
  (CartesianProduct [ FieldML_test_mesh01.elementIdFSet, FieldML_test_mesh01.localNodeFSet ])

prop_test_IntParam_01c = ( not (validExpression brokenParamTest))

prop_test_IntParam_01d = ( validExpression FieldML_test_mesh01.pressureAtGlobalNodes )

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

basis1dLinearLagrange_xi1 = Apply FieldML.Library01.basis1dLinearLagrange (GeneralVariable "ξ1" FieldML.Library01.unitLineSegment)

prop_test_PartialApplication = (validExpression basis1dLinearLagrange_xi1)

prop_test_KroneckerProduct2d = (validExpression FieldML.Library01.basis2dLinearLagrange)

prop_test_KroneckerProduct3d = (validExpression FieldML.Library01.basis3dLinearLagrange)


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
  (Apply FieldML.Library02.normalDistribution (Tuple [mean1, variance1]))

prop_test_normallyDistributedVariable1 = (validExpression statement1)




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

xi = (GeneralVariable "ξ" FieldML.Library01.unitSquare) 
   
edge2Predicate = Lambda
  xi
  ((Project 1 (GeneralVariable "ξ" FieldML.Library01.unitSquare)) `Equal` (RealConstant 0.0))

edge3Predicate = Lambda
  xi
  ((Project 1 (GeneralVariable "ξ" FieldML.Library01.unitSquare)) `Equal` (RealConstant 1.0))

edge1Predicate = Lambda
  xi
  ((Project 2 (GeneralVariable "ξ" FieldML.Library01.unitSquare)) `Equal` (RealConstant 0.0))

edge4Predicate = Lambda
  xi
  ((Project 2 (GeneralVariable "ξ" FieldML.Library01.unitSquare)) `Equal` (RealConstant 1.0))

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
  
prop_test_Where = ((freeVariables equivalenceInducer1) == [])

mesh_WithConnectivity = Quotient equivalenceInducer1

Where a xs = equivalenceInducer1
x1a:x1b:x1s = xs
Equal x2a x2b = x1a
Equal x3a x3b = x1b
Project n x4a = x3b

prop_test_Tuples_And_DisjointUnionValue = (validExpression x3b)

prop_test_Project_NonTuple = (validExpression equivalenceInducer1)


-- Simple contraction

v1 = GeneralVariable "v1" FieldML.Library01.real2'
v2 = GeneralVariable "v2" FieldML.Library01.real2'
simpleContraction = Contraction v1 1 v2 1

prop_test_SimpleContraction = (validExpression simpleContraction)
    

-- Template for scalar field

prop_test_pressureFieldViaTemplate2 = (validExpression FieldML_test_mesh01.pressureViaTemplate)

-- Simple-ish geometric field.
prop_test_geometricFieldViaTemplate = (validExpression FieldML_test_mesh01.geometricFieldExpression)

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

prop_test_Subset_CommutesWith_CartesianProduct_Case = (unitSquare' == FieldML.Library01.unitSquare)

prop_test_Library_basis2dLinearLagrange_Apply_In_unitSquare1 = (validExpression (Apply FieldML.Library01.basis2dLinearLagrange (Tuple [xi,xi])))

prop_test_Library_basis2dLinearLagrange_Apply_In_unitSquare2 = 
  (validExpression 
    (Apply 
      FieldML.Library01.basis2dLinearLagrange 
      (GeneralVariable "ξ" FieldML.Library01.unitSquare)
    )
  )


