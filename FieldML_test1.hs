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

expression1 :: Map
expression1 =  
  (x `LessThan` (RealConstant 1) )
  `And` 
  ( (RealConstant 0) `LessThan` x)

-- Todo: get a chart for a topological space, and name the coordinates in the chart so that they can be mapped to the free variables of a real expression.
-- But perhaps the chart is just the tuple that represents a value in the topological space?  Tuples can consist of named variables.

unitLineSegment = SimpleSubset expression1

-- As above, but more inline:
unitLineSegment' = 
  SimpleSubset (
    (x `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` x)
  )

xy = Tuple [GeneralVariable "xx" Reals, GeneralVariable "yy" Reals]
  
expression2 :: Map
expression2 =
    ((Project 1 xy) `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` (Project 1 xy)) 
    `And`
    ((Project 2 xy) `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` (Project 2 xy))

prop_test_2dTupleMapDomain1a = (domain expression2 == CartesianProduct [Reals,Reals] )

prop_test_2dTupleMapDomain1b = (listOfFreeGeneralVariables expression2 == [GeneralVariable "xx" Reals,GeneralVariable "yy" Reals] )
  
expression3a :: Map
expression3a = Lambda (Tuple [GeneralVariable "x" Reals]) ((RealConstant 1) `Minus` (GeneralVariable "x" Reals))

expression3b :: Map
expression3b = GeneralVariable "x" Reals
  
-- By the way, this is a 1D linear lagrange interpolation basis.
expression3c =
  Restriction
    unitLineSegment -- A validator would have to check that uniLineSegment is a sensible restriction of the original domain of the map.
    (Tuple [expression3a, expression3b])

prop_testResult3a = ( domain expression3c == unitLineSegment )
prop_testResult3b = ( codomain expression3c == CartesianProduct [Reals,Reals] )


expression4 :: Map
expression4 =
  Lambda (Tuple [GeneralVariable "x" Reals, GeneralVariable "y" Reals]) $
    ( (RealConstant 0) `LessThan` GeneralVariable "x" Reals )
    `And`
    ( (RealConstant 0) `LessThan` GeneralVariable "y" Reals )
    `And`
    ( ( GeneralVariable "x" Reals `Plus` GeneralVariable "y" Reals ) `LessThan` (RealConstant 1)  )

simplex2d = SimpleSubset expression4
  
expression5 :: Map
expression5 =
  Lambda (Tuple [GeneralVariable "x" Reals, GeneralVariable "y" Reals]) $
    (GeneralVariable "x" Reals `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` GeneralVariable "x" Reals) 
    `And`
    (GeneralVariable "y" Reals `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` GeneralVariable "y" Reals)

prop_testResult5 = (domain expression5 == CartesianProduct [Reals,Reals] )

unitSquare = SimpleSubset expression5
    

-- Validate
prop_testValidate1 = (validateMap (Lambda (GeneralVariable "x" Reals) (RealConstant 1)) )
    
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
  Tuple
    [
      Times
        (CSymbol "openmath cd transc1 cos" (GeneralVariable "theta" Reals))
        (GeneralVariable "radius" Reals)
      ,
      Times
        (CSymbol "openmath cd transc1 sin" (GeneralVariable "theta" Reals))
        (GeneralVariable "radius" Reals)
    ]

prop_testResult6 = (domain polarToCartesian == CartesianProduct [Reals, Reals])

polarToCartesianFixedRadius = 
  PartialApplication 2 (polarToCartesian) (RealConstant 1)

prop_testResult7 = ((listOfFreeGeneralVariables polarToCartesianFixedRadius) == [ GeneralVariable "theta" Reals ])
  
-- Circle from unit line    
-- Todo: get CD, and add to known lists.
fieldml_pi = CSymbol "openmath cd ? PI" UnitElement

circleConnectionMap =
  Restriction
  unitLineSegment
-- Todo: get CD, and add to known lists.
  (CSymbol "openmath cd ? modulus" (Tuple [GeneralVariable "theta" Reals, fieldml_pi] ) )

circle = Quotient circleConnectionMap

-- Some simplification
prop_testResult8 = ( simplifyTopologicalSpace (Factor 2 (CartesianProduct  [Reals, Booleans, Reals] )) == Reals  )

-- Parameter map test
-- 4 5 6
-- 1 2 3

localNode = GeneralVariable "localNode" (Labels (IntegerRange 1 4))
elementId = GeneralVariable "elementId" (Labels (IntegerRange 1 2))

localToGlobalNodes = 
  FromIntegerParameterSource
    [ 1, 2, 4, 5, 
      2, 3, 5, 6 ]
    (Tuple [ elementId, localNode ])

prop_testResult_IntParam_01a = (domain localToGlobalNodes == CartesianProduct [ Labels (IntegerRange 1 2), Labels (IntegerRange 1 4) ] )

prop_testResult_IntParam_01b = (validateMap localToGlobalNodes)

brokenParamTest = 
  FromIntegerParameterSource
    [ 1, 2, 3, 4, 5 ]
    (Tuple [ elementId, localNode ])

prop_testResult_IntParam_01c = ( not (validateMap brokenParamTest))

-- Todo: don't we want the parameters to the IntegerRange constructor to be variables that can be e.g. Map types?
globalNode = GeneralVariable "globalNode" (Labels (IntegerRange 1 6))

pressureAtNodes = 
  FromRealParameterSource 
    [  0.1,      0.5,  55.9, 
      -0.4,   -100.9,  19.0 ] 
    globalNode

prop_testResult_IntParam_01d = ( validateMap pressureAtNodes )

