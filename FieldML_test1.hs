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

x = RealVariable "x"

expression1 :: Map
expression1 =  (x `LessThan` (Lambda [RealVariable "x"] (RealConstant 1) ))  `And` ( (Lambda [RealVariable "x"] (RealConstant 0) ) `LessThan` x)

-- Todo: get a chart for a topological space, and name the coordinates in the chart so that they can be mapped to the free variables of a real expression.

unitLineSegment = SimpleSubset expression1

-- As above, but all inline:
unitLineSegment' = 
  SimpleSubset (
    (x `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` x)
  )

xy = Lambda [RealVariable "xx", RealVariable "yy"] (Tuple [RealVariable "xx", RealVariable "yy"])
  
expression2 :: Map
expression2 =
    ((Project 1 xy) `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` (Project 1 xy)) 
    `And`
    ((Project 2 xy) `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` (Project 2 xy))

prop_testResult1 = (domain expression2 == CartesianProduct [Reals,Reals] )
  
expression3a :: Map
expression3a = Lambda [RealVariable "x"] ((RealConstant 1) `Minus` (RealVariable "x"))

expression3b :: Map
expression3b = RealVariable "x"
  
-- By the way, this is a 1D linear lagrange interpolation basis.
expression3c =
  Restriction
    unitLineSegment -- A validator would have to check that uniLineSegment is a sensible restriction of the original domain of the map.
    (Tuple [expression3a, expression3b])

prop_testResult3a = ( domain expression3c == unitLineSegment )
prop_testResult3b = ( codomain expression3c == CartesianProduct [Reals,Reals] )


expression4 :: Map
expression4 =
  Lambda [RealVariable "x", RealVariable "y"] $
    ( (RealConstant 0) `LessThan` RealVariable "x" )
    `And`
    ( (RealConstant 0) `LessThan` RealVariable "y" )
    `And`
    ( ( RealVariable "x" `Plus` RealVariable "y" ) `LessThan` (RealConstant 1)  )

simplex2d = SimpleSubset expression4
  
expression5 :: Map
expression5 =
  Lambda [RealVariable "x", RealVariable "y"] $
    (RealVariable "x" `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` RealVariable "x") 
    `And`
    (RealVariable "y" `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` RealVariable "y")

prop_testResult5 = (domain expression5 == CartesianProduct [Reals,Reals] )

unitSquare = SimpleSubset expression5
    

-- Validate
prop_testValidate1 = (validateMap (Lambda [RealVariable "x"] (RealConstant 1)) )
    
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
        (CSymbol "openmath cd transc1 cos" (RealVariable "theta"))
        (RealVariable "radius")
      ,
      Times
        (CSymbol "openmath cd transc1 sin" (RealVariable "theta"))
        (RealVariable "radius")
    ]

prop_testResult6 = (domain polarToCartesian == CartesianPower 2 Reals)

polarToCartesianFixedRadius = 
  PartialApplication 1 (polarToCartesian) (RealConstant 1)

prop_testResult7 = ((listOfFreeRealVariables polarToCartesianFixedRadius) == (Set.fromList ["theta"]))
  
-- Circle from unit line    
-- Todo: get CD, and add to known lists.
fieldml_pi = CSymbol "openmath cd ? PI" UnitElement

circleConnectionMap =
-- Todo: get CD, and add to known lists.
  CSymbol "openmath cd ? modulus" (Tuple [RealVariable "theta", fieldml_pi] )

circle = Quotient unitLineSegment circleConnectionMap

-- Some simplification
prop_testResult8 =
  ( CartesianProduct [Reals, Reals] ==
    simplifyTopologicalSpace  (Factors  ([0,2]) (CartesianProduct  [Reals, Booleans, Reals]))
  )