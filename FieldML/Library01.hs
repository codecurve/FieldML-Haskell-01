module FieldML.Library01 (
  real2,
  real3,
  
  unitLineSegment,
  unitSquare,
  simplex2d,  
  
  basis1dLinearLagrange,
  basis2dLinearLagrange,
  basis3dLinearLagrange

) where

import FieldML.Core

real2 = CartesianProduct [Reals, Reals]
real3 = CartesianProduct [Reals, Reals, Reals]

x = GeneralVariable "x" Reals

unitLinePredicate =  Lambda x
  (  (x `LessThan` (RealConstant 1) )
     `And` 
     ( (RealConstant 0) `LessThan` x)
  )   

unitLineSegment = SimpleSubset unitLinePredicate

xi1 = GeneralVariable "ξ1" unitLineSegment

phi1 = (RealConstant 1) `Minus` xi1

phi2 =  xi1

basis1dLinearLagrange = Lambda xi1 (Tuple [phi1, phi2])


--Todo: Aother way of looking at this is that this assumes that 'equations' for Lambda's may use pattern matching.
xy = Tuple [GeneralVariable "x" Reals, GeneralVariable "y" Reals]

simplex2dPredicate = 
  Lambda xy (
    ( (RealConstant 0) `LessThan` GeneralVariable "x" Reals )
    `And`
    ( (RealConstant 0) `LessThan` GeneralVariable "y" Reals )
    `And`
    ( ( GeneralVariable "x" Reals `Plus` GeneralVariable "y" Reals ) `LessThan` (RealConstant 1) )
  )

simplex2d = SimpleSubset simplex2dPredicate

unitSquarePredicate = 
  Lambda xy (
    (GeneralVariable "x" Reals `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` GeneralVariable "x" Reals) 
    `And`
    (GeneralVariable "y" Reals `LessThan` (RealConstant 1))  `And` ( (RealConstant 0) `LessThan` GeneralVariable "y" Reals)
  )

unitSquare = SimpleSubset unitSquarePredicate

basis1dLinearLagrange_xi1 = Apply basis1dLinearLagrange (GeneralVariable "ξ1" unitLineSegment)
basis1dLinearLagrange_xi2 = Apply basis1dLinearLagrange (GeneralVariable "ξ2" unitLineSegment)
basis1dLinearLagrange_xi3 = Apply basis1dLinearLagrange (GeneralVariable "ξ3" unitLineSegment)

basis2dLinearLagrange = Lambda 
  (Tuple [
    (GeneralVariable "ξ1" unitLineSegment), 
    (GeneralVariable "ξ2" unitLineSegment)
  ]) 
  (KroneckerProduct [
    basis1dLinearLagrange_xi1, 
    basis1dLinearLagrange_xi2
  ])

basis3dLinearLagrange = Lambda
  (Tuple [
    (GeneralVariable "ξ1" unitLineSegment), 
    (GeneralVariable "ξ2" unitLineSegment), 
    (GeneralVariable "ξ3" unitLineSegment)
  ])   
  (KroneckerProduct [
    basis1dLinearLagrange_xi1, 
    basis1dLinearLagrange_xi2, 
    basis1dLinearLagrange_xi3 
  ])
