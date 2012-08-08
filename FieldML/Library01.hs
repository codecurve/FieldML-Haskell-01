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

import qualified FieldML.Core as C
import qualified FieldML.Utility.CoreInfixExpressions as I

real2 = C.CartesianProduct [C.Reals, C.Reals]
real3 = C.CartesianProduct [C.Reals, C.Reals, C.Reals]

x = C.GeneralVariable () "x" C.Reals


unitLinePredicate =  C.Lambda () x
  (  (x `I.lessThan` (C.RealConstant () 1) )
     `I.and'` 
     ( (C.RealConstant () 0) `I.lessThan` x)
  )   

unitLineSegment = C.SimpleSubset unitLinePredicate

xi1 = C.GeneralVariable () "ξ1" unitLineSegment

phi1 = (C.RealConstant () 1) `I.minus` xi1

phi2 =  xi1

basis1dLinearLagrange = C.Lambda () xi1 (C.Tuple () [phi1, phi2])


--Todo: Aother way of looking at this is that this assumes that 'equations' for C.Lambda's may use pattern matching.
xy = C.Tuple () [C.GeneralVariable () "x" C.Reals, C.GeneralVariable () "y" C.Reals]

simplex2dPredicate = 
  C.Lambda () xy (
    ( (C.RealConstant () 0) `I.lessThan` C.GeneralVariable () "x" C.Reals )
    `I.and'`
    ( (C.RealConstant () 0) `I.lessThan` C.GeneralVariable () "y" C.Reals )
    `I.and'`
    ( ( C.GeneralVariable () "x" C.Reals `I.plus` C.GeneralVariable () "y" C.Reals ) `I.lessThan` (C.RealConstant () 1) )
  )

simplex2d = C.SimpleSubset simplex2dPredicate

unitSquarePredicate = 
  C.Lambda () xy (
    (C.GeneralVariable () "x" C.Reals `I.lessThan` (C.RealConstant () 1))  `I.and'` ( (C.RealConstant () 0) `I.lessThan` C.GeneralVariable () "x" C.Reals) 
    `I.and'`
    (C.GeneralVariable () "y" C.Reals `I.lessThan` (C.RealConstant () 1))  `I.and'` ( (C.RealConstant () 0) `I.lessThan` C.GeneralVariable () "y" C.Reals)
  )

unitSquare = C.SimpleSubset unitSquarePredicate

basis1dLinearLagrange_xi1 = C.Apply () basis1dLinearLagrange (C.GeneralVariable () "ξ1" unitLineSegment)
basis1dLinearLagrange_xi2 = C.Apply () basis1dLinearLagrange (C.GeneralVariable () "ξ2" unitLineSegment)
basis1dLinearLagrange_xi3 = C.Apply () basis1dLinearLagrange (C.GeneralVariable () "ξ3" unitLineSegment)

basis2dLinearLagrange = C.Lambda ()
  (C.Tuple () [
    (C.GeneralVariable () "ξ1" unitLineSegment), 
    (C.GeneralVariable () "ξ2" unitLineSegment)
  ]) 
  (C.KroneckerProduct () [
    basis1dLinearLagrange_xi1, 
    basis1dLinearLagrange_xi2
  ])

basis3dLinearLagrange = C.Lambda ()
  (C.Tuple () [
    (C.GeneralVariable () "ξ1" unitLineSegment), 
    (C.GeneralVariable () "ξ2" unitLineSegment), 
    (C.GeneralVariable () "ξ3" unitLineSegment)
  ])   
  (C.KroneckerProduct () [
    basis1dLinearLagrange_xi1, 
    basis1dLinearLagrange_xi2, 
    basis1dLinearLagrange_xi3 
  ])
