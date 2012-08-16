module FieldML.Library01 (
  rc2dCoordLabels,
  rc3dCoordLabels,
  real2,
  real2',
  real3,
  real3',
  
  unitLineSegment,
  unitSquare,
  simplex2d,
  unitCube,
  
  basis1dLinearLagrange,
  basis2dLinearLagrange,
  basis3dLinearLagrange

) where

import FieldML.Core
import qualified Data.Set as Set

-- | Rectangular Cartesian 2-dimensional coordinate labels.
rc2dCoordLabels = Labels (StringLabels (Set.fromList ["x", "y"]))

-- | 2 dimensional Euclidean plane
real2 = CartesianProduct [Reals, Reals]

-- | Alternate type for point in 2-dimensional Euclidian plane dimensional 
real2' = SignatureSpace rc2dCoordLabels Reals -- Todo: Perhaps it is necessary to decide between the two alternatives.  Or, there need to be dictionaries somewhere that group the alternatives that are deemed equivalent.

-- | Rectangular Cartesian 3-dimensional coordinate labels.
rc3dCoordLabels = Labels (StringLabels (Set.fromList ["x", "y", "z"]))

-- | 3 dimensional Euclidean space
real3 = CartesianProduct [Reals, Reals, Reals]

-- | Alternate type for point in 2-dimensional Euclidian plane dimensional 
real3' = SignatureSpace rc3dCoordLabels Reals -- Todo: Seen note at real2'

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

unitSquare = CartesianProduct [unitLineSegment, unitLineSegment]

basis1dLinearLagrange_xi1 = Apply basis1dLinearLagrange (GeneralVariable "ξ1" unitLineSegment)
basis1dLinearLagrange_xi2 = Apply basis1dLinearLagrange (GeneralVariable "ξ2" unitLineSegment)
basis1dLinearLagrange_xi3 = Apply basis1dLinearLagrange (GeneralVariable "ξ3" unitLineSegment)

basis2dLinearLagrange = Lambda 
  (Tuple [
    (GeneralVariable "ξ1" unitLineSegment), 
    (GeneralVariable "ξ2" unitLineSegment)
  ]) 
  (KroneckerProduct [
    basis1dLinearLagrange_xi2,
    basis1dLinearLagrange_xi1
  ])

unitCube = CartesianProduct [unitLineSegment, unitLineSegment, unitLineSegment]

basis3dLinearLagrange = Lambda
  (Tuple [
    (GeneralVariable "ξ1" unitLineSegment), 
    (GeneralVariable "ξ2" unitLineSegment), 
    (GeneralVariable "ξ3" unitLineSegment)
  ])   
  (KroneckerProduct [
    basis1dLinearLagrange_xi3,
    basis1dLinearLagrange_xi2,
    basis1dLinearLagrange_xi1
  ])
