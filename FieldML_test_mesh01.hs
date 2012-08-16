module FieldML_test_mesh01
  (
    mesh_SansConnectivity,
    globalNodesFSet,
    elementIdFSet,
    localNodeFSet,
    elementId,
    localNode,
    localToGlobalNodes,

    pressureAtGlobalNodes,  -- Exported for testing

    t1, -- Exported for testing
    t2, -- Exported for testing
    pressureViaTemplate,
    
    coordinatesAtNodes,
    geometricFieldExpression    
  )
where

import FieldML.Core
import qualified FieldML.Library01
import qualified FieldML.Library02


globalNodesFSet = Labels (IntegerRange 1 6)
elementIdLabels = IntegerRange 1 2 
elementIdFSet = Labels elementIdLabels
localNodeFSet = Labels (IntegerRange 1 4)
elementId = GeneralVariable "elementId" elementIdFSet
localNode = GeneralVariable "localNode" localNodeFSet

-- Todo: Should this be stronly typed as the element IDs?  We want to enforce that the range is discrete. Perhaps just use an FSet, and check that it's discrete as part of validation?  Will have to wait until FSet validation is implemented.


mesh_SansConnectivity = 
  DisjointUnion 
    elementIdLabels
    (CartesianProduct [Labels elementIdLabels, FieldML.Library01.unitSquare])    
    (DomainMapConstant FieldML.Library01.unitSquare) 

xi = (GeneralVariable "ξ" FieldML.Library01.unitSquare) 

-- Local nodes
--  3---4
--  |   |
--  |   |
--  |   |
--  1---2

-- Global nodes for mesh
--  ^  4---5---6
--  |  |   |   |
-- ξ2  | 1 | 2 |
--  |  |   |   |
--  |  1---2---3
--
-- ----ξ1--->

-- Todo: include xi directions on above diagram, and check that basis functions are actually being matched up to the correct nodes.

-- Todo: codomain here is Integers, and should be globalNodesFSet (i.e. the IDs of the global nodes).  
-- Could introduce a constructor syntax, i.e. facility to define constructor and facility to use constructor.
localToGlobalNodes = MultiDimArray  
  (IntegerParameterVector
    [ 1, 2, 4, 5, 
      2, 3, 5, 6 ]
    globalNodesFSet
  )
  (CartesianProduct [ elementIdFSet, localNodeFSet ])

-- Todo: perhaps we want the parameters to the IntegerRange constructor to be variables that can be e.g. Map types.
globalNode = GeneralVariable "globalNode" globalNodesFSet

pressureAtGlobalNodes = MultiDimArray 
  (RealParameterVector [ 
    0.1,
    0.5,
    55.9,
    -0.4,
    -100.9,
    19.0
  ])
  globalNodesFSet


-- Field template
dofSourceSignature = SignatureSpace globalNodesFSet Reals
dofSourceVar = (GeneralVariable "dofSource" dofSourceSignature)
                         
-- Currently this is just a convenience so that this snippet can be used within a Lambda later. xi is not intended to be a closure here, but will be bound when this snippet is used in a Lambda later.
basis2dLLEvaluated = Apply FieldML.Library01.basis2dLinearLagrange xi  

-- | Field template construction step 1: t1 represents the mapping from global nodal DOFs to element local nodal DOFs.
t1 =
  Lambda 
  (Tuple [
    elementId,
    localNode
  ]) 
  (Apply dofSourceVar ((Apply localToGlobalNodes (Tuple [elementId, localNode]))))

-- | Field template construction step 2: t2 represents just the DOFs for local nodes of a particular element.
t2 = PartialApplication t1 1 elementId -- Again: elementId is not intended to be a closure, but will be bound when used in a Lambda later.

-- | Field template construction step 4: scalarFieldTemplate puts it all together so that for any scalar DOF source, the scalar field can be evaluated at any (element,xi).  The scalar field is the inner product, i.e. contraction, of element local node DOFs with evaluated basis functions.
scalarFieldTemplate = 
  Lambda 
  (Tuple [
    dofSourceVar,
    (Tuple [elementId, xi])
  ])
  (Contraction t2 1 basis2dLLEvaluated 1)

-- | Demonstration of a scalar FEM field: to get an actual scalar field, simply apply the template to actual DOFs.
pressureViaTemplate = PartialApplication scalarFieldTemplate 1 pressureAtGlobalNodes


-- Geometry field (x, y) coordinates at each node.

coordinatesAtNodes = MultiDimArray 
  (RealParameterVector [ 
    0.0, 0.0, 0.5,
    0.5, 0.0, 0.6,
    1.0, 0.0, 0.7,
    
    0.0, 0.5, 0.5,
    0.5, 0.5, 0.6,
    1.0, 0.5, 0.7
  ])
  (CartesianProduct [FieldML.Library01.rc3dCoordLabels, globalNodesFSet])

coordinateLabel = GeneralVariable "coordinateLabel" FieldML.Library01.rc3dCoordLabels

-- | Demonstration of a geometric FEM field: achieved via applying a scalar field template to a 'slice' of the DOFs created by partial evaluation.
geometricFieldExpression =
  Lambda 
  (Tuple [
    (Tuple [
      elementId,
      xi
    ]),
    coordinateLabel
  ])

  ( Apply
    ( PartialApplication 
      scalarFieldTemplate
      1 
      (PartialApplication coordinatesAtNodes 1 coordinateLabel)
    )
    (Tuple [elementId, xi])
  )
