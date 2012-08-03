module FieldML_test_mesh01
  (
    mesh_SansConnectivity,
    globalNodesFSet,
    elementIdFSet,
    localNodeFSet,
    elementId,
    localNode,
    localToGlobalNodes,
    nodalDofsForElementExpr,
    fieldTemplate,
    pressureAtNodes
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
    (DomainMapConstant FieldML.Library01.unitSquare) 

xi = (GeneralVariable "ξ" FieldML.Library01.unitSquare) 



-- Parameter map test: Global nodes:
-- 4 5 6
-- 1 2 3

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

pressureAtNodes = MultiDimArray 
  (RealParameterVector
     [  0.1,      0.5,  55.9, 
        -0.4,   -100.9,  19.0 ] 
  )
  globalNodesFSet

{-
pressureField = 
  Lambda 
    (Tuple [GeneralVariable "elementId" elementIdFSet, GeneralVariable "ξ ])
-}

-- MultiDimArray s are Lambda s, hence indexing is by means of application, and slices and slabs can be retrieved via partial application.
elementIdToGlobalNodes = 
  Lambda 
  (GeneralVariable "elementId" elementIdFSet)
  (PartialApplication localToGlobalNodes 1 (GeneralVariable "elementId" elementIdFSet))


-- Field template
localToGlobalNodesMapSignature = SignatureSpace (CartesianProduct [ elementIdFSet, localNodeFSet ]) globalNodesFSet
localToGlobalNodesVar = (GeneralVariable "localToGlobalNodes" localToGlobalNodesMapSignature)

dofSourceSignature = SignatureSpace globalNodesFSet Reals
dofSourceVar = (GeneralVariable "dofSource" dofSourceSignature)
                         
nodalDofsForElementExpr = 
  Lambda 
  (Tuple [
    dofSourceVar,      
    localToGlobalNodesVar,            
    elementId,
    localNode
  ]) 
  (Apply dofSourceVar ((Apply localToGlobalNodesVar (Tuple [elementId, localNode]))))

nodalDofsForElementSignature = SignatureSpace (Domain nodalDofsForElementExpr) (Codomain nodalDofsForElementExpr)
nodalDofsForElementVar = GeneralVariable "nodalDofsForElementVar" nodalDofsForElementSignature

fieldTemplate = 
  Lambda
  (Tuple [
    dofSourceVar,
    nodalDofsForElementVar,
    elementId,
    xi
  ])
  (Contraction
    (Lambda 
      localNode
      (Apply 
        (Tuple [
          dofSourceVar,      
          localToGlobalNodesVar,            
          elementId,
          localNode
        ]) 
        nodalDofsForElementVar 
      )
    )
    1
      
    (Apply FieldML.Library01.basis2dLinearLagrange xi)
    1
  )
