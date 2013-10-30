module FieldML.Library02 (
  normalDistribution
) where

import FieldML.Core

-- Uncertainty 
normalDistribution = 
  Lambda (Tuple[ 
    GeneralVariable "mean" Reals, 
    GeneralVariable "variance" Reals
  ])
  (
    Lambda 
    (GeneralVariable "x" Reals)
    (
      (
        (RealConstant 1.0) 
        `Divide` 
        ( (GeneralVariable "variance" Reals) 
          `Times` 
          ( ((RealConstant 2.0) `Times` Pi) `Power` ((RealConstant 1.0) `Divide` (RealConstant 2.0)) )
        )
      )
     `Times`
      ( Exp  
        ( (Negate ((RealConstant 1) `Divide` (RealConstant 2)) )
          `Times`
          (Power 
            (
              ((GeneralVariable "x" Reals) `Minus` (GeneralVariable "mean" Reals))
              `Divide`
              (GeneralVariable "variance" Reals)
            )
            (RealConstant 2)
          ) 
        )
      )
    )
  )
