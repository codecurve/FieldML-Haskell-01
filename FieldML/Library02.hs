module FieldML.Library02 (
  normalDistribution
) where

import qualified FieldML.Core as C
import qualified FieldML.Utility.CoreInfixExpressions as I


-- Uncertainty 
normalDistribution = 
  C.Lambda () (C.Tuple () [ 
    C.GeneralVariable () "mean" C.Reals, 
    C.GeneralVariable () "variance" C.Reals
  ])
  (
    C.Lambda () 
    (C.GeneralVariable () "x" C.Reals)
    (
      (
        (C.RealConstant () 1.0) 
        `I.divide` 
        ( (C.GeneralVariable () "variance" C.Reals) 
          `I.times` 
          ( ((C.RealConstant () 2.0) `I.times` C.Pi ()) `I.power` ((C.RealConstant () 1.0) `I.divide` (C.RealConstant () 2.0)) )
        )
      )
     `I.times`
      ( C.Exp ()
        ( (C.Negate () ((C.RealConstant () 1) `I.divide` (C.RealConstant () 2)) )
          `I.times`
          (I.power 
            (
              ((C.GeneralVariable () "x" C.Reals) `I.minus` (C.GeneralVariable () "mean" C.Reals))
              `I.divide`
              (C.GeneralVariable () "variance" C.Reals)
            )
            (C.RealConstant () 2)
          ) 
        )
      )
    )
  )
