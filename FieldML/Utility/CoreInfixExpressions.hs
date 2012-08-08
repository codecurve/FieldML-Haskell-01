-- | Utility to provide infix versions of some Expression () constructors.
module FieldML.Utility.CoreInfixExpressions (
  plus,
  minus,
  times,
  divide,
  power,
  equals,
  lessThan,
  and',
  or',
  where'
) where

import FieldML.Core

plus = Plus ()
minus = Minus ()
times = Times ()
divide = Divide ()
power = Power ()
equals = Equal ()
lessThan = LessThan ()
and' = And ()
or' = Or ()
elementOf = ElementOf ()
where' = Where ()