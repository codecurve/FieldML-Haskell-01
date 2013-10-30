module FieldML.Utility.ExpressionTree (
  expressionTree
)
where

import FieldML.Core

import Data.Tree

buildTreeHelper :: Expression -> (Expression, [Expression])

buildTreeHelper x@(UnitElement) = (x, [])
buildTreeHelper x@(BooleanConstant _) = (x, [])
buildTreeHelper x@(RealConstant _) = (x, [])
buildTreeHelper x@(LabelValue _) = (x, [])
buildTreeHelper x@(GeneralVariable _ _) = (x, [])
buildTreeHelper x@(Unspecified _) = (x, [])
buildTreeHelper x@(Cast x1 _) = (x, [x1])
buildTreeHelper x@(Tuple xs) = (x, xs)
buildTreeHelper x@(Project _ x1) = (x, [x1])
buildTreeHelper x@(Lambda x1 expr) = (x, [x1, expr])

buildTreeHelper x@(Inverse f) = (x, [f])
buildTreeHelper x@(Lambdify expr) = (x, [expr])
buildTreeHelper x@(Apply f x1) = (x, [f, x1])
buildTreeHelper x@(Compose f g) = (x, [f, g])
buildTreeHelper x@(PartialApplication f _ x1) = (x, [f, x1])
buildTreeHelper x@(Where expr locals) = (x, expr:locals)

buildTreeHelper x@(And      x1 x2) = (x, [x1, x2])
buildTreeHelper x@(LessThan x1 x2) = (x, [x1, x2])
buildTreeHelper x@(Equal    x1 x2) = (x, [x1, x2])
buildTreeHelper x@(Plus     x1 x2) = (x, [x1, x2])
buildTreeHelper x@(Minus    x1 x2) = (x, [x1, x2])
buildTreeHelper x@(Times    x1 x2) = (x, [x1, x2])
buildTreeHelper x@(Divide   x1 x2) = (x, [x1, x2])
buildTreeHelper x@(Modulus  x1 x2) = (x, [x1, x2])
buildTreeHelper x@(Power    x1 x2) = (x, [x1, x2])
buildTreeHelper x@(Exists   x1 x2) = (x, [x1, x2])

buildTreeHelper x@(Not    x1) = (x, [x1])
buildTreeHelper x@(Negate x1) = (x, [x1])
buildTreeHelper x@(Sin    x1) = (x, [x1])
buildTreeHelper x@(Cos    x1) = (x, [x1])
buildTreeHelper x@(Exp    x1) = (x, [x1])
buildTreeHelper x@(Min    x1) = (x, [x1])
buildTreeHelper x@(Max    x1) = (x, [x1])
buildTreeHelper x@(ElementOf x1 _) = (x, [x1])

buildTreeHelper x@(Pi) = (x, [])
buildTreeHelper x@(If x1 vt vf) = (x, [x1, vt, vf])
buildTreeHelper x@(Restriction _ f ) = (x, [f])
buildTreeHelper x@(Interior _) = (x, [])

buildTreeHelper x@(MultiDimArray (AlgebraicVector x1) _) = (x, [x1])
buildTreeHelper x@(MultiDimArray _ _) = (x, [])

buildTreeHelper x@(Contraction x1 _ x2 _) = (x, [x1, x2])
buildTreeHelper x@(KroneckerProduct xs) = (x, xs)
buildTreeHelper x@(DistributedAccordingTo expr f) = (x, [expr, f])
buildTreeHelper x@(DistributionFromRealisations xs) = (x, xs)

buildTreeHelper x = error ("buildTreeHelper not implemented yet for this Expression constructor. Args: " ++ show x)


expressionTree :: Expression -> Tree Expression
expressionTree = unfoldTree buildTreeHelper

