module FieldML.Utility.ExpressionTree (
  expressionTree
)
where

import FieldML.Core

import Data.Tree

buildTreeHelper :: (Show a, Eq a) => Expression a -> (Expression a, [Expression a])

buildTreeHelper x@(UnitElement _) = (x, [])
buildTreeHelper x@(BooleanConstant _ _) = (x, [])
buildTreeHelper x@(RealConstant _ _) = (x, [])
buildTreeHelper x@(LabelValue _ _) = (x, [])
buildTreeHelper x@(GeneralVariable _ _ _) = (x, [])
buildTreeHelper x@(Unspecified _ _) = (x, [])
buildTreeHelper x@(Cast _ x1 _) = (x, [x1])
buildTreeHelper x@(Tuple _ xs) = (x, xs)
buildTreeHelper x@(Project _ _ x1) = (x, [x1])
buildTreeHelper x@(Lambda _ x1 expr) = (x, [x1, expr])

buildTreeHelper x@(Inverse _ f) = (x, [f])
buildTreeHelper x@(Lambdify _ expr) = (x, [expr])
buildTreeHelper x@(Apply _ f x1) = (x, [f, x1])
buildTreeHelper x@(Compose _ f g) = (x, [f, g])
buildTreeHelper x@(PartialApplication _ f _ x1) = (x, [f, x1])
buildTreeHelper x@(Where _ expr locals) = (x, expr:locals)

buildTreeHelper x@(And      _ x1 x2) = (x, [x1, x2])
buildTreeHelper x@(LessThan _ x1 x2) = (x, [x1, x2])
buildTreeHelper x@(Equal    _ x1 x2) = (x, [x1, x2])
buildTreeHelper x@(Plus     _ x1 x2) = (x, [x1, x2])
buildTreeHelper x@(Minus    _ x1 x2) = (x, [x1, x2])
buildTreeHelper x@(Times    _ x1 x2) = (x, [x1, x2])
buildTreeHelper x@(Divide   _ x1 x2) = (x, [x1, x2])
buildTreeHelper x@(Modulus  _ x1 x2) = (x, [x1, x2])
buildTreeHelper x@(Power    _ x1 x2) = (x, [x1, x2])
buildTreeHelper x@(Exists   _ x1 x2) = (x, [x1, x2])

buildTreeHelper x@(Not    _ x1) = (x, [x1])
buildTreeHelper x@(Negate _ x1) = (x, [x1])
buildTreeHelper x@(Sin    _ x1) = (x, [x1])
buildTreeHelper x@(Cos    _ x1) = (x, [x1])
buildTreeHelper x@(Exp    _ x1) = (x, [x1])
buildTreeHelper x@(Min    _ x1) = (x, [x1])
buildTreeHelper x@(Max    _ x1) = (x, [x1])
buildTreeHelper x@(ElementOf _ x1 _) = (x, [x1])

buildTreeHelper x@(Pi _) = (x, [])
buildTreeHelper x@(If _ x1 vt vf) = (x, [x1, vt, vf])
buildTreeHelper x@(Restriction _ _ f ) = (x, [f])
buildTreeHelper x@(Interior _ _) = (x, [])

buildTreeHelper x@(MultiDimArray _ (AlgebraicVector x1) _) = (x, [x1])
buildTreeHelper x@(MultiDimArray _ _ _) = (x, [])

buildTreeHelper x@(Contraction _ x1 _ x2 _) = (x, [x1, x2])
buildTreeHelper x@(KroneckerProduct _ xs) = (x, xs)
buildTreeHelper x@(DistributedAccordingTo _ expr f) = (x, [expr, f])
buildTreeHelper x@(DistributionFromRealisations _ xs) = (x, xs)

buildTreeHelper x = error ("buildTreeHelper not implemented yet for this Expression constructor. Args: " ++ show x)


expressionTree :: (Show a, Eq a) => Expression a -> Tree (Expression a)
expressionTree = unfoldTree buildTreeHelper

