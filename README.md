FieldML Haskell prototype - Discussion notes
============================================

This prototype is an attempt at using Haskell to prototype FieldML design ideas.
These ideas are based on discussions of the FieldML design work group at the Auckland Bioengineering Institute at Auckland University.  Current members of the FieldML design group are: Poul Nielsen, Richard Christie, Andrew Miller, Alan Wu and myself (Randall Britten).
In the past, others have also contributed to these discussions, notably Caton Little, Chris Bradley and Peter Hunter.

The key structures are in FieldML/Core.hs
Some examples are in FieldML_test_mesh01.hs and FieldML_test1.hs.
The unit tests can be run using "runhaskell FieldML_test1.hs".
Recent results of the unit tests are in "test-log.txt". 

Comparison with ModML
---------------------
Many ideas from ModML influenced the early development of this prototype.
One important difference is that ModML aims to create a DSL embedded in Haskell that end users can use.  In contrast, the current state of this FieldML Haskell prototype is not to create an end-user language, but rather, to prototype the FieldML object model.
Thus, at this stage, there is still heavy reliance on features of Haskell that would need an equivalent in the FieldML object model, e.g. Haskell lists.


Areas of exploration and features
---------------------------------
 * Domain and codomain inference (mostly implemented, still WIP).
 * Validity checking (mostly implemented, still WIP).
 * Mesh refinement (todo).
 * Mapping from representations in this prototype to representations in FEM software such as OpenCMISS (www.opencmiss.org). (todo).
 * Mapping from representations in this prototype to MathML and XML/FieldML wrapping.

Random thoughts and issues
--------------------------
 * Are list expressions and constructors needed at the FieldML level?  Currently, many constructors depend on lists at the lower level language syntax.  Perhaps tuples are fine, since they serve the role of homogenous lists as well as inhomogenous lists.
 * Possibly an XML representation will naturally allow lists to be expressed where necessary, just by repeating XML element types as children of the relevant parent.

 * Additional algebraic structure, e.g. metric, algebraic operations: for some FSet constructions, these can be induced.
 * Need to be able to recognise massive homogeneity in order to optimise, e.g. for OpenCMISS.

 * Note: Maps and Values can be seen as the same thing, a value is just a map whose domain is the UnitSpace. However, it is pragmatic to differentiate between the two. Also, always treating values as maps from UnitSpace is recursive, i.e. v is the same as us → v is the same as us → (us → v), etc.

 * What about expressions that evaluate to an FSet? E.g. f::Reals → FSet s.t. f(x) = SimpleSubset Reals -x x
Andrew Miller pointed out that "Domains" should be static so that FieldML should be a statically typed language. 
Perhaps expressions that produce FSets can still fit into a statically typed FieldML?  Otherwise, perhaps FieldML cannot be statically typed in this way, especially since tangent space declaration depends on the point in a manifold to which the space is tangent.

 * Factors of Cartesian products and Tuples are referred to ordinally, rather than by allocating names to each factor, as is the plan for FieldML.  This shortcut was taken for the sake of simplicity, and to gain some feeling for this style for the sake of comparison with the named style.  Advantages of ordinal style: matches current mathematical convention; makes constructing tuples easy, which makes expressions more terse. Advantages of naming: easier to keep track of the meaning of each factors.

Todo list
---------
 * Instance Show to make expressions pretty (new lines, indentation).  But having Show do Haskell syntax is helpful for debugging.  Rather have another string rendering for simple rendition, and even multiple such rendition options, such as to content or presentation MathML.
 * Use singular names for FSets, as agreed at FieldML meeting on 30 July 2012.
 * Ensure that there is a sensible way to create a value for each FSet, for example, a Tuple is used to represent a value of a disjoint union, but currently it is treated as taking values on a Cartesian product.  This is partially done by the introduction of "Cast", but more examples and testing and thought required.
 * Introduce closures?  Or is the "where" construction sufficient? Since NamedExpression is a valid expression type, this would require implement the model itself, which would have a list of FieldML objects, an assignment of names to expressions would be one way, the other way is to have assignments equating general variables with expressions.
 * Change to using HUnit rather than QuickTest, or use QuickTest properly.
 * Validation:
   Other validation
   When validation fails, report the reason.

 * Represent derivative continuity intention at connected points
 * "Versions", i.e. different values for a field at a node to be used for interpolation of a field, depending on which element.
 * Generate discretised points in an FSet (aka "Grid points").

 * Try use classes for each type of FSet, each type of Expression operator etc. (e.g. Binary Boolean operators).

 * Validation of FSet definitions, and improve validation of Expression's to include validating FSets that are part of definition, and vice versa.

 * Consider using type classes rather than just different constructors.  Might help ensuring that validate, codomain, domain and listOfFreeGeneralVariables are always implemented when new types are introduced.  On the other hand, it means that more of the validation rules have to be included in the validation routines rather than being provided for free by the Haskell syntax rules, which is actually an advantage in terms of making the data model rules more visible.

 * Consider making "Apply" explicit, as in MathML and OpenMath. For most maps, it is implicit, e.g. Plus f g.

 * Conversion to/from MathML (via Andrew Miller's Haskell MathML utility?).

Done:
 * Field template.  Previously had fields that were tied directly to DOFs.  A field template allows DOFs source to be a parameter. An example has been created.
 * Contraction
 * Tensor product basis functions
 * Disjoint Union
 * Separate processing of structure (e.g. validate, domain, codomain, listOfFreeGeneralVariables) from Structure constructors.

 * Read a bit about f5 (fibrebundle.net).
 * Validation:
   Validate Maps like "And" to check that both operands have consistent domains, and codomains are Boolean.
 * Connectivity

 * FEM example: 
   DOF mapping

 * Coordinate system transformation

 * Partial application, binding, composition, projection from a general variable.
 * Arbitrary basis function specification using something like MathML


Thoughts on suitability of Haskell vs Scala for this prototype
--------------------------------------------------------------

 * Scala plus factors: has inheritance.  Has case classes.  Nice support in Eclipse.
 * Scala minus factors: syntax more verbose than Haskell. Can't neatly combine case classes with inheritance though, messy "unapply".
 * Haskell plus factors: terse syntax. 
 * Haskell minus factors: Tools not great.


Original idea sketch
--------------------
The goal was to be able to represent things similar to the following:
(As at 2012-07-25, this goal has actually essentially been achieved)

    M1 :: FSet
    M1 = Reals

    M2 :: FSet
    M2 = CartesianProduct[Reals, Reals]

    f1 :: RealExpression
    f1 = x + y
    domain(f1) == CartesianProduct[Reals, Reals]
    codomain(f1) = Reals

    f2 :: Expression
    domain(f2) == CartesianProduct[Reals, Reals]
    codomain(f2) = Reals
    f2 x = Project(1, x) + Project(2, x)
Or
    f2 = lambda x → Project(1, x) + Project(2, x)

Naming Expressions so that we can write point free composition?

Tensor product:
    f3 = Tuple[1-x, x]
    TensorProduct(f3, f3)

Would hopefully be:
    Tuple[
      (1-x)(1-y)
      x(1-y)
      (1-x)y
      xy
    ]


Mapping to OpenCMISS
--------------------

The goal of this will be to represent a mesh and a the relevant fields for a FEM problem that OpenCMISS can solve, and map the FieldML-Haskell representation of the mesh and the fields to the OpenCMISS representation.
It was necessary to try to strike a balance between keeping the prototype simple and getting the prototype's representation of domains and fields abstract enough so that the mapping to OpenCMISS is a reasonable exploration of the issues that may be problematic.
This is now the next main focus of this work.
