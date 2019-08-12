
# Kind inference

This file describes how the kind inference algorithm works for future reference.
In Besra itself, there is no possibility to add kind information to declarations
yet. However, the type system needs this information for some of the more
advanced features of the typesystem. In order to infer the kinds of all types,
we use an constraint-solver/unification algorithm similar to type inference
algorithms used in Hindley/Milner based languages. For more information, it's
probably best to consult some
[tutorials](http://dev.stephendiehl.com/fun/006_hindley_milner.html) or
[resources](https://www.cl.cam.ac.uk/teaching/1415/L28/type-inference.pdf)
on this topic.

The kind system resembles Haskell98's system closely.
Kinds can have only 2 possible "shapes":

1. `Type` (or `*`)
2. `Type -> Type` (or `* -> *`)

Another way to read this is that for values of (1) it is possible to think of
concrete values (e.g. Int: 1, 2, 3); but not for (2) ("Maybe" requires another
type to actually mean something, for example: Maybe Int: Just 1, Nothing, ...).


## Algorithm in general

The algorithm performs inference in 4 steps in order to infer kinds for all
type-level expressions:

1. ADTs
2. Traits
3. Impls
4. Other expressions / declarations

After each step, the algorithm obtains more information which it uses in
subsequent steps. Each of these steps is described in 1 of the sections below.
If an error occurs (kind unification failure, infinite kinds, ...), the
algorithm aborts and raises a corresponding error.


## Kind inference for ADTs

Kinds are first computed for ADTs, since there are no other dependencies except
for other data types (potentially in other files).

1. Sort/group ADTs based on the way they refer to each other.
2. For each ADT group, gather a set of assumptions & constraints
3. Run the solver.
4. Apply the resulting solution back to the ADT group,
   resulting in the actual solution.
5. Update the state to contain the newly inferred ADTs.
6. Repeat until all ADT groups inferred.


For step 1, a graph is created of all ADTs and the ADTs to which they
refer in their data constructors. The strongly connected components of the graph
are then computed, ordering the ADTs by their dependencies in groups
(mutually referring to one another inside a group).

The gathering of constraints/assumptions is as follows
(taking Haskell's definition of Maybe as an example):

```haskell
data Maybe a where
  Just :: a -> Maybe a
  Nothing :: Maybe a
```
(Note: GADT syntax is currently not supported in Besra, but it shows the used
types much more clearly.)


1. Collect all constraints:
  - Maybe a ~ Type         (ADT head)
  - Maybe ~ ?              (ADT head)
  - a ~ ?                  (ADT head)
  - (a -> Maybe a) ~ Type  (Just constructor)
  - (Maybe a) ~ Type       (Nothing constructor)
2. The algorithm generates fresh placeholder variables durig the inference
   algorithm. However, all "a" type variables are the same so additional
   constraints need to be generated for these as well.
3. Add all constraints from external types (this includes the "->" type with
   kind `Type -> (Type -> Type)`
   In this case, it will now be able to also detect that a ~ Type.
3. Group all constraints together into 1 big collection.
4. Use the constraint solving algorithm to find the kinds of each type
   or give an error if there are conflicting constraints.
5. Enrich the type without kinds to now also have kind information.


## Kind inference for traits

Traits form the most complicated part of the algorithm since there is so much
information at the type level that needs to be taken into account. On a high
level, the steps are mostly the same as kind inference for ADTs:

1. Sort traits by the way they refer to each other.
   (Currently traits can't mutually refer to each other.)
2. For each trait, gather a set of assumptions & constraints.
3. Run the solver.
4. Apply the resulting solution back to the trait,
   resulting in the actual solution.
5. Update the state to contain the newly inferred trait.
6. Repeat until all traits inferred.


```besra
trait Functor f where
  map : (a -> b) -> (f a -> f b)

trait Functor f => Applicative f where
  pure : a -> f a
  (<*>) : f (a -> b) -> a -> b
```

Given the following snippet of Besra code above, it needs to gather the
following constraints (for applicative):

1. f ~ Type -> Type (inferred from Functor definition)
2. All `f` type variables used in the entire trait block are the same.
3. All variables not mentioned in the trait-head (in this case `a` and `b`)
   are the same in a single type declaration.
4. If concrete types are used, look up their kind (obtained during ADT
   kind-inference)


## Kind inference for impls

After inference of ADTs and traits, kinds for impls can be inferred/checked too.
Like before, it's most easily explained with an example:

```besra
trait Eq a where
  (==) : a -> a -> Bool

impl Eq a => Eq (Maybe a) where
  map = ...
```

The following constraints are generated:

1. Kinds for constraints are looked up (leading to: a ~ Type).
2. Kinds for current impl is looked up (giving us: Maybe a ~ Type).
3. Same variables in impl head are constrainted to have same kind.

If type annotations occur inside the impl block, they are unrelated to the
impl-head. (Meaning if `a` occurs there again, it's a different type var `a`.)


## Kind inference for regular type annotations

The final step of the algorithm is to traverse the AST and enrich all type
annotations with kind information. For each type annotation it comes across, it
does the following steps:

1. Gather assumptions & constraints
2. Run solver
3. Apply solution back to type annotation

```besra
trait Functor f where
  map : (a -> b) -> (f a -> f b)

(<$) : Functor f => a -> f b -> f a
```

During kind inference of type annotations, the following constraints are
gathered, using snippet above as an example):

1. Kinds are looked up for eventual predicates (f ~ Type -> Type)
2. ADT types are looked up as well in a similar way.
2. All type variables with the same name are equated.
3. The resulting kind of the entire type signature is of kind `Type`.

