
TODO markdown formatting and formatting in general
TODO add also other steps here (not just ADTs).

This pass contains an algorithm for solving a set of constraints
(in this case related to the kinds of types).
This is used to enrich the Tyvar and Tycon data types with kind information
(needed for the typechecker step).

The way the algorithm infers kinds is as follows
(using the Maybe type as an example):

data Maybe a where
  Just :: a -> Maybe a
  Nothing :: Maybe a

1. Collect all "equations" on type level:
  - Maybe a :: *       (ADT head)
  - Maybe :: ?         (ADT head)
  - a :: ?             (ADT head)
  - a -> Maybe a :: *  (Just)
  - Maybe a :: *       (Nothing)
2. Since the way the algorithm currently works generates fresh placeholder
   variables, we also have to add constraints for which variables are the
   same at the end.
3. Add all constraints from external types (this includes the "->" type!)
  - ->: * -> (* -> *)
  In this case, it will now be able to also detect that a :: *.
3. Group all constraints together into 1 big collection.
4. Use the constraint solving algorithm to find the kinds of each type
   or give an error if there are conflicting constraints.
5. Enrich the type without kinds to now also have kind information.

Kinds are first inferred for all ADTs. Afterwards, the rest of the AST
can have kinds inferred based on this extra information

