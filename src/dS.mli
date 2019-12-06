(** This module defines some kinds of useful Map types. The submodules are used to masquerate
    these type names, so to make interfaces more readable. *)

(** General module for maps with [int] (in this case, representing Program Counter) keys *)
module PCMap : Map.S with type key = int

(** General module for sets of [int] (in this case, representing Program Counter) elements *)
module PCSet : Set.S with type elt = int
