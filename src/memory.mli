(** This module implements EVM memory. Keys have [int] type, values have type [string], possibly convertible to [int] *)


(** Memory type *)
type t

(** Execution starts with an empty memory *)
val empty : t

(** Returns the size in bytes of a memory instance *)
val memory_size : t -> int


(** Stores data to memory.
    
    [store length position value memory] stores [value] at position [position] in [memory].
    If length of [value] doesn't match [length], it gets padded or shortened accordingly. *)
val store : int -> int -> string option -> t -> t


(** Retrieves data from memory.
    
    [extract_string numOfBytes start m] extracts [numOfBytes] (usually, 8 or 32) bytes from
    [m] starting at [start].
    If [m] is too short, result is zero-padded to match [numOfBytes].

    @return a string that might be convertible to [int] if required data was known upon write. *)
val extract_string : int -> int -> t -> string