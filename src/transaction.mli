(** Represents a transaction *)
type t =
{
      origin    : string ;
      from      : string ;
      gas       : string option ;
      hash      : string option ;
      calldata  : string option ;
      recipient : string ;
      callvalue : string option ;
      gasprice  : string option ;
}

(** Placeholders for contract-only analysis *)
val noTransaction : string -> t

(** constructor for external CALLs *)
val internal : string -> string -> string option -> string option -> string -> string option -> string option -> t


val get_origin : t -> string
val get_from : t -> string
val get_calldata : t -> string option
val get_recipient : t -> string
val get_callvalue : t -> string option
val get_gasprice : t -> string option