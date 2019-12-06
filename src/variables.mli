type t

(** Registers a SLOAD / SSTORE operation *)
val storage : Data.t -> string -> OpStack.expression_with_pc -> t -> Message.log -> t * Message.log

(** Registers a CALL operation *)
val receivers : Data.t -> Evaluate.t -> OpStack.expression_with_pc -> Evaluate.t -> t -> Message.log -> t * Message.log

(** Inits *)
val noVariables : Data.t -> t

(** Prints records to stdout *)
val print : t -> unit