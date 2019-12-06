(** This modules performs evaluation from stack symbols to Big_int values. Conversion to string is included. *)

(** An evaluated value can be either a Big_int (success) or a list of instructions evaluation depends upon (failure) *)
type t = Eval of Big_int.big_int | NoEval of string list

(** Tries to evaluate a symbol *)
val evaluate : Data.t -> OpStack.expression * int -> t

(** Checks if all list elements have been successfully evaluated  *)
val all_Eval : t list -> bool

(** String conversion  *)
val hex_of_big_int : Big_int.big_int -> string