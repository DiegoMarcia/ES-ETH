(** This module defines the expressions used to simulate the EVM operand stack, some stack manipulation functions
    and expression/stack conversion to string *)


(** Type used to represent expressions. *)
type expression =
  Val of string                           (** A simple value, like the PUSH operand *)
| Exp of string * (expression * int) list (** An abstract expression, name+operands *)
| Unknown                                 (** Remainder of the received stack (disconnected) *)

(** Type used to represent stack elements. Carries PC of push *)
and expression_with_pc = (expression * int)

(** Type renaming. A bit useless. *)
type t = expression_with_pc list

(** Obfuscates the fact that a stack is just a fancy list *)
val empty : t


(** Converts an [expression_with_pc] to a string.
   
    Examples:
    - [string_of_stack_element Val ("0xdeadbeaf", 0)] returns ["0xdeadbeaf"]
    - [string_of_stack_element Exp ("ADD", ["0x00000000"; "0xdeadbeaf"])] returns ["ADD( 0x00000000, 0xdeadbeaf )"] *)
val string_of_stack_element : expression_with_pc -> string


(** Used to fetch stack elements. [cut number stack] returns the first [number]
    elements in [stack] and the remainder of the stack.
    @raise BufOver if caller tries to fetch a negative number of elements.
    @raise BufUnder if [stack] is too short to fetch [number] elements. *)
val cut : int -> t -> t * t


(** [exec inst op pc] symbolic execution of instruction [instr] with operand [op],
    calculates PC of next instruction based on [pc] and type of [instr].
    If [instr] is a {b PUSH}, a {b SWAP} or a {b DUP}, it is performed *)
val exec : Instruction.t -> string -> int -> t -> t * int


(** [get_operands stack inst] returns from [stack] the operands needed by [instr]. *)
val get_operands : t -> Instruction.t -> t