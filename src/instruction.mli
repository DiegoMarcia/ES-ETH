(** This module defines the internal representation of an EVM instruction, String
    conversion of instructions, some functions to recognise some kinds of
    instructions as well as next PC calculation. *)


(** Record type used to represent instructions. *)
type t = {
      name   : string ; (** Mnemonic of the instruction *)
      reads  : int ;    (** Number of operands removed from the stack *)
      writes : bool ;   (** All EVM instructions either write one value or none *)
   }

(** {b STOP} instruction and arithmetic instructions.
    
    Opcodes {b 0x00} to {b 0x0b} *)
val stoparith : t list

(** Logic and byte-extraction instructions.
    
    Opcodes {b 0x10} to {b 0x1a} *)
val logicbyte : t list

(** Instructions to retrieve the execution environment information.
    
    Opcodes {b 0x30} to {b 0x3c} *)
val envirinfo : t list

(** Instructions to retrieve the containing block information.
    
    Opcodes {b 0x40} to {b 0x45} *)
val blockinfo : t list

(** Instructions to manipulate the stack, memory, storage and the execution flow.
    
    Opcodes {b 0x50} to {b 0x5b} *)
val storeflow : t list

(** Account creation, message-calls, and return.
    
    Opcodes {b 0xf0} to {b 0xf4} *)
val crcallret : t list

(** [is_push i] returns [true] if [i] is a {b PUSH1}, {b ...}, {b PUSH32}
    instruction, and [false] otherwise. *)
val is_push : t -> bool

(** [is_dup i] returns [true] if [i] is a {b DUP1}, {b ...}, {b DUP16}
    instruction, and [false] otherwise. *)
val is_dup : t -> bool

(** [is_swap i] returns [true] if [i] is a {b SWAP1}, {b ...}, {b SWAP16}
    instruction, and [false] otherwise. *)
val is_swap : t -> bool

(** [is_jumpdest i] returns [true] if [i] is a {b JUMPDEST} instruction, and
    [false] otherwise. *)
val is_jumpdest : t -> bool

(** [is_jumpi i] returns [true] if [i] is a {b JUMPI} instruction, and
    [false] otherwise. *)
val is_jumpi : t -> bool

(** [is_jump i] returns [true] if [i] is a {b JUMP} instruction, and [false]
    otherwise. *)
val is_jump : t -> bool

(** [is_terminal i] returns [true] if [i] is a {b RETURN}, {b STOP}, {b INVALID}
    or {b SUICIDE} instruction, and [false] otherwise. *)
val is_terminal : t -> bool

(** [next_pc i o PC] returns the position in the bytecode of next instruction
    after [PC]. This depends on the specific instruction [i] at [PC] and its
    operand [o]. *)
val next_pc : t -> string -> int -> int

(** Provides safer substring functionality than [String.sub] (doesn't fail if
    string is too short or arguments are negative). *)
val substr : string -> int -> int -> string

(** [get_source startPos length source] returns a portion of length [length] from
    [source] starting at PC [startPos]. *)
val get_source : int -> int -> string -> string

(** [get_source_size source] returns the size of [source] *)
val get_source_size : string -> int

