(** This module provides instruction fetching and function detection functionality. *)

(** Invalid opcode in the received bytecode. Position and opcode are included in the Exception. *)
exception WrongOpcode of string * int

(** Tried to read at invalid position in the received bytecode. Position is included in the Exception. *)
exception OutOfBounds of int

(** Fetches the next token from the source at a given position.
    @raise WrongOpcode in case the source contains an invalid opcode at given position
    @raise OutOfBounds in case the position is less than 0 or too big *)
val get_token : string -> int -> Instruction.t * string
(**  A token is here a couple [(instruction, operand)] *)

(** Checks the destination address to be a valid JUMPDEST. EVM exceptions are thrown jumping to an invalid address. *)
val check_throw : string -> int -> bool

(** safely extract 32 bytes from string starting at given position *)
val extract_from : int -> string -> string

(** Finds the address at which a function has been invoked. Returns a pair (invoke's PC, function start PC).
    @raise Failure if invocation can't be found.
    @see < DS.Invokes.html >  DS.Invokes *)
(*val find_the_invoke : string -> int -> Invokes.t -> Functions.t -> Visited.t -> int -> Edges.Edges.t -> int * int*)
(** {b ASSUMPTION:} function calls follow this pattern:
{v 
   function f0() returns (int) \{
      return f1(f2(3));
   \}

   0x3c  JUMPDEST   #
   0x3d  PUSH1 0x42 # <-- RETURN ADDRESS FOR f0
   0x3f  PUSH1 0x5a # <-- FUNCTION START OF f0
   0x41  JUMP       # <-- f0 INVOKED HERE
   0x42  JUMPDEST   # <-- RESULT OF f0 ON TOP
   ...
   0x5a  JUMPDEST   #
   0x5b  PUSH1 0x68 # <-- RETURN ADDRESS FOR f1
   0x5d  PUSH1 0x64 # <-- RETURN ADDRESS FOR f2
   0x5f  PUSH1 0x03 # <-- ARGUMENT FOR f2
   0x61  PUSH1 0x8b # <-- FUNCTION START OF f2
   0x63  JUMP       # <-- f2 INVOKED HERE

   0x64  JUMPDEST   # <-- RESULT OF f2 ON TOP
   0x65  PUSH1 0x7a # <-- FUNCTION START OF f1
   0x67  JUMP       # <-- f1 INVOKED HERE

   0x68  JUMPDEST   # <-- RESULT OF f1 ON TOP
   ...
   0x6f  JUMP       # <-- f0 RETURNS
v}
    From this, two facts can be deduced:

    {ol
       {li The invocation follows the order:
          {ol
             {li [PUSH] return address}
             {li [PUSH] arguments}
             {li [PUSH] function start}
             {li [JUMP]}
          }
       }
       {li The return address is the [JUMPDEST] following function invocation}
    }
    
    [find_the_invoke] exploits the first fact. Further tests are required before deciding whether the second one
    (resulting in a much simpler function) should be used instead. *)

(** Checks if instructions are present at some point (SWARM metadata ca be there). *)
val end_of_code : string -> int -> bool


val anonymous_return : string -> int -> bool
