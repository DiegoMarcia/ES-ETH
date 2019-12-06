type expression =
  Val of string
| Exp of string * t
| Unknown

and expression_with_pc = expression * int

and t = expression_with_pc list


(** Buffer underflow, thrown if the operand stack happens to be smaller than
    expected. Carries the operator mnemonic, the number of expected operands
    and the stack as received by the throwing function. *)
exception BufUnder of string * int * string

(** Buffer overflow, thrown in case caller tries to operate on nonexistent stack
    elements (zero or negative positions, since top of the stack is 1) *)
exception BufOver of string * int



(* element to string conversion *)
let rec string_of_stack_element (arg : expression_with_pc) =
   match arg with
     Val value, pc       -> value
   | Exp (name, ops), pc -> name ^ "( " ^ String.concat ", " (List.map string_of_stack_element ops) ^ " )"
   | Unknown, pc         -> "UNKNOWN"

(** Converts a [OpStack.t] to a string as a square-bracket-enclosed, double-colon-separated list of expressions. *)
let string_of_stack (s : t) = "[ " ^ String.concat " :: " (List.map string_of_stack_element s) ^ " ]"


(* operations on stack *)

(** Implements {b SWAP} instruction. Takes top element, brings it to the desired
    position, takes the there-found element and brings the latter to the top.
    @raise BufOver if caller tries to swap elements above the top.
    @raise BufUnder if the stack is too short to perform the swap. *)
let swap n stack : t = 
   if n < 1 then
      raise (BufOver ("SWAP", n)) (* I't sometimes nice to have some unnecessary complexity *)
   else
      let fromTop = List.hd stack in
      let rec swap_helper n stack =
         let currentElement = List.hd stack in
         if n = 0 then
            (currentElement, fromTop::List.tl stack)
         else
            let (bringToTop, swapped) = swap_helper (n-1) (List.tl stack) in
               (bringToTop, currentElement :: swapped)
      in
         try
            let (putOnTop, swapped) = swap_helper (n-1) (List.tl stack) in
               putOnTop :: swapped
         with
            _ -> raise (BufUnder ("SWAP", n, (string_of_stack stack)))


(** Implements {b DUP} instruction. Duplicates the desired stack element and puts it
    to the top of the stack.
    @raise BufOver if caller tries to duplicate an element above the top.
    @raise BufUnder if the stack is too short to perform the duplication. *)
let dup n stack : t = 
   if n < 1 then
      raise (BufOver ("DUP", n))
   else
      try
         let dupped = List.nth stack (n-1) in
            dupped :: stack
      with
         _ -> raise (BufUnder ("DUP", n, (string_of_stack stack)))


(** Implements {b POP} instruction. Removes an element from the top position in the
    stack.
    @raise BufUnder if the stack is empty. *)
let pop (stack : t) : t =
   match stack with
     []   -> raise (BufUnder ("POP", 1, "[  ]"))
   | a::l -> l


(* Fetch stack elements. *)
let cut (n : int) (stack : t) : t * t =
      if n < 0 then
         raise (BufOver ("cut", n))
      else
         let rec cut_helper n acc stack =
            if n = 0 then
               (List.rev acc, stack)
            else
               cut_helper (n-1) (List.hd stack :: acc) (List.tl stack)
         in
            try
               cut_helper n [] stack
            with
               _ -> raise (BufUnder ("cut", n, (string_of_stack stack)))


(* All the changes to operands stack and next PC computation *)
let exec i op pc stack =
   let nextPC = Instruction.next_pc i op pc in
   let stack' =
      if Instruction.is_push i then
         (Val op, pc) :: stack
      else if Instruction.is_swap i then
         swap i.Instruction.reads stack
      else if Instruction.is_dup i then
         dup i.Instruction.reads stack
      else
         (* put symbol *)
         let arity = i.Instruction.reads in
         let (operands, rest) = cut arity stack in
         if i.Instruction.writes then
            (Exp (i.Instruction.name, operands), pc) :: rest
         else
            rest
   in
   stack', nextPC


(* Operands needed by instruction *)
let get_operands stack instruction =
   let arity = instruction.Instruction.reads in
   let (operands, rest) = cut arity stack in
   operands


let empty = []

(* Avoids Stack Underflow when starting computation at an arbitrary point in code *)
let unknownStack = [(Unknown, 0) ; (Unknown, 1) ; (Unknown, 2) ; (Unknown, 3) ; (Unknown, 4) ; (Unknown, 5) ; (Unknown, 6) ; (Unknown, 7) ; (Unknown, 8) ; (Unknown, 9) ; (Unknown, 10) ; (Unknown, 11) ; (Unknown, 12) ; (Unknown, 13) ; (Unknown, 14) ; (Unknown, 15) ; ]

