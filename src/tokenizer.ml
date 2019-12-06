open Instruction

exception WrongOpcode of string * int
exception OutOfBounds of int

let get_instruction bytecode pos =
	match int_of_string ("0x" ^ String.sub bytecode pos 2) with
	  n when n >= 0 && n <= 11    -> List.nth stoparith n
	| n when n >= 16 && n <= 26   -> List.nth logicbyte (n - 16)
	| 32                          -> { name = "SHA3"; reads = 2; writes = true; }
	| n when n >= 48 && n <= 60   -> List.nth envirinfo (n - 48)
	| n when n >= 64 && n <= 69   -> List.nth blockinfo (n - 64)
	| n when n >= 80 && n <= 91   -> List.nth storeflow (n - 80)
	| n when n >= 96 && n <= 127  -> { name = "PUSH" ^ string_of_int (n - 95); reads = 0; writes = true; }
	| n when n >= 128 && n <= 143 -> { name = "DUP"  ^ string_of_int (n - 127); reads = (n - 127); writes = true; }
	| n when n >= 144 && n <= 159 -> { name = "SWAP" ^ string_of_int (n - 143); reads = (n - 143); writes = true; }
	| n when n >= 160 && n <= 164 -> { name = "LOG"  ^ string_of_int (n - 160); reads = (n - 158); writes = false; }
	| n when n >= 240 && n <= 244 -> List.nth crcallret (n - 240)
   | 253                         -> { name = "REVERT"; reads = 2; writes = false; }
	| 254                         -> { name = "INVALID"; reads = 0; writes = false; }
	| 255                         -> { name = "SUICIDE"; reads = 1; writes = false; }
	| _ as wrong                  -> raise (WrongOpcode (("0x" ^ String.sub bytecode pos 2) ^ " or " ^ string_of_int wrong, (pos / 2)))

let get_operand bytecode pos =
   let checkIfPush = int_of_string ("0x" ^ String.sub bytecode pos 2) in
      if checkIfPush >= 96 && checkIfPush <= 127 then
         "0x" ^ substr bytecode (pos + 2) ((checkIfPush - 95) * 2)
      else
         ""

let get_token source pc =
   let pos = pc * 2 in
   if pos < 0 || pos >= (String.length source - 1) then
      raise (OutOfBounds pc)
   else
      let (instruction, operand) = (get_instruction source pos), (get_operand source pos) in
      (instruction, operand)


let check_throw source pc =
   try
      let i = get_instruction source (pc * 2) in
      i.name <> "JUMPDEST"
   with _ -> true

   
let extract_from startPos hexString =
   let startPos = startPos * 2 in
   (* Strip 0x *)
   let hexString =
      if String.length hexString > 1 && String.sub hexString 0 2 = "0x" then
         String.sub hexString 2 (String.length hexString - 2)
      else
         hexString
   in
   (* Pad with 0 *)
   let hexString =
      if (String.length hexString) - startPos < 64 then
         hexString ^ (String.make (64 - ((String.length hexString) - startPos)) '0')
      else
         hexString
   in
   String.sub hexString startPos 64

(*
(* TODO: change approach to mostly use existing edges *)
let find_the_invoke source setAt taken functions visited setNode edges =
   let rec find_the_invoke_h stack pc tmpvis lastNode =
      let instruction, operand = get_token source pc in

      if is_terminal instruction then
         failwith ((Printf.sprintf "%0#x: Bad bug. Was looking for a function using return address pushed at %0#x.\n" pc setAt))

      else if is_jumpdest instruction && PCMap.mem pc functions then
      (* Gently falling into a function without a JUMP *)
         (lastNode, pc)

      else if is_jump instruction || is_jumpi instruction then
         if PCMap.mem pc taken && snd (PCMap.find pc taken) <> setAt then
         (* This JUMP has already been assigned based on a different setAt (so is a jump to another function). Ignore this JUMP and skip function. *)
            let (stack', pc') = OpStack.exec instruction operand pc stack in
            let callee = fst (PCMap.find pc taken) in
            (* Get the function descriptor *)
            let (returnPCStackPos, (stackRemove, stackAdd), returnStatements, skippedEx) = PCMap.find callee functions in
            (* Fetch return address, which we know points to a JUMPDEST (which might be another function start), so the +1 *)
            let pc'' = (OpStack.get_int_at returnPCStackPos stack') + 1 in
            (* Add/remove the stack elements, so to simulate the function execution *)
            let stack'' = OpStack.transform stack' stackRemove stackAdd pc callee in

            find_the_invoke_h stack'' pc'' tmpvis pc

         else if check_throw source (OpStack.get_int_at 0 stack)
                  || (is_jumpi instruction
                     && (List.mem (OpStack.get_int_at 0 stack) tmpvis
                        || not (PCMap.mem (OpStack.get_int_at 0 stack) visited)
                        || not (Edges.connected pc (OpStack.get_int_at 0 stack) edges)
                        )
                     )
         then
         (* We are jumping to an invalid address, so this jump is a throw
            OR
            This is a JUMPI looping
            OR
            The JUMPI destination has not been visited yet, so it can't be the JUMP/JUMPI we followed to reach the alleged return statement
            OR
            There's no edge linking to the JUMPI destination, see rev. 32 commit message for explanation.
         *)
(* TO BE CHECKED: in case we're throwing, wouldn't OpStack.operations calculate PC+1 as next PC? *)
(* is it normal that, when finding a throw, we just keep looking from next PC? please check this *)
            let (stack', pc') = OpStack.operations stack pc instruction operand in
            find_the_invoke_h stack' pc' tmpvis pc

         else
         (* This JUMP has not been assigned yet or has been assigned based on this setAt (previous return from the same function). We take it. *)
            (pc, (OpStack.get_int_at 0 stack))

      else
         let (stack', pc') = OpStack.operations stack pc instruction operand in
         (* Avoid cycles to be considered invokes *)
         let tmpvis = if is_jumpdest instruction then pc :: tmpvis else tmpvis in
         find_the_invoke_h stack' pc' tmpvis lastNode

   in
   find_the_invoke_h OpStack.unknownStack setAt [] setNode
*)

(* The anonymous return usually follows this scheme when returning:
   0x69 ... (not a PUSH anyway)
   0x6a JUMP
   0x6b JUMPDEST
   0x6c STOP
*)
let anonymous_return source pc =
   try
      (get_instruction source (pc * 2)).name = "JUMPDEST" && (get_instruction source ((pc + 1) * 2)).name = "STOP"
   with _ -> false


(*
let rec end_of_code source pc =
   if (substr source (pc * 2) 2) = "00" then
      end_of_code source (pc + 1)
   else
      (substr source (pc * 2) 18) = "a165627a7a72305820" (* + 32 bytes + 00 29 *)
*)
let end_of_code source pc =
   (pc * 2) >= (String.length source - 1)


