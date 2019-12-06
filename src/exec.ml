open Instruction
open Tokenizer
open DS
open OpStack
open Data

(**
   Follows recursively the execution flow one instruction at a time, dispatcing joint nodes to appropriate functions.

   Function arguments are, in order:
   
   @param source         Contract code
   @param q              Queue of all paths to be explored: list of ()
   @param msgs           List of debug/error messages to be printed before exiting
*)
let rec exec source data variables flow q msgs =
   let instruction, operand = Tokenizer.get_token source data.pc in

   if is_terminal instruction then
      kill_path source variables flow q msgs

   else if is_jumpdest instruction then
      (Flow.get_jumpDestFun flow) source data.pc data variables flow q msgs

   else if is_jumpi instruction then
      (Flow.get_jumpiFun flow) source data variables flow q msgs

   else if is_jump instruction then
      (Flow.get_jumpFun flow) source data variables flow q msgs


   else if instruction.name = "CALLDATACOPY" then
      let (arguments, rest) = OpStack.cut 3 data.stack in
      (* Base address of memory to write to, offset of data in the calldata, length of data to be copied *)
      let baseMem = Evaluate.evaluate data (List.nth arguments 0) in
      let dataOff = Evaluate.evaluate data (List.nth arguments 1) in
      let dataLen = Evaluate.evaluate data (List.nth arguments 2) in

      let mem' =
         match (baseMem, dataOff, dataLen) with
           Evaluate.Eval bm, Evaluate.Eval df, Evaluate.Eval dl  -> let dataOff = (Big_int.int_of_big_int df) in
                                                                    let dataLen = (Big_int.int_of_big_int dl) in
                                                                    let dataToCopy = Data.get_calldata dataOff dataLen data in
                                                                    Memory.store dataLen (Big_int.int_of_big_int bm) dataToCopy data.memory
                                                                    (* In case we don't know where to read from, write Top to memory *)
         | Evaluate.Eval bm, Evaluate.NoEval _, Evaluate.Eval dl -> Memory.store (Big_int.int_of_big_int dl) (Big_int.int_of_big_int bm) None data.memory

         | _                                                     -> data.memory
      in
      let data' = { data with stack = rest ; pc = data.pc + 1 ; memory = mem' } in
      exec source data' variables flow q msgs


   else if instruction.name = "CODECOPY" then
      let (arguments, rest) = OpStack.cut 3 data.stack in
      (* Base address of memory to write to, offset of data in the bytecode, length of data to be copied *)
      let baseMem = Evaluate.evaluate data (List.nth arguments 0) in
      let dataPC  = Evaluate.evaluate data (List.nth arguments 1) in
      let dataLen = Evaluate.evaluate data (List.nth arguments 2) in

      let mem' =
         match (baseMem, dataPC, dataLen) with
           Evaluate.Eval bm, Evaluate.Eval dp, Evaluate.Eval dl  -> let dataPC = (Big_int.int_of_big_int dp) in
                                                                    let dataLen = (Big_int.int_of_big_int dl) in
                                                                    let dataToCopy = Instruction.get_source dataPC dataLen source in
                                                                    Memory.store dataLen (Big_int.int_of_big_int bm) (Some dataToCopy) data.memory
                                                                    (* In case we don't know where to read from, write Top to memory *)
         | Evaluate.Eval bm, Evaluate.NoEval _, Evaluate.Eval dl -> Memory.store (Big_int.int_of_big_int dl) (Big_int.int_of_big_int bm) None data.memory

         | _                                                     -> data.memory
      in
      let data' = { data with stack = rest ; pc = data.pc + 1 ; memory = mem' } in
      exec source data' variables flow q msgs


   else if instruction.name = "CODESIZE" then
      (* Don't wait for eval, so we save a function argument *)
      let codesize = Instruction.get_source_size source in
      let data' = { data with stack = (Val (Printf.sprintf "%0#x: " codesize), data.pc) :: data.stack ; pc = data.pc + 1 } in
      exec source data' variables flow q msgs


   else if instruction.name = "MSTORE" || instruction.name = "MSTORE8" then
      let len = if instruction.name = "MSTORE" then 32 else 1 in
      let (arguments, rest) = OpStack.cut 2 data.stack in
      (* Where to write, what to write *)
      let address = Evaluate.evaluate data (List.hd arguments) in
      let bytes = Evaluate.evaluate data (List.nth arguments 1) in

      let mem' =
         match (address, bytes) with
           Evaluate.NoEval _, _                ->  data.memory
         | Evaluate.Eval bi, Evaluate.NoEval _ ->  Memory.store len (Big_int.int_of_big_int bi) None data.memory
         | Evaluate.Eval b1, Evaluate.Eval b2  ->  Memory.store len (Big_int.int_of_big_int b1) (Some (Evaluate.hex_of_big_int b2)) data.memory
      in
      let data' = { data with stack = rest ; pc = data.pc + 1 ; memory = mem' } in
      exec source data' variables flow q msgs


   else if instruction.name = "MLOAD" then
      let address = Evaluate.evaluate data (List.hd data.stack) in
      let data' =
         match address with
           Evaluate.Eval bi   -> let extracted = "0x" ^ (Memory.extract_string 32 (Big_int.int_of_big_int bi) data.memory) in
                                 if not (String.contains extracted 'T') then
                                    (* read evaluated data, put it on stack *)
                                    { data with stack = (Val extracted, data.pc) :: (List.tl data.stack) ; pc = data.pc + 1 }
                                 else
                                    (* Put symbol on stack *)
                                    Data.exec instruction operand data

         | Evaluate.NoEval _  -> Data.exec instruction operand data
      in
      exec source data' variables flow q msgs


   else if instruction.name = "SLOAD" || instruction.name = "SSTORE" then
      let location = List.hd (OpStack.get_operands data.stack instruction) in
      let (variables', msgs') = Variables.storage data instruction.name location variables msgs in
      let data' = Data.exec instruction operand data in
      exec source data' variables' flow q msgs'


   else if instruction.name = "CALL" then
      (* the operand order is: gas, to, value, in offset, in size, out offset, out size *)
      let receiver   = Evaluate.evaluate data (List.nth data.stack 1) in
      let callValue  = Evaluate.evaluate data (List.nth data.stack 2) in
      let inputPos   = Evaluate.evaluate data (List.nth data.stack 3) in
      let inputSize  = Evaluate.evaluate data (List.nth data.stack 4) in
      let outputPos  = Evaluate.evaluate data (List.nth data.stack 5) in
      let outputSize = Evaluate.evaluate data (List.nth data.stack 6) in
      (* Mark state change for CALL *)
      let (variables', msgs') = Variables.receivers data receiver (List.nth data.stack 1) callValue variables msgs in
      let data' = Data.exec instruction operand data in

      if Evaluate.all_Eval [ outputPos ; outputSize ] then
         (* The called contract might have more than one RETURN, suppose it always returns TOP *)
         let mem' =
            match (outputPos, outputSize) with
              Evaluate.Eval pos, Evaluate.Eval size -> Memory.store (Big_int.int_of_big_int size) (Big_int.int_of_big_int pos) None data.memory
            | _ , _                                 -> data'.memory (* But we know they've been evaluated *)
         in
         let data'' = { data' with memory = mem'} in

         let address =
            match receiver with
              Evaluate.Eval bi   -> "0x" ^ Evaluate.hex_of_big_int bi
            | Evaluate.NoEval _  -> "0x00" (* But we know it's been evaluated *)
         in
         let destSource = if address = "0x00" then "0x" else Arguments.get_contract_from_address address in
         if destSource = "0x" then
            (* Ignore and go on *)
            exec source data'' variables' flow q msgs'
         else
            (* Strip 0x *)
            let destSource = String.sub destSource 2 (String.length destSource - 2) in
            let destCallData =
               match (inputPos, inputSize) with
                 Evaluate.Eval pos, Evaluate.Eval size -> Some (Memory.extract_string (Big_int.int_of_big_int size) (Big_int.int_of_big_int pos) data.memory)
               | _ , _                                 -> None
            in
            let destCallValue =
               match callValue with
                 Evaluate.Eval bi   -> Some (Evaluate.hex_of_big_int bi)
               | Evaluate.NoEval _  -> None
            in
            let destTransaction = Transaction.internal (Data.get_origin data) (Data.get_recipient data) None destCallData address destCallValue (Data.get_gasprice data) in
            let freshFlow = (Flow.init exec_jumpdest exec_jump exec_jumpi test_jumpdest exec_jump_in_loops queue_jumpi) in
            let (_, msgs'', variables'') = exec destSource (Data.init destTransaction) variables' freshFlow q msgs' in
            exec source data'' variables'' flow q msgs''
      else
         exec source data' variables' flow q msgs'


   else if instruction.name = "SHA3" then
(* TODO: Symbol should have memory content as argument *)
      let data' = Data.exec instruction operand data in
      exec source data' variables flow q msgs

   else
      let data' = Data.exec instruction operand data in
      exec source data' variables flow q msgs



and exec_jumpdest source pc data variables flow q msgs =
   let flow' = Flow.add_visited pc flow in
   exec source { data with pc = pc + 1 } variables flow' q msgs



(*  *)
and queue_jumpi source data variables flow q msgs =
   match OpStack.cut 2 data.stack with
     (((Val v, set) :: _), rest) -> let qdata = { data with stack = ((Val v, set) :: rest) } in
                                    let q' = (qdata, Flow.get_visited flow) :: q in
                                    let data' = { data with stack = rest ; pc = data.pc + 1 } in
                                    exec source data' variables flow q' msgs
| (otherExpression, rest)        -> failwith ((Printf.sprintf "%0#x: " data.pc) ^ "cannot jump to " ^ string_of_stack_element (List.hd otherExpression))


(* Invoked by [exec] when instruction JUMPI is found.
   Conditional jumps are put to the execution queue and flow proceeds to next PC. We don't jump to computed addresses. *)
and exec_jumpi source data variables flow q msgs =
   match OpStack.cut 2 data.stack with
     (((Val v, set) :: guard :: _), rest) -> (
                                             match Evaluate.evaluate data guard with
                                               Evaluate.Eval bi  ->  if Big_int.eq_big_int bi Big_int.zero_big_int then
                                                                        let data' = { data with stack = rest ; pc = data.pc + 1 } in
                                                                        exec source data' variables flow q msgs
                                                                     else
                                                                        let data' = { data with stack = ((Val v, set) :: rest) } in
                                                                        (Flow.get_jumpFun flow) source data' variables flow q msgs
                                             | Evaluate.NoEval _ ->  queue_jumpi source data variables flow q msgs
                                             )
   | (otherExpression, rest)             -> failwith ((Printf.sprintf "%0#x: " data.pc) ^ "cannot jump to " ^ string_of_stack_element (List.hd otherExpression))



(** Invoked by [exec] when instruction JUMP is found, or by [kill_path] when resuming a path.
   Jumping to anything but a JUMPDEST stops the execution and other cases are treated by [jump_cases]. We don't jump to computed addresses. *)
and exec_jump source data variables flow q msgs =
   match cut 1 data.stack with
     [Val v, set], rest -> let newPC = int_of_string v in
                           (* Check if jumping to an invalid address (throw) *)
                           if Tokenizer.check_throw source newPC then
                              kill_path source variables flow q msgs
                           else
                           (* The jump is to a JUMPDEST, so it is legal *)
                              if (Flow.is_visited newPC flow) && not (Flow.is_safe newPC flow) then
                              (* Jumping to an already visited PC *)

                                 if Flow.is_suspect newPC flow then
                                 (* It's a loop. *)
                                    kill_path source variables flow q msgs
                                 else
                                    (* Suspect a loop. *)
                                    let flow' = Flow.add_suspect newPC flow in
                                    let loopflow = Flow.set_looping flow' in
                                    (* For logging purposes *)
                                    let msgsForLoopTest = Message.new_level "detecting loop" msgs in
                                    (* Test if really a loop *)
                                    let (flowFromLoopTest, msgsFromLoopTest, variblesFromLoopTest) =
                                                exec source { data with stack = rest ; pc = newPC + 1 } variables loopflow [] msgsForLoopTest in
                                    (* Restore normal execution *)
                                    let flow' = Flow.set_normal flowFromLoopTest in
                                    (* Back to previous logging format *)
                                    let msgs' = Message.level_back msgsFromLoopTest in
                                    (* Keeps track of whether or not it was a loop *)
                                    let flow' = if Flow.get_loopFound flow' then flow' else Flow.add_safeJump newPC data.pc flow' in
                                    (* Explore the unexplored *)
                                    let unexplored = Flow.get_unexplored flow' in
                                    if unexplored = [] then
                                       kill_path source variblesFromLoopTest flow' q msgs'
                                    else
                                       (* For logging purposes *)
                                       let msgsForExplore = Message.new_level "" msgs' in
                                       let (flow'', msgsFromExplore, variables') = kill_path source variblesFromLoopTest flow' unexplored msgsForExplore in
                                       (* Back to previous logging format *)
                                       let msgs'' = Message.level_back msgsFromExplore in
                                       (* unexplored paths found by ExecLoops have been explored at this point, so this one can be killed *)
                                       kill_path source variables' flow'' q msgs''

                              else
                              (* Destination is yet to be visited *)
                                 exec source { data with stack = rest ; pc = newPC } variables flow q msgs

   | otherEx, rest      -> failwith ((Printf.sprintf "%0#x: " data.pc) ^ "cannot jump to " ^ string_of_stack_element (List.hd otherEx))



and exec_jump_in_loops source data variables flow q msgs =
   match cut 1 data.stack with
     [Val v, set], rest -> let newPC = int_of_string v in
                           if Tokenizer.check_throw source newPC then
                           (* We are jumping to an invalid address, so this jump is a throw *)
                              kill_path source variables flow q msgs
                           else
                           (* The jump is to a JUMPDEST, so it is legal *)
                              test_jumpdest source newPC data variables flow q msgs
   | otherEx, rest      -> failwith ((Printf.sprintf "detecting loop: %0#x: cannot jump to %s" data.pc (string_of_stack_element (List.hd otherEx))))


and test_jumpdest source dest data variables flow q msgs =
   let (loop, newPath, msgs') =
      if not (Flow.is_visited dest flow) then
         let stack' = if data.pc <> dest then data.stack else (Val (Printf.sprintf "%0#x" dest), dest) :: data.stack in
         let data' = { data with stack = stack' } in
         (* Are we really interested in gathering new paths here? Could we just check the absence of loops and then JUMP normally?
            Same thought applies to state changes *)
         let newPath = [(data', Flow.get_visited flow)] in
         (false, newPath, msgs)
      else if (Flow.is_suspect dest flow) && not (Flow.is_safe dest flow) then
         (true, [], msgs)
      else
         (false, [], msgs)
   in
   let found = Flow.get_loopFound flow || loop in
   let flow' = Flow.update_loopFound found flow in
   (* Check if the search is over *)
   if (not loop) && (newPath = []) then
      let stack' = if data.pc <> dest then (List.tl data.stack) else data.stack in
      let data' = { data with stack = stack' ; pc = if data.pc <> dest then dest else data.pc + 1} in
      exec source data' variables flow' q msgs'
   else
   (* either a loop, or a never visited location *)
      let flow'' = Flow.add_unexplored newPath flow' in
      kill_path source variables flow'' q msgs'



(** Invoked when a path reaches an end, be it a RETURN / STOP instruction, a throw statement or loop detection.
   If paths queue is empty, returns the error/debug messages. Otherwise, continues on next path. *)
and kill_path source variables flow q msgs =
   match q with
   | []                       ->  (flow, msgs, variables)
   | (newData, newVisited)::t ->  (Flow.get_jumpFun flow) source newData variables (Flow.update_visited newVisited flow) t msgs


(** Public launcher *)
let pub_exec transaction bytecode =
   let data = (Data.init transaction) in
   let flow = (Flow.init exec_jumpdest exec_jump exec_jumpi test_jumpdest exec_jump_in_loops queue_jumpi) in
   exec bytecode data (Variables.noVariables data) flow [] Message.empty



