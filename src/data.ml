type t =
{
      pc           : int          ;
      stack        : OpStack.t    ;
      memory       : Memory.t     ;
      calldata     : string option;
      calldatasize : int option   ;
      transaction  : Transaction.t;
}

let init (trans : Transaction.t) =
{
      pc           = 0 ;
      stack        = OpStack.empty ;
      memory       = Memory.empty ;
      calldata     = (Transaction.get_calldata trans) ;
      transaction  = trans ;
      calldatasize = match (Transaction.get_calldata trans) with
                       None   -> None 
                     | Some s -> Some (String.length s / 2) ;
}

(* Modifies data according to the instruction to be executed. Memory should be added too *)
let exec instruction operand data =
   (* SHA3: Symbol should have memory content as argument, not operands from stack *)
   let (stack', pc') = OpStack.exec instruction operand data.pc data.stack in
   { data with stack = stack' ; pc = pc' }

(* Proxying transaction *)
let get_origin d =
   Transaction.get_origin d.transaction

let get_recipient d =
   Transaction.get_recipient d.transaction

let get_from d =
   Transaction.get_from d.transaction

let get_calldata offset length d =
   match Transaction.get_calldata d.transaction with
     None   -> None
   | Some s -> Some (Instruction.get_source offset length s)

let get_callvalue d =
   Transaction.get_callvalue d.transaction

let get_gasprice d =
   Transaction.get_gasprice d.transaction
   