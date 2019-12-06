open Message
open Data
open Transaction

module LocationsSet = Set.Make( 
   struct
      (* Representing a labeled outbound edge *)
      type t = string
      let compare = String.compare
   end
)

type record = { contract : string ; locations : LocationsSet.t }

type t = {
      evironmentRead  : record list ; (* Unused *)
      storageRead     : record list ;
      storageWritten  : record list ;
      balancesWritten : record list ;
}

let rec add_record contract location records =
   match records with
     []                              -> [{ contract = contract ; locations = LocationsSet.add location LocationsSet.empty }]
   | hd :: tl when hd.contract = contract -> { hd with locations = (LocationsSet.add location hd.locations) } :: tl
   | hd :: tl                        -> hd :: (add_record contract location tl)


let noVariables data =
   let def = { evironmentRead = [] ; storageRead = [] ; storageWritten = [] ; balancesWritten = [] ; } in
   { def with balancesWritten = add_record (Data.get_recipient data) "<BALANCE>" def.balancesWritten ; }



let printer op record =
   let contract = if record.contract = "00" then "<TOP>" else record.contract in
   LocationsSet.iter (fun l -> print_endline ("(" ^ contract ^ ", " ^ l ^ ", " ^ op ^ ")")) record.locations

let print (variables : t) =
   List.iter (printer "r") (List.rev variables.storageRead);
   List.iter (printer "w") (List.rev variables.storageWritten);
   List.iter (printer "w") (List.rev variables.balancesWritten)


let debug_message pc operand eval msgs operation =
   let msgs = Message.add_message ((Printf.sprintf "\n\n%0#x: %s " pc operation) ^ (OpStack.string_of_stack_element operand)) Debug msgs in
   let msgs = 
      match eval with
           Evaluate.Eval bi     -> Message.add_message (Printf.sprintf "%0#x: %s %s" pc operation (Evaluate.hex_of_big_int bi)) Debug msgs

         | Evaluate.NoEval deps -> let title = (Printf.sprintf "%0#x: %s computed as a function of instruction(s):" pc operation) in
                                   let m = List.fold_left (fun a d -> a ^ "\n" ^ d) title deps in
                                   Message.add_message m Debug msgs
   in
   Message.add_message "\n\n" Debug msgs


(* SLOAD / SSTORE *)
let storage data name operand old msgs =
   (* Evaluate operand *)
   let eval = Evaluate.evaluate data operand in
   (* Known or TOP *)
   let location =
      match eval with
        Evaluate.Eval bi  -> "0x" ^ Evaluate.hex_of_big_int bi
      | Evaluate.NoEval _ -> "<TOP>"
   in
   if name = "SLOAD" then
      (* DEBUG *) let msgs' = debug_message data.pc operand eval msgs "Reading at position" in
      (* New record *)
      let stores = { old with storageRead = add_record (Data.get_recipient data) location old.storageRead } in
      (stores, msgs')
   else
      (* DEBUG *) let msgs' = debug_message data.pc operand eval msgs "Writing to position" in
      (* New record *)
      let stores = { old with storageWritten = add_record (Data.get_recipient data) location old.storageWritten } in
      (stores, msgs')

(* CALL *)
let receivers data evaluatedReceiver receiverExpression evaluatedCallValue balances msgs =
   (* DEBUG *) let msgs' = debug_message data.pc receiverExpression evaluatedReceiver msgs "Call to address" in
   (* Is this CALL moving Ether? *)
   let balances' =
      match evaluatedCallValue with
        Evaluate.Eval cv when Big_int.eq_big_int cv Big_int.zero_big_int -> balances
      | _                                                                -> { balances with balancesWritten =
                                                                               add_record (Data.get_recipient data) "<BALANCE>" balances.balancesWritten ;
                                                                            }
   in
   (* Known or TOP *)
   let receiver =
      match evaluatedReceiver with
        Evaluate.Eval bi  -> "0x" ^ Evaluate.hex_of_big_int bi
      | Evaluate.NoEval _ -> "<TOP>"
   in
   (* New record *)
   let balances'' = { balances' with balancesWritten = add_record receiver "<BALANCE>" balances'.balancesWritten ; } in
   (balances'', msgs')


