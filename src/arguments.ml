open Transaction

(* GENERAL *)

(* Name says it all *)
let from_file filename =
   let inChannel = open_in filename in
   try
      (* read line from inChannel and discard '\n' *)
      let line = input_line inChannel in
      (* close the input channel *)
      close_in inChannel;
      line
   with e -> close_in_noerr inChannel; raise e

(* Name says it all *)
let strip0x fullString =
   if String.sub fullString 0 2 = "0x" then
      String.sub fullString 2 (String.length fullString - 2)
   else
      fullString



(* CONTRACT *)
(* TODO: don't repeat requests if file already exists *)

exception Wrong_bytecode_format

(** Checks that only 8-bit values in hexadecimal representation are fed to the tool *)
let check_format str =
   Str.string_match (Str.regexp_case_fold "^\\(0x\\)?\\(\\([a-f]\\|[0-9]\\)*\\)$") str 0 && String.length str mod 2 = 0

(** Input string can either start with [0x] or not. Removes it in case it's present. *)
let strip_0x str =
   (* Remove metadata *)
   let str = Str.global_replace (Str.regexp_case_fold "\\(00a165627a7a72305820................................................................0029\\)$") "" str in
   if not (check_format str) then
      raise Wrong_bytecode_format
   else
      if String.sub str 0 2 = "0x" then
         (* remove 0x *)
         String.sub str 2 (String.length str - 2)
      else
         str
      
(* Extracts contract's bytecode from JSON file*)
let extract_bytecode jsonString =
   try
      let json = Yojson.Basic.from_string jsonString in
      let open Yojson.Basic.Util in
         let bytecode = json |> member "result" |> to_string |> strip_0x in
         bytecode
   with
      Yojson.Basic.Util.Type_error (s, j) -> print_endline ("Yojson error" ^ "\n" ^ s);
                                             "0x"

(* Saves account json to <address>.tmp file. Requires wget installed. 'curl -s' could be used instead. *)
let get_contract_from_address address =
   let httpString = "http://api.etherscan.io/api?module=proxy&action=eth_getCode&tag=latest&apikey=VUDNRQ5JSSTP5WJ7HXEDIYNMKAEJMA9H5D&address=" in
   let exitcode = Unix.system ("wget -qO- \"" ^ httpString ^ address ^ "\" > " ^ address ^ ".tmp") in
   if exitcode = Unix.WEXITED 0 then
      (address ^ ".tmp") |> from_file |> extract_bytecode
   else
      failwith ("Can't retrieve bytecode from " ^ httpString ^ address)



(* TRANSACTION *)
(* TODO: don't repeat requests if file already exists *)

(* Extracts transaction from JSON file*)
let extract_transaction jsonString =
   let json = Yojson.Basic.from_string jsonString in
   let open Yojson.Basic.Util in
      let result = json |> member "result" in
      {
         from      = result  |> member "from"  |> to_string ;
         origin    = result  |> member "from"  |> to_string ;
         recipient = result  |> member "to"    |> to_string ;
         calldata  = Some (result  |> member "input" |> to_string |> strip0x) ;
         gas       = (let str = (result  |> member "gas"        |> to_string) in if str = "" then None else Some str) ;
         hash      = (let str = (result  |> member "hash"       |> to_string) in if str = "" then None else Some str) ;
         callvalue = (let str = (result  |> member "value"      |> to_string) in if str = "" then None else Some str) ;
         gasprice  = (let str = (result  |> member "gasPrice"   |> to_string) in if str = "" then None else Some str) ;
      }

(* Saves transaction json to <hash>.tmp file. Requires wget installed. 'curl -s' could be used instead. *)
let get_transaction_from_hash hash =
   let httpString = "http://api.etherscan.io/api?module=proxy&action=eth_getTransactionByHash&apikey=VUDNRQ5JSSTP5WJ7HXEDIYNMKAEJMA9H5D&txhash=" in
   let exitcode = Unix.system ("wget -qO- \"" ^ httpString ^ hash ^ "\" > " ^ hash ^ ".tmp") in
   if exitcode = Unix.WEXITED 0 then
      (hash ^ ".tmp") |> from_file |> extract_transaction
   else
      failwith ("Can't retrieve transaction from " ^ httpString ^ hash)



(* COMMAND-LINE ARGS *)

let isContract = ref false
let hash = ref ""
let inputFile = ref ""

(* In case user didn't provide something, we read from stdin *)
let get_hash () =
   if !hash = "" then
      read_line ()
   else
      !hash

(* Either the bytecode or the account address *)
let set_hash h =
   if !hash = "" then
      hash := h
   else
      raise (Arg.Bad "Only one hash allowed")


(* To be a valid hash, filename should match (0x)([a-f]|[0-9])* (20 bytes for contracts, 32 for transactions) *)
let checkFileName fileName =
   let length = if !isContract then 42 else 66 in
   Str.string_match (Str.regexp_case_fold "^\\(0x\\)\\(\\([a-f]\\|[0-9]\\)*\\)$") fileName 0 && String.length fileName = length


(* See
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html
   and pray a god of your choice.
   Or do yourself a favour and read this
   http://scylardor.fr/2013/10/14/ocaml-parsing-a-programs-arguments-with-the-arg-module/ *)
let parse_args () =
   let speclist = Arg.align [ ("--contract" , Arg.Set isContract    , " Retrieve contract bytecode from etherscan instead of transaction");
                              (        "-i" , Arg.Set_string inputFile , " Input file") ]
   in
   let usage_msg = "Usage:\n  evmaddr [--contract] (ADDRESS | -i FILE)\n" in
   Arg.parse speclist set_hash usage_msg;

   if !inputFile <> "" then
      if not (checkFileName !inputFile) then
         raise (Arg.Bad "File name not valid, please use a valid hash starting with 0x")
      else
         if !isContract then
            let transaction = Transaction.noTransaction !inputFile in
            let bytecode = transaction.recipient |> from_file |> extract_bytecode in
            (transaction, bytecode)
         else
            let transaction = !inputFile |> from_file |> extract_transaction in
            let bytecode = get_contract_from_address transaction.recipient in
            (transaction, bytecode)
   else
      if !isContract then
         let transaction = Transaction.noTransaction (get_hash ()) in
         let bytecode = get_contract_from_address transaction.recipient in
         (transaction, bytecode)
      else
         let transaction = get_transaction_from_hash (get_hash ()) in
         let bytecode = get_contract_from_address transaction.recipient in
         (transaction, bytecode)

