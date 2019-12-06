(**
   This module parses the command-line arguments fed by the user. Transaction or
   account hash is read either from the command-line, stdin or a file.

    Supported arguments:
    - [evm2cfg TRANSACTION-HASH]
    - [evm2cfg -i TRANSACTION-HASH]
    - [evm2cfg --contract CONTRACT-HASH]
    - [evm2cfg --contract -i CONTRACT-HASH] *)

(** Does the arguments parsing.
    @return the pair [(transaction, bytecode)]
    @raise  Arg.Bad if user didn't provide a valid hash.
    {b See} {{: http://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html }  Arg.Bad } *)
val parse_args : unit -> Transaction.t * string

(** Retrieves an account's bytecode from its address.
    @param  address  Contract address
    @return the account's bytecode
    @raise  Arg.Bad if user didn't provide a valid hash.
    {b See} {{: http://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html }  Arg.Bad } *)
val get_contract_from_address : string -> string