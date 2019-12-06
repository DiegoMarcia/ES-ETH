type t = {
      name   : string ;
      reads  : int ;
      writes : bool ;
   }

let stoparith = [ { name = "STOP";       reads = 0; writes = false; };
                  { name = "ADD";        reads = 2; writes = true;  };
                  { name = "MUL";        reads = 2; writes = true;  };
                  { name = "SUB";        reads = 2; writes = true;  };
                  { name = "DIV";        reads = 2; writes = true;  };
                  { name = "SDIV";       reads = 2; writes = true;  };
                  { name = "MOD";        reads = 2; writes = true;  };
                  { name = "SMOD";       reads = 2; writes = true;  };
                  { name = "ADDMOD";     reads = 3; writes = true;  };
                  { name = "MULMOD";     reads = 3; writes = true;  };
                  { name = "EXP";        reads = 2; writes = true;  };
                  { name = "SIGNEXTEND"; reads = 2; writes = true;  } ]

let logicbyte = [ { name = "LT";     reads = 2; writes = true; };
                  { name = "GT";     reads = 2; writes = true; };
                  { name = "SLT";    reads = 2; writes = true; };
                  { name = "SGT";    reads = 2; writes = true; };
                  { name = "EQ";     reads = 2; writes = true; };
                  { name = "ISZERO"; reads = 1; writes = true; };
                  { name = "AND";    reads = 2; writes = true; };
                  { name = "OR";     reads = 2; writes = true; };
                  { name = "XOR";    reads = 2; writes = true; };
                  { name = "NOT";    reads = 1; writes = true; };
                  { name = "BYTE";   reads = 2; writes = true; } ]

let envirinfo = [ { name = "ADDRESS";      reads = 0; writes = true;  };
                  { name = "BALANCE";      reads = 1; writes = true;  };
                  { name = "ORIGIN";       reads = 0; writes = true;  };
                  { name = "CALLER";       reads = 0; writes = true;  };
                  { name = "CALLVALUE";    reads = 0; writes = true;  };
                  { name = "CALLDATALOAD"; reads = 1; writes = true;  };
                  { name = "CALLDATASIZE"; reads = 0; writes = true;  };
                  { name = "CALLDATACOPY"; reads = 3; writes = false; };
                  { name = "CODESIZE";     reads = 0; writes = true;  };
                  { name = "CODECOPY";     reads = 3; writes = false; };
                  { name = "GASPRICE";     reads = 0; writes = true;  };
                  { name = "EXTCODESIZE";  reads = 1; writes = true;  };
                  { name = "EXTCODECOPY";  reads = 4; writes = false; } ]

let blockinfo = [ { name = "BLOCKHASH";  reads = 1; writes = true; };
                  { name = "COINBASE";   reads = 0; writes = true; };
                  { name = "TIMESTAMP";  reads = 0; writes = true; };
                  { name = "NUMBER";     reads = 0; writes = true; };
                  { name = "DIFFICULTY"; reads = 0; writes = true; };
                  { name = "GASLIMIT";   reads = 0; writes = true; } ]

let storeflow = [ { name = "POP";      reads = 1; writes = false; };
                  { name = "MLOAD";    reads = 1; writes = true;  };
                  { name = "MSTORE";   reads = 2; writes = false; };
                  { name = "MSTORES";  reads = 2; writes = false; };
                  { name = "SLOAD";    reads = 1; writes = true;  };
                  { name = "SSTORE";   reads = 2; writes = false; };
                  { name = "JUMP";     reads = 1; writes = false; };
                  { name = "JUMPI";    reads = 2; writes = false; };
                  { name = "PC";       reads = 0; writes = true;  };
                  { name = "MSIZE";    reads = 0; writes = true;  };
                  { name = "GAS";      reads = 0; writes = true;  };
                  { name = "JUMPDEST"; reads = 0; writes = false; } ]

let crcallret = [ { name = "CREATE";       reads = 3; writes = true;  };
                  { name = "CALL";         reads = 7; writes = true;  };
                  { name = "CALLCODE";     reads = 7; writes = true;  };
                  { name = "RETURN";       reads = 2; writes = false; };
                  { name = "DELEGATECALL"; reads = 7; writes = true;  } ]

(* [string_of_instruction ~sep:"-" { name = "name"; reads = 0; writes = true; };] *)
let string_of_instruction ?(sep=" ") instr = instr.name ^ sep ^ string_of_int instr.reads ^ sep ^ string_of_bool instr.writes

(* Provides safer substring functionality than String.sub (doesn't fail) *)
let substr s start sublength =
   let maxlength = String.length s - start in
   let sublength = min sublength maxlength in
   if start >= 0 && sublength >= 0 then String.sub s start sublength else ""

(* Extracts a portion of source *)
let get_source position length source =
   let simple = substr source (position * 2) (length * 2) in
   (* Pad to required size *)
   let padder =
      if String.length simple < length then
         String.make (length - String.length simple) '0'
      else
         ""
   in
   simple ^ padder

(* Name says it all *)
let get_source_size source =
   String.length source / 2


let is_push instr =
   substr instr.name 0 4 = "PUSH"

let is_dup instr =
   substr instr.name 0 3 = "DUP"

let is_swap instr =
   substr instr.name 0 4 = "SWAP"

let is_jumpdest instr =
   instr.name = "JUMPDEST"

let is_jumpi instr =
   instr.name = "JUMPI"

let is_jump instr =
   instr.name = "JUMP"

let is_jump_fresh jumpPC pushPC value =
   pushPC + (String.length value / 2) = jumpPC

let is_terminal instr =
   instr.name = "RETURN" || instr.name = "STOP" || instr.name = "INVALID" || instr.name = "SUICIDE" || instr.name = "REVERT"

(* Push instructions are followed by operand *)
let next_pc instr op pc =
   if is_push instr then (pc + String.length op / 2) else (pc + 1)


