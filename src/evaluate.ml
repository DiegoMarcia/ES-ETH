open Instruction
open OpStack
open Data
open Big_int

type t = Eval of Big_int.big_int | NoEval of string list

let rec all_Eval l =
   if l = [] then
      true
   else
      match List.hd l with
        Eval bi  -> all_Eval (List.tl l)
      | NoEval _ -> false

(* This will always result in a positive number *)
let get_big_int s = big_int_of_string s

(* Getting a big_int from a string always results in a positive number, so a function is needed to correctly interpret it as negative *)
let get_signed_int num =
   (* If 256th bit (sign) is 1, num is a negative in two's complement *)
   if eq_big_int zero_big_int (extract_big_int num 255 1) then
      num
   else
      (* 256 1s *)
      let mask = add_int_big_int (-1) (power_int_positive_int 2 256) in
      (* the two's complement of a number in two's complement is that number's magnitude *)
      let magnitude = add_int_big_int 1 (xor_big_int num mask) in
      (* negative big_int *)
      minus_big_int magnitude



let get_low_4 (num : big_int) =
   (* 4 1s *)
   let mask = big_int_of_string "0b1111" in
   let extracted = and_big_int num mask in
   int_of_big_int extracted


let hex_of_big_int num =
   let rec helper num iter =
      let conv = Printf.sprintf "%x" (get_low_4 num) in
      let num = div_big_int num (big_int_of_int 16) in
      if iter = 1 || eq_big_int num zero_big_int then
         conv
      else
         (helper num (iter - 1)) ^ conv
   in
   helper num 64


let calc f a b =
   let res = f a b in
   (* Arithmetics modulo 2^256, 256th is sign *)
   extract_big_int res 0 256



let eval_callvalue d =
   match Data.get_callvalue d with
     None   -> NoEval ["CALLVALUE"]
   | Some s -> Eval (get_big_int s)

let eval_gasprice d =
   match Data.get_gasprice d with
     None   -> NoEval ["GASPRICE"]
   | Some s -> Eval (get_big_int s)

let eval_calldataload cd offset =
   match cd with
     None   -> NoEval ["CALLDATALOAD"]
   | Some s ->
               let offset = int_of_big_int offset in
               (* get as much calldata as possible *)
               let extracted = Instruction.substr s (offset * 2) 64 in
               (* might be empty *)
               let padder = String.make (64 - String.length extracted) '0' in
               (* format with right padding *)
               let extracted = "0x" ^ extracted ^ padder in
               (* check if data contains Top *)
               if not (String.contains extracted 'T') then
                  Eval (get_big_int extracted)
               else
                  NoEval ["CALLDATALOAD"]

let eval_calldatasize cds =
   match cds with
     None   -> NoEval ["CALLDATASIZE"]
   | Some i -> Eval (big_int_of_int i)



let implemented name =
   let extraInstructions = ["ADDRESS" ; "ORIGIN" ; "CALLER" ; "CALLVALUE" ; "CALLDATALOAD" ; "CALLDATASIZE" ; "GASPRICE" ; "MSIZE"] in
   let check_name (i : Instruction.t) = i.name = name in
   List.exists check_name Instruction.stoparith || List.exists check_name Instruction.logicbyte || List.mem name extraInstructions


(* flattens the dependency list *)
let gather_dependencies oldDependencies evaluated =
   match evaluated with
     NoEval names -> names @ oldDependencies
   | _ -> oldDependencies


let rec evaluate data expression =
   match fst expression with
     Exp (name, args) -> let evaluatedArgs = List.map (evaluate data) args in
                         (** TODO: dependsOn is a list (possibly with duplicates), might make it a set (proper, or as a list). *)
                         let dependsOn = List.fold_left gather_dependencies [] evaluatedArgs in
                         if not (implemented name) then
                           NoEval (name :: dependsOn)
                         else
                            if dependsOn <> [] then
                               NoEval dependsOn
                            else
                            (* Instruction and arguments are implemented *)
                               do_eval data name evaluatedArgs
   | Val v            -> Eval (get_big_int v)
   | Unknown          -> NoEval ["UNKNOWN"]


and do_eval data name evaluatedArgs =
   let evalArgs = List.map (fun x -> match x with Eval v -> v | _ -> zero_big_int) evaluatedArgs in
      match name with
        "ADDRESS"    -> Eval (get_big_int (get_recipient data))
      | "ORIGIN"     -> Eval (get_big_int (get_origin data))
      | "CALLER"     -> Eval (get_big_int (get_from data))
      | "CALLVALUE"  -> eval_callvalue data
      | "CALLDATALOAD"-> eval_calldataload data.calldata (List.nth evalArgs 0)
      | "CALLDATASIZE"-> eval_calldatasize data.calldatasize
      | "GASPRICE"   -> eval_gasprice data
      | "MSIZE"      -> Eval (big_int_of_int (Memory.memory_size data.memory))
      | "ADD"        -> Eval (calc (add_big_int) (List.nth evalArgs 0) (List.nth evalArgs 1))
      | "MUL"        -> Eval (calc (mult_big_int) (List.nth evalArgs 0) (List.nth evalArgs 1))
      | "SUB"        -> Eval (calc (sub_big_int) (List.nth evalArgs 0) (List.nth evalArgs 1))

       (* floor of 8/(-3) should be -3, not -2 (which is the integer part). Check how actual EVM implements floor *)
      | "DIV"        -> if sign_big_int (List.nth evalArgs 1) = 0 then
                           Eval (zero_big_int)
                        else
                           Eval (calc (div_big_int) (List.nth evalArgs 0) (List.nth evalArgs 1))

      | "SDIV"       -> let signed0 = get_signed_int (List.nth evalArgs 0) in
                        let signed1 = get_signed_int (List.nth evalArgs 1) in
                        if eq_big_int signed1 zero_big_int then
                           Eval (signed1)
                        else
                           let minus2pow255 = minus_big_int (power_int_positive_int 2 255) in
                           let minus1 = big_int_of_int (-1) in
                           if eq_big_int signed0 minus2pow255 && eq_big_int signed1 minus1 then
                              Eval (signed0)
                           else
                              Eval (calc (div_big_int) signed0 signed1)

      | "MOD"        -> if sign_big_int (List.nth evalArgs 1) = 0 then
                           Eval (zero_big_int)
                        else
                           Eval (calc (mod_big_int) (List.nth evalArgs 0) (List.nth evalArgs 1))

      | "SMOD"       -> let signed0 = get_signed_int (List.nth evalArgs 0) in
                        let signed1 = get_signed_int (List.nth evalArgs 1) in
                        if eq_big_int signed1 zero_big_int then
                           Eval (signed1)
                        else
                           let absmod = calc (mod_big_int) (abs_big_int signed0) (abs_big_int signed1) in
                           if sign_big_int signed0 < 0 then
                              Eval (extract_big_int (minus_big_int absmod) 0 256) (* Arithmetics modulo 2^256, 256th is sign *)
                           else
                              Eval (absmod)

      | "ADDMOD"     -> if eq_big_int (List.nth evalArgs 2) zero_big_int then
                           Eval (List.nth evalArgs 2)
                        else
                           (* Not subject to 2^256 modulo *)
                           let intermediate = add_big_int (List.nth evalArgs 0) (List.nth evalArgs 1) in 
                           (* Subject to 2^256 modulo *)
                           Eval (calc (mod_big_int) intermediate (List.nth evalArgs 2))

      | "MULMOD"     -> if eq_big_int (List.nth evalArgs 2) zero_big_int then
                           Eval (List.nth evalArgs 2)
                        else
                           (* Not subject to 2^256 modulo *)
                           let intermediate = mult_big_int (List.nth evalArgs 0) (List.nth evalArgs 1) in 
                           (* Subject to 2^256 modulo *)
                           Eval (calc (mod_big_int) intermediate (List.nth evalArgs 2))

      | "EXP"        -> Eval (calc (power_big_int_positive_big_int) (List.nth evalArgs 0) (List.nth evalArgs 1))

      | "SIGNEXTEND" -> let t = calc (mult_big_int) (big_int_of_int 8) (List.nth evalArgs 0) in
                        let t = calc (add_int_big_int) 7 t in
                        let t = int_of_big_int t in
                        let number = extract_big_int (List.nth evalArgs 1) 0 t in
                        let sign = extract_big_int (List.nth evalArgs 1) t 1 in
                        if eq_big_int sign zero_big_int then
                           Eval (number)
                        else
                           let i_mask = sub_big_int (power_int_positive_int 2 256) unit_big_int in
                           let s_mask = shift_left_big_int i_mask t in
                           let new_number = or_big_int number s_mask in
                           Eval (extract_big_int new_number 0 256)

      | "LT"         -> if lt_big_int (List.nth evalArgs 0) (List.nth evalArgs 1) then
                           Eval (unit_big_int)
                        else
                           Eval (zero_big_int)

      | "GT"         -> if gt_big_int (List.nth evalArgs 0) (List.nth evalArgs 1) then
                           Eval (unit_big_int)
                        else
                           Eval (zero_big_int)

      | "SLT"        -> let signed0 = get_signed_int (List.nth evalArgs 0) in
                        let signed1 = get_signed_int (List.nth evalArgs 1) in
                        if lt_big_int signed0 signed1 then
                           Eval (unit_big_int)
                        else
                           Eval (zero_big_int)

      | "SGT"        -> let signed0 = get_signed_int (List.nth evalArgs 0) in
                        let signed1 = get_signed_int (List.nth evalArgs 1) in
                        if gt_big_int signed0 signed1 then
                           Eval (unit_big_int)
                        else
                           Eval (zero_big_int)

      | "EQ"         -> if eq_big_int (List.nth evalArgs 0) (List.nth evalArgs 1) then
                           Eval (unit_big_int)
                        else
                           Eval (zero_big_int)

      | "ISZERO"     -> if eq_big_int (List.nth evalArgs 0) zero_big_int then
                           Eval (unit_big_int)
                        else
                           Eval (zero_big_int)

      | "AND"        -> Eval (calc (and_big_int) (List.nth evalArgs 0) (List.nth evalArgs 1))
      | "OR"         -> Eval (calc (or_big_int) (List.nth evalArgs 0) (List.nth evalArgs 1))
      | "XOR"        -> Eval (calc (xor_big_int) (List.nth evalArgs 0) (List.nth evalArgs 1))

      | "NOT"        -> let mask = add_int_big_int (-1) (power_int_positive_int 2 256) in
                        Eval (calc (xor_big_int) (List.nth evalArgs 0) mask)

      | "BYTE"       -> if lt_big_int (List.nth evalArgs 0) (big_int_of_int 32) then
                           let b = int_of_big_int (List.nth evalArgs 0) in
                           Eval (extract_big_int (List.nth evalArgs 1) (b * 8) 8)
                        else
                           Eval (zero_big_int)

      | _ -> NoEval [name]

