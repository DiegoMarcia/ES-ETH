module IntOrder = struct
  type t = int
  let compare = Pervasives.compare
end

module IntMap = Map.Make( IntOrder )

module T = struct
  include IntMap

   (* find_opt has been introduced in 4.05 while this tool is being developed under 4.04 *)
   let find_opt (k : IntMap.key) m =
      try
         Some (IntMap.find k m)
      with
      | Not_found -> None

   (* min_binding_opt has been introduced in 4.05 while this tool is being developed under 4.04 *)
   let min_binding_opt m =
      try
         Some (IntMap.min_binding m)
      with
      | Not_found -> None

   (* max_binding_opt has been introduced in 4.05 while this tool is being developed under 4.04 *)
   let max_binding_opt m =
      try
         Some (IntMap.max_binding m)
      with
      | Not_found -> None
end

(* NOTE: Top constructor isn't necessary when using a specific format for Val strings.
   For instance, when representing values as Strings in hexadecimal format, one could
   represent unknown byte values as series of "tt", which is not a valid hex value. *)
type el = Mem of string | Top of int

type t = el T.t

let empty = T.empty

(* Utilities *)
let isTop (v : el) =
   match v with
     Top i -> true
   | _     -> false

let length (v : el) =
   match v with
     Mem s -> (String.length s) / 2
   | Top i -> i

let substring (start : int) (sublength : int) (v : el) =
   let maxlength = length v - start in
   let sublength = min sublength maxlength in
   match v with
     Top i -> if start >= 0 && sublength > 0 then Top sublength else Top 0
   | Mem s -> if start >= 0 && sublength > 0 then Mem (String.sub s (start * 2) (sublength * 2)) else Mem ""

let toString (v : el) =
   match v with
     Mem s -> s
   | Top i -> String.make (i * 2) 'T'

let dump (m : el T.t) =
   let printer (k, v) =
      let address = Printf.sprintf "%03d" k in
      let dataLength = string_of_int (length v) in
      print_endline ("at " ^ address ^ ": " ^ (toString v) ^
                     " length: " ^ dataLength ^
                     " next is at " ^
                     address ^ " + " ^ dataLength ^ " = " ^
                     (string_of_int (k + (length v)))
      )
   in
   List.iter (printer) (T.bindings m)

let memory_size (m : t) =
   match T.max_binding_opt m with
     None        -> 0
   | Some (k, v) -> k + length v

(* End of Memory type primitives *)


(* Checks data starting after 'position that might overlap with 'value. *)
let rec check_following_and_finish m (following : (int * el) option) position value =
   match following with
        None        -> (* Nothing after the position to be written *)
                        T.add position value m

      | Some (k, v) ->  (* This is the first position after following data *)
                        let afterFollowing = (k + length v) in
                        (* This is the first position after new data *)
                        let afterNew = position + length value in
                        (* Following data starts before new data's end, gets removed *)
                        let m' = T.remove k m in
                        (* Check boundaries *)
                        if afterFollowing < afterNew then
                        (* Following data ends before what will be the first useful position after write *)
                           (* No holes are expected *)
                           let following' = match T.find_opt afterFollowing m' with
                                None    -> None
                              | Some el -> Some (afterFollowing, el)
                           in
                           (* There might be more data before the first useful position *)
                           check_following_and_finish m' following' position value
                        else if afterFollowing = afterNew then
                        (* Following data ends just where new data will *)
                           T.add position value m'
                        else
                        (* Following data ends after first useful position *)
                           let saveFromFollowing = substring (afterNew - k) (afterFollowing - afterNew) v in
                           let m'' = T.add afterNew saveFromFollowing m' in
                           T.add position value m''


(* Checks if any data starts at 'position. Three cases are possible: new data and old data both
   end at the same point; new data is longer than old data; new data is shorter than old data.
   In the second case, part of following data will be overwritten too, in the third one old data
   will be partially orverwritten. *)
let check_occupied_and_continue m (present : el option) following position value =
   match present with
       None    -> (* Writing just after last data *)
                  T.add position value m

     | Some el -> let afterNew = position + length value in
                  let afterPresent = (position + length el) in
                  if afterPresent < afterNew then
                  (* Present data is shorter than new data: it will be overwritten, and [part of] following data too *)
                     check_following_and_finish m following position value
                  else if afterPresent = afterNew then
                  (* New write data perfectly fits in present data *)
                     T.add position value m
                  else
                  (* Present data is longer than new data, it needs trimming *)
                     let saveFromPresent = substring (afterNew - position) (afterPresent - afterNew) el in
                     let m' = T.add afterNew saveFromPresent m in
                     T.add position value m'


(* Checks if the data inserted before 'position overlaps with the locations that are going to be
   [over]written. Two cases are possible: "simple" overlapping or the enclosing of all locations.
   To give numerical examples: writing four bytes at position 1 when three bytes are present at 0
   or writing four bytes at position 1 when five bytes are present at 0. *)
let check_preceding_and_continue (m : el T.t) (preceding : int * el) present following position value =
   (* expand argument *)
   let (precedingPosition, precedingData) = preceding in
   (* This is the first position after preceding data *)
   let afterPreceding = (precedingPosition + length precedingData) in
   (* This is the first position after new data *)
   let afterNew = position + length value in
   (* Check overlapping *)
   if afterPreceding > position then
      (* Start by cutting off the overlap *)
      let newPrecedingData = substring 0 (position - precedingPosition) precedingData in
      let m' = T.add precedingPosition newPrecedingData m in
      (* Check how far the preceding data reaches *)
      if afterPreceding < afterNew then
         (* the following word might overlap by starting before (position + length) *)
         check_following_and_finish m' following position value
      else if afterPreceding = afterNew then
         (* Only overlapping with preceding data, so writing can go *)
         T.add position value m'
      else
         (* Saving data after (position + length) *)
         let saveFromPreceding = substring (afterNew - precedingPosition) (afterPreceding - afterNew) precedingData in
         let m'' = T.add afterNew saveFromPreceding m' in
         (* Cut-and-paste completed, write new data *)
         T.add position value m''
   else
      (* Check if there's data at position *)
      check_occupied_and_continue m present following position value


(* 0-Expands memory when necessary. This happens when user writes to memory after the first word,
   with the special case of first write to empty memory. NOTE: 0-expansion is not necessary, and
   it would be more space-efficient expanding words upon reading some memory with holes in it.
   Position is assumed to be greater than zero: this is to be checked by the caller *)
let expand_and_continue m preceding present following position value =
   let expand_and_continue_aux firstFreeLocation =
      (* Fill the gap with 0s *)
      let fillerString = Mem (String.make ((position - firstFreeLocation) * 2) '0') in
      let m' = T.add firstFreeLocation fillerString m in
      (* Actual write *)
      T.add position value m'
   in
   match preceding with
       None        -> expand_and_continue_aux 0

     | Some (k, v) -> if k + length v < position then
                      (* Expansion is needed *)
                         expand_and_continue_aux (k + length v)
                      else
                      (* No empty space *)
                         check_preceding_and_continue m (k, v) present following position value



let write (position : int) (value : el) (m : el T.t) =
   (* split IS NECESSARY SINCE find_first_opt AND find_last_opt HAVE BEEN INTRODUCED IN 4.05 *)
   let (all_lt, present, all_gt) = T.split position m in
   let preceding = T.max_binding_opt all_lt in
   let following = T.min_binding_opt all_gt in
   
   if position > 0 then
      expand_and_continue m preceding present following position value
   else
      check_occupied_and_continue m present following position value


let store (bytes: int) position (value : string option) m =
   match value with
     None   -> write position (Top bytes) m
   | Some v -> let memVal =
                  if String.length v < bytes * 2 then
                     (* left padding *)
                     let padder = String.make (bytes * 2 - String.length v) '0' in
                     Mem (padder ^ v)
                  else if String.length v > bytes * 2 then
                     (* get lower bytes *)
                     let lower = String.sub v (String.length v - bytes * 2) (bytes * 2) in
                     Mem lower
                  else
                     Mem v
               in
               write position memVal m



let extract_string (numOfBytes : int) (start : int) (m : t) =
   let rec extract_helper (element : int * el) start (remainder : int) (acc : string) =
      (* expand argument *)
      let (elementPosition, elementData) = element in
      (* extract what possible *)
      let extracted = substring start remainder elementData in

      let newRemainder = remainder - length extracted in
      if newRemainder = 0 then
         acc ^ toString extracted
      else
         let nextPos = elementPosition + length elementData in
         match T.find_opt nextPos m with
           None   -> (acc ^ toString extracted ^ (String.make (newRemainder * 2) '0'))
         | Some e -> extract_helper (nextPos, e) 0 newRemainder (acc ^ toString extracted)
   in
   (* split IS NECESSARY SINCE find_last_opt HAS BEEN INTRODUCED IN 4.05 *)
   let (all_lt, present, all_gt) = T.split start m in
   let preceding = T.max_binding_opt all_lt in

   match (preceding, present) with
     (* After end of data *)
     Some (k, v), None when (k + length v) <= start -> String.make (numOfBytes * 2) '0'
     (* Before end of previous data *)
   | Some (k, v), None                              -> extract_helper (k, v) (start - k) numOfBytes ""
     (* Precise 32-byte-data address *)
   | _, Some e when length e = numOfBytes           -> toString e (* Quicker, might also be most common case *)
     (* Precise, longer-or-shorter-data address *)
   | _, Some e                                      -> extract_helper (start, e) 0 numOfBytes ""
     (* Empty memory *)
   | None, None                                     -> String.make (numOfBytes * 2) '0'







(* Not used *)
let extract_series (element : int * el) (startpos : int) (numOfBytes : int) (m : el T.t) =
   let rec extract_helper element startpos (remainder : int) (acc : string) =
      (* expand argument *)
      let (elementPosition, elementData) = element in

      match elementData with
        Top i ->  Top numOfBytes (* Data containing Top is treated as Top *)
      | Mem s ->  let extracted = Instruction.substr s (startpos * 2) (remainder * 2) in
                  let newRemainder = remainder - String.length extracted / 2 in
                  if newRemainder = 0 then
                     Mem (acc ^ extracted)
                  else
                     let nextPos = elementPosition + length elementData in
                     match T.find_opt nextPos m with
                       None   -> Mem (acc ^ extracted ^ (String.make (newRemainder * 2) '0'))
                     | Some e -> extract_helper (nextPos, e) 0 newRemainder (acc ^ extracted)
   in
   extract_helper element startpos numOfBytes ""


let read (numOfBytes : int) (position : int) (m : el T.t) =
   (* split IS NECESSARY SINCE find_last_opt HAS BEEN INTRODUCED IN 4.05 *)
   let (all_lt, present, all_gt) = T.split position m in
   let preceding = T.max_binding_opt all_lt in

   match (preceding, present) with
     (* After end of data *)
     Some (k, v), None when (k + length v) <= position -> Mem (String.make (numOfBytes * 2) '0')
     (* Before end of previous data *)
   | Some (k, v), None                                 -> extract_series (k, v) (position - k) numOfBytes m
     (* Precise 32-byte-data address *)
   | _, Some e when length e = numOfBytes              -> e (* Quicker, might also be most common case *)
     (* Precise, longer-or-shorter-data address *)
   | _, Some e                                         -> extract_series (position, e) 0 numOfBytes m
     (* Empty memory *)
   | None, None                                        -> Mem (String.make (numOfBytes * 2) '0')










