type looper = {
   unexplored : (Data.t * DS.PCSet.t) list ;
   loopFound  : bool                       ;
}

let new_looper = { unexplored = [] ; loopFound = false }

(*
(* Polymorphic example, requires records with function in f field instead of functions, but can be defined independently from type t: *)
type jumpDestFunction = { f : 'a 'b 'c 'd 'e 'f 'g. 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'e * 'g * 'd }
type jumpFunction     = { f : 'a    'c 'd 'e 'f 'g. 'a ->       'c -> 'd -> 'e -> 'f -> 'g -> 'e * 'g * 'd }
(* Hiding other fields in type t: *)
type t = { ... ; jumpDestNormal : jumpDestFunction ; jumpNormal : jumpFunction ; jumpDestLoop : jumpDestFunction ; jumpLoop : jumpFunction ; }
let init jdfn jfn jdfl jfl = { ... ; jumpDestNormal = { f = jdfn } ; jumpNormal = { f = jfn } ; jumpDestLoop = { f = jdfl } ; jumpLoop = { f = jfl } ; }
*)
type jumpDestFunction = string -> int -> Data.t -> Variables.t -> t -> (Data.t * DS.PCSet.t) list -> Message.log -> t * Message.log * Variables.t
and  jumpFunction     = string ->        Data.t -> Variables.t -> t -> (Data.t * DS.PCSet.t) list -> Message.log -> t * Message.log * Variables.t
and t = {
   visited        : DS.PCSet.t     ;
   suspects       : DS.PCSet.t     ;
   safeJumps      : int DS.PCMap.t ;
   loopMode       : bool           ;
   loopData       : looper         ;
   jumpDestNormal : jumpDestFunction ;
   jumpNormal     : jumpFunction ;
   jumpiNormal    : jumpFunction ;
   jumpDestLoop   : jumpDestFunction ;
   jumpLoop       : jumpFunction ;
   jumpiLoop      : jumpFunction ;
}

let init jdfn jfn jifn jdfl jfl jifl = {
   visited        = DS.PCSet.empty ;
   suspects       = DS.PCSet.empty ;
   safeJumps      = DS.PCMap.empty ;
   loopMode       = false          ;
   loopData       = new_looper     ;
   jumpDestNormal = jdfn           ;
   jumpNormal     = jfn            ;
   jumpiNormal    = jifn           ;
   jumpDestLoop   = jdfl           ;
   jumpLoop       = jfl            ;
   jumpiLoop      =jifl            ;
}

let add_visited pc f =
   { f with visited = DS.PCSet.add pc f.visited }

let is_visited pc f =
   DS.PCSet.mem pc f.visited

let get_visited f =
   f.visited

let update_visited v f =
   { f with visited = v }

let add_suspect pc f =
   { f with suspects = DS.PCSet.add pc f.suspects }

let is_suspect pc f =
   DS.PCSet.mem pc f.suspects

let add_safeJump k v f =
   { f with safeJumps = DS.PCMap.add k v f.safeJumps }

let is_safe pc f =
   DS.PCMap.mem pc f.safeJumps

let get_safeJumps f =
   DS.PCMap.bindings f.safeJumps

let looping f =
   f.loopMode

let set_looping f =
   { f with loopMode = true ; loopData = new_looper }

let set_normal f =
   { f with loopMode = false }

let add_unexplored u f =
   { f with loopData = { f.loopData with unexplored = u @ f.loopData.unexplored } }

let get_unexplored f =
   f.loopData.unexplored

let update_loopFound found f =
   { f with loopData = { f.loopData with loopFound = found } }

let get_loopFound f =
   f.loopData.loopFound

let get_jumpDestFun f =
   if f.loopMode then
      f.jumpDestLoop
   else
      f.jumpDestNormal

let get_jumpFun f =
   if f.loopMode then
      f.jumpLoop
   else
      f.jumpNormal

let get_jumpiFun f =
   if f.loopMode then
      f.jumpiLoop
   else
      f.jumpiNormal
