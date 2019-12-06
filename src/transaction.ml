type t =
{
      origin    : string ;
      from      : string ;
      gas       : string option ;
      hash      : string option ;
      calldata  : string option ;
      recipient : string ;
      callvalue : string option ;
      gasprice  : string option ;
}

(* Used when we run on contracts only *)
let noTransaction this =
{
      origin    = ""    ;
      from      = "00"  ;
      gas       = None  ;
      hash      = None  ;
      calldata  = None    ;
      recipient = this  ;
      callvalue = None  ;
      gasprice  = None  ;
}

(* Used by CALLs *)
let internal o f g cd r cv gp =
{
      origin    = o    ;
      from      = f    ;
      gas       = g    ;
      hash      = None ;
      calldata  = cd   ;
      recipient = r    ;
      callvalue = cv   ;
      gasprice  = gp   ;
}

let get_origin t = t.origin
let get_from t = t.from
let get_calldata t = t.calldata
let get_recipient t = t.recipient
let get_callvalue t = t.callvalue
let get_gasprice t = t.gasprice
