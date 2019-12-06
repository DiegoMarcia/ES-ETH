type levels = Trace | Debug | Info | Warn | Error | Fatal

let level_to_int = function
    Trace -> 0
  | Debug -> 10
  | Info  -> 20
  | Warn  -> 30
  | Error -> 40
  | Fatal -> 50

type t = {
      indent   : int ;
      severity : levels ;
      message  : string ;
      tag      : string ;
}

type log = {
   messages : t list ;
   indent   : int    ;
   tag      : string list ;
}


let empty = {
   messages = [] ;
   indent = 0 ;
   tag = [] ;
}


let new_level t l =
   { l with indent = l.indent + 2 ; tag = t :: l.tag }

let level_back l =
   if l.tag = [] then
      l
   else
      { l with indent = if l.indent > 1 then l.indent - 2 else 0 ; tag = List.tl l.tag }


let add_message s lev l =
   let m = { indent = l.indent ; severity = lev ; tag = if l.tag = [] then "" else List.hd l.tag ; message = s } in
   { l with messages = m :: l.messages }



let print_all log minLevel =
   let intMinLevel = level_to_int minLevel in
   List.iter (
               fun m ->
                  if level_to_int m.severity >= intMinLevel then
                     (
                        print_string (String.make m.indent ' ');
                        print_endline ((if m.tag = "" then m.tag else (m.tag ^ ": ")) ^ m.message)
                     )
             )
             (List.rev log.messages)

