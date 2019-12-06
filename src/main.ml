
let () =
   if Array.length Sys.argv < 1 then
      exit 1
   else
      let (transaction, bytecode) = Arguments.parse_args () in
      let (flow, msgs, variables) = Exec.pub_exec transaction bytecode in
      Message.print_all msgs Message.Fatal ;
      Variables.print variables