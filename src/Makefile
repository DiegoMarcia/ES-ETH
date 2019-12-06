sources = instruction.mli instruction.ml opStack.mli opStack.ml memory.mli memory.ml DS.mli DS.ml transaction.mli transaction.ml data.ml tokenizer.mli tokenizer.ml evaluate.mli evaluate.ml message.mli message.ml variables.mli variables.ml flow.ml arguments.mli arguments.ml exec.mli exec.ml main.ml
objects = ../bin/ES-ETH

$(objects): $(sources)
	ocamlfind ocamlc str.cma unix.cma nums.cma -g -annot -o $(objects) -linkpkg -package yojson $(sources)

clean:
	rm *.cmi *.cmo *.annot ../bin/ES-ETH
