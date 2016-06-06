
CP=cp

OCAMLBUILD_FLAGS=-use-ocamlfind -I src -I bin
OCAMLBUILD=ocamlbuild $(OCAMLBUILD_FLAGS)

all: prof_spacetime prof_spacetime_sturgeon

FORCE:

main.native: FORCE
	$(OCAMLBUILD) main.native

prof_spacetime: main.native
	$(CP) main.native prof_spacetime

sturgeon_main.native: FORCE
	$(OCAMLBUILD) sturgeon_main.native

prof_spacetime_sturgeon: sturgeon_main.native
	$(CP) sturgeon_main.native prof_spacetime_sturgeon

clean:
	$(OCAMLBUILD) -clean
