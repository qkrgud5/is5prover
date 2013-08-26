
CMOS1 = lexer.cmx parser.cmx def.cmx formula.cmx label.cmx labellookup.cmx 
SRC = lexer.mll parser.mly def.ml latex.mli latex.ml formula.mli formula.ml \
label.mli label.ml labellookup.mli labellookup.ml rule.mli rule.ml \
sequent.mli sequent.ml prover.ml Makefile README

CC = ocamlopt

prover: prover.ml sequent.cmx rule.cmx latex.cmx
	$(CC) -c prover.ml
	$(CC) -o prover $(CMOS1) latex.cmx rule.cmx sequent.cmx unix.cmxa prover.cmx 

sequent.cmx: sequent.mli sequent.ml rule.cmx latex.cmx
	$(CC) -c sequent.mli
	$(CC) -c sequent.ml

rule.cmx: rule.mli rule.ml labellookup.cmx label.cmx formula.cmx
	$(CC) -c rule.mli
	$(CC) -c rule.ml

labellookup.cmx: labellookup.mli labellookup.ml label.cmx
	$(CC) -c labellookup.mli
	$(CC) -c labellookup.ml

label.cmx: label.mli label.ml formula.mli formula.cmx
	$(CC) -c label.mli
	$(CC) -c label.ml

formula.cmx: formula.mli formula.ml parser.cmx lexer.cmx def.cmx
	$(CC) -c formula.mli
	$(CC) -c formula.ml

parser.cmx: lexer.mll parser.mly def.cmx
	ocamllex lexer.mll 
	ocamlyacc parser.mly
	$(CC) -c parser.mli
	$(CC) -c lexer.ml
	$(CC) -c parser.ml

def.cmx: def.ml
	$(CC) -c def.ml

latex.cmx: latex.mli latex.ml
	$(CC) -c latex.mli
	$(CC) -c latex.ml

clean:
	rm -f *.cmx *.cma lexer.ml parser.ml parser.mli *.cmi result\.* sequent 
	rm -f *.tex *.log result[0-9]*\.*

cleanswp:
	rm -f .*.swp

targz: $(SRC)
	mkdir is5 ; cp $(SRC) is5 ; tar -cvf prover.tar is5 ; gzip prover.tar ; rm -r -f is5
