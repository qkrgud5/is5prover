
CMOS1 = lexer.cmo parser.cmo def.cmo formula.cmo label.cmo labellookup.cmo 
SRC = lexer.mll parser.mly def.ml latex.mli latex.ml formula.mli formula.ml \
label.mli label.ml labellookup.mli labellookup.ml rule.mli rule.ml \
sequent.mli sequent.ml prover.ml Makefile README

CC = ocamlc

prover: prover.ml sequent.cmo rule.cmo latex.cmo
	$(CC) -c prover.ml
	$(CC) -o prover $(CMOS1) latex.cmo rule.cmo sequent.cmo unix.cma prover.cmo 

sequent.cmo: sequent.mli sequent.ml rule.cmo latex.cmo
	$(CC) -c sequent.mli
	$(CC) -c sequent.ml

rule.cmo: rule.mli rule.ml labellookup.cmo label.cmo formula.cmo
	$(CC) -c rule.mli
	$(CC) -c rule.ml

labellookup.cmo: labellookup.mli labellookup.ml label.cmo
	$(CC) -c labellookup.mli
	$(CC) -c labellookup.ml

label.cmo: label.mli label.ml formula.mli formula.cmo
	$(CC) -c label.mli
	$(CC) -c label.ml

formula.cmo: formula.mli formula.ml parser.cmo lexer.cmo def.cmo
	$(CC) -c formula.mli
	$(CC) -c formula.ml

parser.cmo: lexer.mll parser.mly def.cmo
	ocamllex lexer.mll 
	ocamlyacc parser.mly
	$(CC) -c parser.mli
	$(CC) -c lexer.ml
	$(CC) -c parser.ml

def.cmo: def.ml
	$(CC) -c def.ml

latex.cmo: latex.mli latex.ml
	$(CC) -c latex.mli
	$(CC) -c latex.ml

clean:
	rm -f *.cmo *.cma lexer.ml parser.ml parser.mli *.cmi result\.* sequent 
	rm -f *.tex *.log result[0-9]*\.*

cleanswp:
	rm -f .*.swp

targz: $(SRC)
	mkdir is5 ; cp $(SRC) is5 ; tar -cvf prover.tar is5 ; gzip prover.tar ; rm -r -f is5
