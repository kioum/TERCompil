#
# Mettre la liste des fichiers .ml, .mly, .mll et .mli
# constituant le projet. Si un fichier b.ml dépend d'un fichier
# a.ml, a.ml doit se trouver avant dans la liste.
#

SOURCES = ast.ml typing.ml parser.mly lexer.mll main.ml 

# Nom du binaire

EXEC = deca

#######################################################################
# Partie générique, ne pas modifier.
#######################################################################




# Compilateurs

CAMLC = ocamlc -g
CAMLOPT = ocamlopt
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = menhir

# Sources
# Un peu de magie noire pour détecter les .mly/.mll et convaincre make
# d'appliquer les bonnes règles pour générer les .ml, .mli etc. correpondants.

PARSER_MLY=$(filter %.mly, $(SOURCES))
PARSER_MLI=$(PARSER_MLY:.mly=.mli)
PARSER_ML=$(PARSER_MLY:.mly=.ml)

LEXER_MLL=$(filter %.mll, $(SOURCES))
LEXER_ML=$(LEXER_MLL:.mll=.ml)

ALL_SOURCES=$(patsubst $(LEXER_MLL), $(LEXER_ML), $(patsubst $(PARSER_MLY), $(PARSER_MLI) $(PARSER_ML), $(SOURCES)))

OBJS = $(patsubst %.ml, %.cmo, $(filter %.ml, $(ALL_SOURCES)))
OPTOBJS = $(OBJS:.cmo=.cmx)
JS_OBJS = $(subst main.cmo, , $(OBJS))
TESTS_SRC = $(shell ls tests/*.java)



.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly .java
.PHONY: depend

all: depend $(EXEC)

opt: depend $(EXEC).opt


$(EXEC): $(OBJS)
	$(CAMLC) $(CUSTOM) -o $(EXEC) $(OBJS)

$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) -o $(EXEC) $(OPTOBJS)

.ml.cmo: 
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx: 
	$(CAMLOPT) -c $<

$(LEXER_ML): $(LEXER_MLL)
	$(CAMLLEX) $<

$(PARSER_MLI): $(PARSER_ML)

$(PARSER_ML): $(PARSER_MLY)
	$(CAMLYACC) --infer $<


%.class: %.java
	javac $<

%.exe: %.s
	gcc -o $@ -g -static $<

%.s: %.java $(EXEC)
	./$(EXEC) $<

test: all

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f $(PARSER_ML) $(PARSER_MLI) $(LEXER_ML)
	rm -f $(EXEC)
	rm -f $(EXEC).opt

depend: $(ALL_SOURCES)
	$(CAMLDEP) $(ALL_SOURCES) > depend
	$(CAMLYACC) --depend $(PARSER_MLY) >> depend

-include depend
