Projet Petit Pyret - Partie 1  

Projet de compilation ENS 2025-2026


##############################################################

Structure du projet

##############################################################

Le projet est organisé en plusieurs modules OCaml.  

        
        ast.ml : Définition des arbres de syntaxe abstraite  
        lexer.mll : Analyseur lexical (ocamllex)  
        parser.mly : Analyseur syntaxique (Menhir)  
        typing.ml : Typage statique  
        pyretc.ml : Programme principal  


##############################################################

Tests

##############################################################

Le projet passe les tests fournis  avec :

    make tests

Pour les tests d'analyse syntaxique :

    ./test -1 ./pyretc.exe

Pour les tests d'analyse sémantique :

    ./test -2 ./pyretc.exe

Compilation :

    make            # Compiler le projet  
    make tests      # Lancer les tests
    make clean      # Nettoyer