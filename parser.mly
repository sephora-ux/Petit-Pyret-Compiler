/* Analyseur syntaxique pour Pyret */

%{
  open Ast
%}

%token EOF
%token <int> INT 
%token <string> IDENT STRING
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN LBRACK RBRACK
%token LT GT LEQ GEQ EQEQ NEQ
%token AND OR
%token COMMA COLON COLONCOLON EQ COLONEQ ARROW DARROW PIPE
%token BLOCK CASES IF ELSE END FOR FROM FUN LAM VAR TRUE FALSE
%token BLOCK_COLON ELSE_COLON

%start prog
%type <Ast.file> prog

%%

prog:
  | sl=list(stmt) EOF { ([], Sblock sl) }
;

stmt:
  | FUN f=IDENT tp=loption(delimited(LT, separated_nonempty_list(COMMA, IDENT), GT))
    LPAREN ps=separated_list(COMMA, param) RPAREN rt=preceded(ARROW, typ) 
    ub=ublock b=block END
    { Sfundef (f, tp, ps, rt, ub, b) }
  | v=boption(VAR) x=IDENT a=option(preceded(COLONCOLON, typ)) EQ e=bexpr
    { if v then Svardef (x, a, e) else Sletdef (x, a, e) }
  | x=IDENT COLONEQ e=bexpr
    { Smut (x, e) }
  | e=expr
    { Seval e }
;

param:
  | x=IDENT COLONCOLON t=typ { (x, t) }
;

ublock:
  | COLON { false }
  | BLOCK_COLON { true }
;

typ:
  | name=IDENT ta=option(delimited(LT, separated_nonempty_list(COMMA, typ), GT))
    { match ta with None -> Tident name | Some ts -> Tpoly (name, ts) }
  | LPAREN tl=separated_list(COMMA, typ) ARROW t=typ RPAREN
    { Tfun (tl, t) }
;

block:
  | sl=nonempty_list(stmt) { sl }
;

bexpr:
  | e=expr { e }
  | e1=expr op=binop e2=expr
    { Ebinop (op, e1, e2) }
  | e1=bexpr op=binop e2=expr
    { match e1 with
      | Ebinop (op1, _, _) when op1 = op -> Ebinop (op, e1, e2)
      | Ebinop (op1, _, _) -> raise Parsing.Parse_error
      | _ -> Ebinop (op, e1, e2) }
;

binop:
  | EQEQ { Beq } | NEQ { Bneq } | LT { Blt } | LEQ { Ble }
  | GT { Bgt } | GEQ { Bge } | PLUS { Badd } | MINUS { Bsub }
  | TIMES { Bmul } | DIV { Bdiv } | AND { Band } | OR { Bor }
;

expr:
  | primary=primary_expr { primary }
  | e=expr LPAREN al=separated_nonempty_list(COMMA, bexpr) RPAREN
    { Ecall (e, al) }
  | e=expr LPAREN RPAREN
    { Ecall (e, []) }
;

primary_expr:
  | TRUE { Ecst (Cbool true) }
  | FALSE { Ecst (Cbool false) }
  | n=INT { Ecst (Cint n) }
  | s=STRING { Ecst (Cstring s) }
  | x=IDENT { Eident x }
  | LPAREN e=bexpr RPAREN { e }
  | BLOCK_COLON b=block END { Eblock b }
  | IF e=bexpr ub=ublock b=block 
    el=list(elif_clause)
    els=option(else_clause) END
    { Eif (e, ub, b, el, els) }
  | LAM LPAREN ps=separated_list(COMMA, param) RPAREN 
    rt=preceded(ARROW, typ) ub=ublock b=block END
    { Elam (ps, rt, ub, b) }
  | CASES LPAREN t=typ RPAREN e=bexpr ub=ublock bl=list(branch) END
    { Ecases (t, e, ub, bl) }
  | FOR c=caller LPAREN fl=separated_list(COMMA, from_clause) RPAREN 
    rt=preceded(ARROW, typ) ub=ublock b=block END
    { Efor (c, fl, rt, ub, b) }
;

elif_clause:
  | ELSE IF e=bexpr COLON b=block { (e, b) }
;

else_clause:
  | ELSE_COLON b=block { b }
;

caller:
  | x=IDENT { x }
;

branch:
  | PIPE n=IDENT al=option(delimited(LPAREN, separated_list(COMMA, branch_arg), RPAREN))
    DARROW b=block
    { let args = match al with None -> [] | Some l -> l in
      (n, args, b) }
;

branch_arg:
  | x=IDENT { if x = "_" then None else Some x }
;

from_clause:
  | x=IDENT COLONCOLON t=typ FROM e=bexpr { ((x, t), e) }
;