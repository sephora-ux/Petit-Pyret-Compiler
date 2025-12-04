(* Analyseur lexical pour Pyret *)

{  
open Lexing
  open Parser

  exception Lexing_error of string

  let id_or_kwd =
    let h = Hashtbl.create 32 in
    List.iter (fun (s, tok) -> Hashtbl.add h s tok)
      [
        "and", AND;
        "block", BLOCK;
        "cases", CASES;
        "else", ELSE;
        "end", END;
        "false", FALSE;
        "for", FOR;
        "from", FROM;
        "fun", FUN;
        "if", IF;
        "lam", LAM;
        "or", OR;
        "true", TRUE;
        "var", VAR;
      ];
    fun s ->
      try Hashtbl.find h s 
      with Not_found -> IDENT s

  let string_buffer = Buffer.create 1024
}

let letter = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let ident_part = letter | digit
let ident = letter (ident_part | '-')*
let integer = ['+' '-']? digit+
let space = [' ' '\t']
let newline = '\n' | '\r' | "\r\n"

rule token = parse
  | newline { new_line lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | "#|"    { comment_block 1 lexbuf; token lexbuf }
  | '#' [^'\n' '\r']* { token lexbuf }
  
  | "block:" { BLOCK_COLON }
  | "else:" { ELSE_COLON }
  
  | ident as id { id_or_kwd id }
  
  | integer as s { INT (int_of_string s) }
  
  | '"'  { STRING (string_dquote lexbuf) }
  | '\'' { STRING (string_squote lexbuf) }
  
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | '<' { LT }
  | '>' { GT }
  | ',' { COMMA }
  | ':' { COLON }
  | '=' { EQ }
  | ":=" { COLONEQ }
  | "->" { ARROW }
  | "=>" { DARROW }
  | '|' { PIPE }
  | "::" { COLONCOLON }
  
  (* Opérateurs binaires *)
  | "==" { EQEQ }
  | "<>" { NEQ }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  
  | eof { EOF }
  | _ as c { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

and comment_block depth = parse
  | "#|" { comment_block (depth + 1) lexbuf }
  | "|#" { if depth = 1 then () else comment_block (depth - 1) lexbuf }
  | newline { new_line lexbuf; comment_block depth lexbuf }
  | eof { raise (Lexing_error "unterminated comment") }
  | _ { comment_block depth lexbuf }

and string_dquote = parse
  | '"' { 
      let s = Buffer.contents string_buffer in
      Buffer.reset string_buffer;
      s 
    }
  | "\\\"" { Buffer.add_char string_buffer '"'; string_dquote lexbuf }
  | "\\'" { Buffer.add_char string_buffer '\''; string_dquote lexbuf }
  | "\\\\" { Buffer.add_char string_buffer '\\'; string_dquote lexbuf }
  | "\\t" { Buffer.add_char string_buffer '\t'; string_dquote lexbuf }
  | "\\n" { Buffer.add_char string_buffer '\n'; string_dquote lexbuf }
  | newline { raise (Lexing_error "newline in string") }
  | eof { raise (Lexing_error "unterminated string") }
  | _ as c { Buffer.add_char string_buffer c; string_dquote lexbuf }

and string_squote = parse
  | '\'' { 
      let s = Buffer.contents string_buffer in
      Buffer.reset string_buffer;
      s 
    }
  | "\\\"" { Buffer.add_char string_buffer '"'; string_squote lexbuf }
  | "\\'" { Buffer.add_char string_buffer '\''; string_squote lexbuf }
  | "\\\\" { Buffer.add_char string_buffer '\\'; string_squote lexbuf }
  | "\\t" { Buffer.add_char string_buffer '\t'; string_squote lexbuf }
  | "\\n" { Buffer.add_char string_buffer '\n'; string_squote lexbuf }
  | newline { raise (Lexing_error "newline in string") }
  | eof { raise (Lexing_error "unterminated string") }
  | _ as c { Buffer.add_char string_buffer c; string_squote lexbuf }