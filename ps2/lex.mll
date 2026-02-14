(* Lexer for Fish --- TODO *)

(* You need to add new definition to build the
 * appropriate terminals to feed to parse.mly.
 *)

{
open Parse
open Lexing

let incr_lineno lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }
}

(* definition section *)
let cr='\013'
let nl='\010'
let eol=(cr nl|nl|cr)
let ws=('\012'|'\t'|' ')*
let digit=['0'-'9'] 

(* rules section *)
rule lexer = parse
| eol             { incr_lineno lexbuf; lexer lexbuf } (* when we hit a newline, increment the line number, and recursively call the lexer *)
| ws+             { lexer lexbuf }                     (* matches one or more spaces or tabs and skip them *)
| digit+ as value { INT(int_of_string(value)) }

(* Keywords *)
| "if"            { IF }
| "else"          { ELSE }
| "while"         { WHILE }
| "for"           { FOR }
| "return"        { RETURN }

(* Identifiers *)
| ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as value { VAR(value) }

(* Multi-character Operators *)
| "=="            { EQ }
| "!="            { NEQ }
| "<="            { LTE }
| ">="            { GTE }
| "&&"            { AND }
| "||"            { OR }

(* Single-character Operators & Syntax *)
| "+"             { PLUS }
| "-"             { MINUS }
| "*"             { TIMES }
| "/"             { DIV }
| "<"             { LT }
| ">"             { GT }
| "!"             { NOT }
| "="             { ASSIGN }
| "("             { LPAREN }
| ")"             { RPAREN }
| "{"             { LBRACE }
| "}"             { RBRACE }
| ";"             { SEMI }
| ","             { COMMA }

| "/*"            { comment lexbuf }                      (* Call a separate rule for comments *)
| eof             { EOF }
| _               { raise (Failure ("Illegal character: " ^ Lexing.lexeme lexbuf)) }

and comment = parse
| "*/"            { lexer lexbuf }                        (* Exit comment and resume normal lexing *)
| eol             { incr_lineno lexbuf; comment lexbuf }  (* Track line numbers inside comments *)
| _               { comment lexbuf }                      (* Ignore any other character *)