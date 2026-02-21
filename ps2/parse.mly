/* Parser for Fish --- TODO */

%{
open Ast
open Lexing

(* use this to get the line number for the n'th token *)
let rhs n =
  let pos = Parsing.rhs_start_pos n in
  pos.pos_lnum

(* use this to print an error message *)
let parse_error s =
  let pos = Parsing.symbol_end_pos () in
  let l = pos.pos_lnum in
  print_string ("line "^(string_of_int l)^": "^s^"\n") 
%}

/* Tells us which non-terminal to start the grammar with. */
%start program

/* This specifies the non-terminals of the grammar and specifies the
 * types of the values they build. 
 * Don't forget to add any new non-terminals here.
 */
%type <Ast.program> program
%type <Ast.stmt> stmt
%type <Ast.exp> exp
%type <Ast.stmt> stmt_list

/* The %token directive gives a definition of all of the terminals
 * (i.e., tokens) in the grammar. This will be used to generate the
 * tokens definition used by the lexer. So this is effectively the
 * interface between the lexer and the parser --- the lexer must
 * build values using this datatype constructor to pass to the parser.
 * You will need to augment this with your own tokens...
 */

/* we use brackets for containers that need to store the actual value of an INT or a VAR */
%token <int> INT 
%token <string> VAR
%token PLUS MINUS TIMES DIV
%token EQ NEQ LT LTE GT GTE
%token AND OR NOT
%token ASSIGN
%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA
%token RETURN IF ELSE WHILE FOR
%token EOF

/* Priority increases as we move down the list */
%left OR
%left AND
%left EQ NEQ LT LTE GT GTE
%left PLUS MINUS
%left TIMES DIV

/* nonassoc means the operators cannot be chained. e.g., NOT NOT or x < y < z are invalid */
%nonassoc NOT
%nonassoc UMINUS  // unary minus e.g., -42

/* dangling else - we give priority to match else to "if" that came right before, i.e., when we see "if", we check whether "if" is followed by an "else" */
%nonassoc IF_WITHOUT_ELSE
%nonassoc TRY_ELSE

/* Here's where the real grammar starts -- you'll need to add 
 * more rules here... Do not remove the 2%'s!! 
 */
%%

/* $1, $2, $3 matches the components of the LHS e.g., PLUS is $2 of exp PLUS exp */

program:
  // stmt EOF { $1 }
  | EOF               { (Ast.Return((Ast.Int(0), 0)), 0) }
  | stmt_list EOF     { $1 }

exp:    
  | INT                     { (Ast.Int($1), rhs 1) }
  | VAR                     { (Ast.Var($1), rhs 1) }
  | exp PLUS exp            { (Ast.Binop($1, Ast.Plus, $3), rhs 2) }
  | exp MINUS exp           { (Ast.Binop($1, Ast.Minus, $3), rhs 2) }
  | exp TIMES exp           { (Ast.Binop($1, Ast.Times, $3), rhs 2) }
  | exp DIV exp             { (Ast.Binop($1, Ast.Div, $3), rhs 2) }
  | exp EQ exp              { (Ast.Binop($1, Ast.Eq, $3), rhs 2) }
  | exp NEQ exp             { (Ast.Binop($1, Ast.Neq, $3), rhs 2) }
  | exp LT exp              { (Ast.Binop($1, Ast.Lt, $3), rhs 2) }
  | exp LTE exp             { (Ast.Binop($1, Ast.Lte, $3), rhs 2) }
  | exp GT exp              { (Ast.Binop($1, Ast.Gt, $3), rhs 2) }
  | exp GTE exp             { (Ast.Binop($1, Ast.Gte, $3), rhs 2) }
  | exp AND exp             { (Ast.And($1, $3), rhs 2) }
  | exp OR exp              { (Ast.Or($1, $3), rhs 2) }
  | NOT exp                 { (Ast.Not($2), rhs 1) }
  | VAR ASSIGN exp          { (Ast.Assign($1, $3), rhs 2) }
  | LPAREN exp RPAREN       { $2 }
  | MINUS exp %prec UMINUS  { (Ast.Binop((Ast.Int(0), rhs 1), Ast.Minus, $2), rhs 1) }

stmt:
  | exp SEMI                                              { (Ast.Exp($1), rhs 2) }
  | LBRACE stmt_list RBRACE                               { $2 }
  | IF LPAREN exp RPAREN stmt %prec TRY_ELSE ELSE stmt    { (Ast.If($3, $5, $7), rhs 1) }
  | IF LPAREN exp RPAREN stmt %prec IF_WITHOUT_ELSE       { (Ast.If($3, $5, (Ast.skip, rhs 1)), rhs 1) }
  | WHILE LPAREN exp RPAREN stmt                          { (Ast.While($3, $5), rhs 1) }
  | FOR LPAREN exp SEMI exp SEMI exp RPAREN stmt          { (Ast.For($3, $5, $7, $9), rhs 1) }
  | RETURN exp SEMI                                       { (Ast.Return($2), rhs 1) }
  | SEMI                                                  { (Ast.skip, rhs 1) }

stmt_list:
  | stmt              { $1 }
  | stmt stmt_list    { (Ast.Seq($1, $2), rhs 1) }
