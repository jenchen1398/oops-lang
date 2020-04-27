type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | ASSIGN
  | EQ
  | NEQ
  | LESSER
  | LESSEREQ
  | GREATER
  | GREATEREQ
  | AND
  | OR
  | IF
  | ELSE
  | WHILE
  | FOR
  | INT
  | BOOL
  | STR
  | RETURN
  | COMMA
  | DOT
  | CLASS
  | PRIVATE
  | PUBLIC
  | PROTECTED
  | NEW
  | CONS
  | NUM of (int)
  | BLIT of (bool)
  | STRLIT of (string)
  | ID of (string)
  | OBJECT of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Ast
# 51 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* LBRACK *);
  263 (* RBRACK *);
  264 (* PLUS *);
  265 (* MINUS *);
  266 (* TIMES *);
  267 (* DIVIDE *);
  268 (* MOD *);
  269 (* ASSIGN *);
  270 (* EQ *);
  271 (* NEQ *);
  272 (* LESSER *);
  273 (* LESSEREQ *);
  274 (* GREATER *);
  275 (* GREATEREQ *);
  276 (* AND *);
  277 (* OR *);
  278 (* IF *);
  279 (* ELSE *);
  280 (* WHILE *);
  281 (* FOR *);
  282 (* INT *);
  283 (* BOOL *);
  284 (* STR *);
  285 (* RETURN *);
  286 (* COMMA *);
  287 (* DOT *);
  288 (* CLASS *);
  289 (* PRIVATE *);
  290 (* PUBLIC *);
  291 (* PROTECTED *);
  292 (* NEW *);
  293 (* CONS *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  294 (* NUM *);
  295 (* BLIT *);
  296 (* STRLIT *);
  297 (* ID *);
  298 (* OBJECT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\007\000\006\000\006\000\005\000\
\005\000\005\000\003\000\003\000\003\000\009\000\009\000\011\000\
\011\000\013\000\013\000\015\000\014\000\014\000\014\000\012\000\
\016\000\016\000\017\000\017\000\010\000\010\000\018\000\018\000\
\018\000\018\000\018\000\018\000\019\000\019\000\019\000\019\000\
\019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\019\000\019\000\019\000\019\000\020\000\020\000\008\000\008\000\
\021\000\021\000\000\000"

let yylen = "\002\000\
\003\000\000\000\002\000\007\000\009\000\001\000\002\000\001\000\
\001\000\001\000\000\000\003\000\002\000\000\000\003\000\002\000\
\005\000\001\000\001\000\001\000\001\000\001\000\001\000\008\000\
\000\000\001\000\001\000\003\000\000\000\002\000\002\000\003\000\
\007\000\005\000\009\000\003\000\001\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\004\000\003\000\
\003\000\004\000\006\000\005\000\001\000\003\000\000\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\009\000\010\000\067\000\000\000\000\000\
\000\000\021\000\022\000\023\000\020\000\000\000\000\000\000\000\
\000\000\018\000\019\000\003\000\000\000\001\000\000\000\000\000\
\013\000\000\000\016\000\000\000\012\000\000\000\000\000\026\000\
\000\000\000\000\000\000\000\000\000\000\000\000\028\000\000\000\
\017\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\037\000\038\000\039\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
\030\000\031\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\015\000\
\000\000\000\000\064\000\057\000\032\000\000\000\056\000\000\000\
\000\000\000\000\036\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\045\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\062\000\
\000\000\000\000\000\000\000\000\058\000\055\000\000\000\000\000\
\066\000\000\000\034\000\000\000\060\000\000\000\000\000\000\000\
\000\000\059\000\000\000\033\000\000\000\005\000\000\000\035\000"

let yydgoto = "\002\000\
\006\000\007\000\014\000\008\000\009\000\043\000\044\000\097\000\
\045\000\062\000\015\000\016\000\017\000\018\000\019\000\031\000\
\032\000\063\000\064\000\070\000\099\000"

let yysindex = "\024\000\
\244\254\000\000\000\000\000\000\000\000\000\000\035\255\244\254\
\253\254\000\000\000\000\000\000\000\000\031\000\008\255\035\255\
\002\255\000\000\000\000\000\000\016\255\000\000\035\255\035\255\
\000\000\006\255\000\000\056\255\000\000\041\255\075\255\000\000\
\066\255\035\255\035\255\080\255\033\255\049\255\000\000\035\255\
\000\000\048\255\086\255\049\255\092\255\099\255\103\255\000\000\
\000\000\070\255\092\255\070\255\111\255\113\255\120\255\070\255\
\081\255\000\000\000\000\000\000\011\255\119\255\092\255\160\000\
\035\255\070\255\242\000\121\255\120\000\118\255\070\255\070\255\
\070\255\181\000\125\255\070\255\091\255\070\255\094\255\000\000\
\000\000\000\000\070\255\070\255\070\255\070\255\070\255\070\255\
\070\255\070\255\070\255\070\255\070\255\070\255\070\255\000\000\
\134\255\137\000\000\000\000\000\000\000\070\255\000\000\005\001\
\024\001\202\000\000\000\070\255\135\255\139\255\057\001\137\255\
\108\255\108\255\143\255\143\255\000\000\096\001\096\001\071\255\
\071\255\071\255\071\255\084\001\071\001\152\255\070\255\000\000\
\092\255\092\255\070\255\154\255\000\000\000\000\070\255\035\255\
\000\000\136\255\000\000\223\000\000\000\158\255\092\255\092\255\
\070\255\000\000\164\255\000\000\043\001\000\000\092\255\000\000"

let yyrindex = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\162\000\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\
\000\000\000\000\000\000\000\000\000\000\000\000\002\000\167\255\
\000\000\000\000\000\000\000\000\000\000\176\255\000\000\000\000\
\000\000\144\255\000\000\000\000\000\000\000\000\000\000\063\255\
\000\000\000\000\000\000\175\255\178\255\000\000\000\000\000\000\
\000\000\000\000\178\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\133\255\000\000\178\255\000\000\
\063\255\182\255\000\000\000\000\179\255\000\000\000\000\000\000\
\000\000\000\000\000\000\182\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\190\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\182\255\000\000\000\000\029\255\000\000\
\205\255\229\255\157\255\181\255\000\000\079\000\082\000\250\255\
\016\000\037\000\058\000\103\000\251\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\182\255\063\255\
\000\000\000\000\000\000\000\000\000\000\000\000\178\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\186\000\003\000\000\000\000\000\159\000\000\000\187\255\
\195\255\208\255\232\255\000\000\000\000\000\000\000\000\000\000\
\169\000\132\255\218\255\105\000\078\000"

let yytablesize = 627
let yytable = "\030\000\
\002\000\011\000\068\000\096\000\138\000\139\000\109\000\026\000\
\023\000\024\000\030\000\067\000\076\000\069\000\081\000\046\000\
\077\000\074\000\025\000\148\000\003\000\004\000\005\000\078\000\
\001\000\029\000\152\000\098\000\021\000\054\000\022\000\054\000\
\104\000\105\000\106\000\054\000\038\000\098\000\132\000\111\000\
\046\000\079\000\027\000\033\000\113\000\114\000\115\000\116\000\
\117\000\118\000\119\000\120\000\121\000\122\000\123\000\124\000\
\125\000\028\000\054\000\034\000\010\000\011\000\012\000\069\000\
\014\000\142\000\014\000\014\000\014\000\098\000\035\000\050\000\
\037\000\041\000\143\000\052\000\013\000\036\000\083\000\084\000\
\085\000\086\000\087\000\040\000\014\000\042\000\014\000\014\000\
\098\000\047\000\048\000\014\000\140\000\050\000\147\000\051\000\
\098\000\052\000\014\000\065\000\014\000\014\000\014\000\014\000\
\066\000\057\000\149\000\058\000\059\000\060\000\061\000\046\000\
\071\000\053\000\072\000\054\000\055\000\085\000\086\000\087\000\
\056\000\073\000\075\000\080\000\103\000\101\000\108\000\057\000\
\110\000\058\000\059\000\060\000\061\000\040\000\112\000\040\000\
\126\000\133\000\135\000\040\000\040\000\040\000\040\000\040\000\
\040\000\134\000\040\000\040\000\040\000\040\000\040\000\040\000\
\040\000\040\000\087\000\136\000\141\000\043\000\144\000\043\000\
\146\000\011\000\040\000\043\000\043\000\043\000\043\000\043\000\
\150\000\025\000\043\000\043\000\043\000\043\000\043\000\043\000\
\043\000\043\000\027\000\006\000\011\000\044\000\029\000\044\000\
\063\000\061\000\043\000\044\000\044\000\044\000\044\000\044\000\
\065\000\020\000\044\000\044\000\044\000\044\000\044\000\044\000\
\044\000\044\000\049\000\039\000\137\000\041\000\128\000\041\000\
\000\000\000\000\044\000\041\000\041\000\041\000\000\000\000\000\
\000\000\000\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\000\000\000\000\000\000\042\000\000\000\042\000\
\000\000\000\000\041\000\042\000\042\000\042\000\000\000\000\000\
\000\000\000\000\042\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\048\000\053\000\048\000\053\000\000\000\000\000\
\048\000\053\000\042\000\000\000\000\000\000\000\000\000\048\000\
\048\000\048\000\048\000\048\000\048\000\048\000\048\000\053\000\
\049\000\000\000\049\000\000\000\000\000\000\000\049\000\048\000\
\053\000\000\000\002\000\002\000\002\000\049\000\049\000\049\000\
\049\000\049\000\049\000\049\000\049\000\050\000\011\000\050\000\
\000\000\000\000\002\000\050\000\000\000\049\000\000\000\000\000\
\000\000\000\000\050\000\050\000\050\000\050\000\050\000\050\000\
\050\000\050\000\051\000\000\000\051\000\000\000\000\000\000\000\
\051\000\000\000\050\000\000\000\000\000\000\000\000\000\051\000\
\051\000\051\000\051\000\051\000\051\000\051\000\051\000\046\000\
\000\000\046\000\047\000\000\000\047\000\046\000\000\000\051\000\
\047\000\000\000\000\000\000\000\046\000\046\000\000\000\047\000\
\047\000\000\000\046\000\046\000\000\000\047\000\047\000\052\000\
\000\000\052\000\000\000\000\000\046\000\052\000\000\000\047\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\052\000\052\000\000\000\000\000\000\000\083\000\
\084\000\085\000\086\000\087\000\052\000\088\000\089\000\090\000\
\091\000\092\000\093\000\094\000\095\000\000\000\000\000\000\000\
\083\000\084\000\085\000\086\000\087\000\102\000\088\000\089\000\
\090\000\091\000\092\000\093\000\094\000\095\000\000\000\000\000\
\082\000\000\000\000\000\000\000\000\000\000\000\127\000\083\000\
\084\000\085\000\086\000\087\000\000\000\088\000\089\000\090\000\
\091\000\092\000\093\000\094\000\095\000\107\000\000\000\000\000\
\000\000\000\000\000\000\000\000\083\000\084\000\085\000\086\000\
\087\000\000\000\088\000\089\000\090\000\091\000\092\000\093\000\
\094\000\095\000\131\000\000\000\000\000\000\000\000\000\000\000\
\000\000\083\000\084\000\085\000\086\000\087\000\000\000\088\000\
\089\000\090\000\091\000\092\000\093\000\094\000\095\000\145\000\
\000\000\000\000\000\000\000\000\000\000\000\000\083\000\084\000\
\085\000\086\000\087\000\000\000\088\000\089\000\090\000\091\000\
\092\000\093\000\094\000\095\000\100\000\000\000\000\000\000\000\
\000\000\083\000\084\000\085\000\086\000\087\000\000\000\088\000\
\089\000\090\000\091\000\092\000\093\000\094\000\095\000\129\000\
\000\000\000\000\000\000\000\000\083\000\084\000\085\000\086\000\
\087\000\000\000\088\000\089\000\090\000\091\000\092\000\093\000\
\094\000\095\000\130\000\000\000\000\000\000\000\000\000\083\000\
\084\000\085\000\086\000\087\000\000\000\088\000\089\000\090\000\
\091\000\092\000\093\000\094\000\095\000\151\000\000\000\000\000\
\000\000\000\000\083\000\084\000\085\000\086\000\087\000\000\000\
\088\000\089\000\090\000\091\000\092\000\093\000\094\000\095\000\
\083\000\084\000\085\000\086\000\087\000\000\000\088\000\089\000\
\090\000\091\000\092\000\093\000\094\000\095\000\083\000\084\000\
\085\000\086\000\087\000\000\000\088\000\089\000\090\000\091\000\
\092\000\093\000\094\000\083\000\084\000\085\000\086\000\087\000\
\000\000\088\000\089\000\090\000\091\000\092\000\093\000\083\000\
\084\000\085\000\086\000\087\000\000\000\000\000\000\000\090\000\
\091\000\092\000\093\000"

let yycheck = "\024\000\
\000\000\000\000\051\000\065\000\129\000\130\000\076\000\006\001\
\001\001\002\001\035\000\050\000\002\001\052\000\063\000\040\000\
\006\001\056\000\016\000\144\000\033\001\034\001\035\001\013\001\
\001\000\023\000\151\000\066\000\032\001\001\001\000\000\003\001\
\071\000\072\000\073\000\007\001\034\000\076\000\108\000\078\000\
\065\000\031\001\041\001\038\001\083\000\084\000\085\000\086\000\
\087\000\088\000\089\000\090\000\091\000\092\000\093\000\094\000\
\095\000\042\001\030\001\004\001\026\001\027\001\028\001\102\000\
\002\001\135\000\004\001\005\001\006\001\108\000\030\001\002\001\
\007\001\041\001\136\000\006\001\042\001\003\001\008\001\009\001\
\010\001\011\001\012\001\004\001\022\001\037\001\024\001\025\001\
\127\000\042\001\005\001\029\001\131\000\002\001\143\000\004\001\
\135\000\006\001\036\001\001\001\038\001\039\001\040\001\041\001\
\002\001\036\001\145\000\038\001\039\001\040\001\041\001\136\000\
\002\001\022\001\002\001\024\001\025\001\010\001\011\001\012\001\
\029\001\002\001\042\001\005\001\007\001\005\001\002\001\036\001\
\038\001\038\001\039\001\040\001\041\001\001\001\041\001\003\001\
\003\001\003\001\002\001\007\001\008\001\009\001\010\001\011\001\
\012\001\007\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\012\001\004\001\003\001\001\001\023\001\003\001\
\003\001\000\000\030\001\007\001\008\001\009\001\010\001\011\001\
\005\001\003\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\003\001\005\001\037\001\001\001\005\001\003\001\
\003\001\007\001\030\001\007\001\008\001\009\001\010\001\011\001\
\003\001\008\000\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\044\000\035\000\127\000\001\001\102\000\003\001\
\255\255\255\255\030\001\007\001\008\001\009\001\255\255\255\255\
\255\255\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\255\255\255\255\255\255\001\001\255\255\003\001\
\255\255\255\255\030\001\007\001\008\001\009\001\255\255\255\255\
\255\255\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\001\001\001\001\003\001\003\001\255\255\255\255\
\007\001\007\001\030\001\255\255\255\255\255\255\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\021\001\
\001\001\255\255\003\001\255\255\255\255\255\255\007\001\030\001\
\030\001\255\255\026\001\027\001\028\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\001\001\037\001\003\001\
\255\255\255\255\042\001\007\001\255\255\030\001\255\255\255\255\
\255\255\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\001\001\255\255\003\001\255\255\255\255\255\255\
\007\001\255\255\030\001\255\255\255\255\255\255\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\001\001\
\255\255\003\001\001\001\255\255\003\001\007\001\255\255\030\001\
\007\001\255\255\255\255\255\255\014\001\015\001\255\255\014\001\
\015\001\255\255\020\001\021\001\255\255\020\001\021\001\001\001\
\255\255\003\001\255\255\255\255\030\001\007\001\255\255\030\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\020\001\021\001\255\255\255\255\255\255\008\001\
\009\001\010\001\011\001\012\001\030\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\255\255\255\255\255\255\
\008\001\009\001\010\001\011\001\012\001\030\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\255\255\255\255\
\001\001\255\255\255\255\255\255\255\255\255\255\030\001\008\001\
\009\001\010\001\011\001\012\001\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\001\001\255\255\255\255\
\255\255\255\255\255\255\255\255\008\001\009\001\010\001\011\001\
\012\001\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\001\001\255\255\255\255\255\255\255\255\255\255\
\255\255\008\001\009\001\010\001\011\001\012\001\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\001\001\
\255\255\255\255\255\255\255\255\255\255\255\255\008\001\009\001\
\010\001\011\001\012\001\255\255\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\003\001\255\255\255\255\255\255\
\255\255\008\001\009\001\010\001\011\001\012\001\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\003\001\
\255\255\255\255\255\255\255\255\008\001\009\001\010\001\011\001\
\012\001\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\003\001\255\255\255\255\255\255\255\255\008\001\
\009\001\010\001\011\001\012\001\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\003\001\255\255\255\255\
\255\255\255\255\008\001\009\001\010\001\011\001\012\001\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\008\001\009\001\010\001\011\001\012\001\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\008\001\009\001\
\010\001\011\001\012\001\255\255\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\008\001\009\001\010\001\011\001\012\001\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\008\001\
\009\001\010\001\011\001\012\001\255\255\255\255\255\255\016\001\
\017\001\018\001\019\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACK\000\
  RBRACK\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MOD\000\
  ASSIGN\000\
  EQ\000\
  NEQ\000\
  LESSER\000\
  LESSEREQ\000\
  GREATER\000\
  GREATEREQ\000\
  AND\000\
  OR\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  INT\000\
  BOOL\000\
  STR\000\
  RETURN\000\
  COMMA\000\
  DOT\000\
  CLASS\000\
  PRIVATE\000\
  PUBLIC\000\
  PROTECTED\000\
  NEW\000\
  CONS\000\
  EOF\000\
  "

let yynames_block = "\
  NUM\000\
  BLIT\000\
  STRLIT\000\
  ID\000\
  OBJECT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'class_decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 37 "parser.mly"
                        ( (_1, fst _2, snd _2) )
# 416 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
              ( [] )
# 422 "parser.ml"
               : 'class_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'class_decls) in
    Obj.repr(
# 41 "parser.mly"
                      ( _1 :: _2 )
# 430 "parser.ml"
               : 'class_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'modifier) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'decls) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'cons_list) in
    Obj.repr(
# 45 "parser.mly"
  (
    {
      cmod = _1;
      cname = _3;
      vars = fst _5;
      funcs = snd _5
    }
  )
# 447 "parser.ml"
               : 'cdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'args_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 56 "parser.mly"
  (
    {
      rtyp=_2;
      fname=_2;
      formals=_4;
      locals=_7;
      body=_8
    }
  )
# 465 "parser.ml"
               : 'cons))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cons) in
    Obj.repr(
# 67 "parser.mly"
         ( _1 )
# 472 "parser.ml"
               : 'cons_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cons) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cons_list) in
    Obj.repr(
# 68 "parser.mly"
                   ( _1 :: _2 )
# 480 "parser.ml"
               : 'cons_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
           ( Private )
# 486 "parser.ml"
               : 'modifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
          ( Public )
# 492 "parser.ml"
               : 'modifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
             ( Protected )
# 498 "parser.ml"
               : 'modifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "parser.mly"
                 ( ([], [])              )
# 504 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 78 "parser.mly"
                    ( ((_1 :: fst _3), snd _3) )
# 512 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 79 "parser.mly"
               ( (fst _2, (_1 :: snd _2)) )
# 520 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
              ( [] )
# 526 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 83 "parser.mly"
                           (  _1 :: _3 )
# 534 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "parser.mly"
           ( (_1, _2) )
# 542 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "parser.mly"
                             ( (Array(_1, _3), _5) )
# 551 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primitive) in
    Obj.repr(
# 91 "parser.mly"
              ( _1 )
# 558 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'obj) in
    Obj.repr(
# 92 "parser.mly"
        ( _1 )
# 565 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 95 "parser.mly"
         ( Obj(_1) )
# 572 "parser.ml"
               : 'obj))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
          ( Int   )
# 578 "parser.ml"
               : 'primitive))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser.mly"
          ( Bool  )
# 584 "parser.ml"
               : 'primitive))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
          ( String )
# 590 "parser.ml"
               : 'primitive))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 106 "parser.mly"
  (
    {
      rtyp=fst _1;
      fname=snd _1;
      formals=_3;
      locals=_6;
      body=_7
    }
  )
# 608 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
              ( [] )
# 614 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 119 "parser.mly"
                 ( _1 )
# 621 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 122 "parser.mly"
        ( [_1] )
# 628 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 123 "parser.mly"
                             ( _1::_3 )
# 636 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "parser.mly"
                ( [] )
# 642 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 127 "parser.mly"
                    ( _1::_2 )
# 650 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                                            ( Expr _1      )
# 657 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 131 "parser.mly"
                                            ( Block _2 )
# 664 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 134 "parser.mly"
                                            ( If(_3, _5, _7) )
# 673 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 135 "parser.mly"
                                            ( While (_3, _5)  )
# 681 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 136 "parser.mly"
                                                     ( For (_3, _5, _7, _9) )
# 691 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                                            ( Return _2      )
# 698 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 141 "parser.mly"
                     ( Num(_1)            )
# 705 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 142 "parser.mly"
                     ( BoolLit(_1)            )
# 712 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 143 "parser.mly"
                     ( StrLit(_1)            )
# 719 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 144 "parser.mly"
                     ( Id(_1)                 )
# 726 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 145 "parser.mly"
                     ( Binop(_1, Add,   _3)   )
# 734 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
                     ( Binop(_1, Sub,   _3)   )
# 742 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 147 "parser.mly"
                      ( Binop(_1, Times,   _3)   )
# 750 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                       ( Binop(_1, Divide,   _3)   )
# 758 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 149 "parser.mly"
                     ( Binop(_1, Mod, _3)     )
# 766 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 150 "parser.mly"
                     ( Binop(_1, Equal, _3)   )
# 774 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 151 "parser.mly"
                     ( Binop(_1, Neq, _3)     )
# 782 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 152 "parser.mly"
                         ( Binop(_1, Lesser,  _3)   )
# 790 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 153 "parser.mly"
                           ( Binop(_1, LesserEq,  _3)   )
# 798 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 154 "parser.mly"
                          ( Binop(_1, Greater,  _3)   )
# 806 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 155 "parser.mly"
                            ( Binop(_1, GreaterEq,  _3)   )
# 814 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 156 "parser.mly"
                     ( Binop(_1, And,   _3)   )
# 822 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 157 "parser.mly"
                     ( Binop(_1, Or,    _3)   )
# 830 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 158 "parser.mly"
                     ( Assign(_1, _3)         )
# 838 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 159 "parser.mly"
                         ( ArrayCall(_1, _3) )
# 846 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 160 "parser.mly"
                            ( ArrayLit(_2) )
# 853 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 161 "parser.mly"
                       ( _2                   )
# 860 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 163 "parser.mly"
                              ( Call (_1, _3)  )
# 868 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 164 "parser.mly"
                                     ( MethodCall(_1, _3, _5) )
# 877 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 165 "parser.mly"
                                      ( Constructor(_2, _4) )
# 885 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 168 "parser.mly"
         ( [_1] )
# 892 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 169 "parser.mly"
                         ( _1 :: _3 )
# 900 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 173 "parser.mly"
              ( [] )
# 906 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 174 "parser.mly"
         ( _1 )
# 913 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 177 "parser.mly"
        ( [_1] )
# 920 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 178 "parser.mly"
                    ( _1::_3 )
# 928 "parser.ml"
               : 'args))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
