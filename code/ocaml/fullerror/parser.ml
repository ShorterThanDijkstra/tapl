type token =
  | IMPORT of (Support.Error.info)
  | TBOT of (Support.Error.info)
  | LAMBDA of (Support.Error.info)
  | TTOP of (Support.Error.info)
  | IF of (Support.Error.info)
  | THEN of (Support.Error.info)
  | ELSE of (Support.Error.info)
  | TRUE of (Support.Error.info)
  | FALSE of (Support.Error.info)
  | BOOL of (Support.Error.info)
  | ERROR of (Support.Error.info)
  | TRY of (Support.Error.info)
  | OTHERWISE of (Support.Error.info)
  | TYPE of (Support.Error.info)
  | UCID of (string Support.Error.withinfo)
  | LCID of (string Support.Error.withinfo)
  | INTV of (int Support.Error.withinfo)
  | FLOATV of (float Support.Error.withinfo)
  | STRINGV of (string Support.Error.withinfo)
  | APOSTROPHE of (Support.Error.info)
  | DQUOTE of (Support.Error.info)
  | ARROW of (Support.Error.info)
  | BANG of (Support.Error.info)
  | BARGT of (Support.Error.info)
  | BARRCURLY of (Support.Error.info)
  | BARRSQUARE of (Support.Error.info)
  | COLON of (Support.Error.info)
  | COLONCOLON of (Support.Error.info)
  | COLONEQ of (Support.Error.info)
  | COLONHASH of (Support.Error.info)
  | COMMA of (Support.Error.info)
  | DARROW of (Support.Error.info)
  | DDARROW of (Support.Error.info)
  | DOT of (Support.Error.info)
  | EOF of (Support.Error.info)
  | EQ of (Support.Error.info)
  | EQEQ of (Support.Error.info)
  | EXISTS of (Support.Error.info)
  | GT of (Support.Error.info)
  | HASH of (Support.Error.info)
  | LCURLY of (Support.Error.info)
  | LCURLYBAR of (Support.Error.info)
  | LEFTARROW of (Support.Error.info)
  | LPAREN of (Support.Error.info)
  | LSQUARE of (Support.Error.info)
  | LSQUAREBAR of (Support.Error.info)
  | LT of (Support.Error.info)
  | RCURLY of (Support.Error.info)
  | RPAREN of (Support.Error.info)
  | RSQUARE of (Support.Error.info)
  | SEMI of (Support.Error.info)
  | SLASH of (Support.Error.info)
  | STAR of (Support.Error.info)
  | TRIANGLE of (Support.Error.info)
  | USCORE of (Support.Error.info)
  | VBAR of (Support.Error.info)

open Parsing;;
let _ = parse_error;;
# 7 "parser.mly"
open Support.Error
open Support.Pervasive
open Syntax
# 66 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* IMPORT *);
  258 (* TBOT *);
  259 (* LAMBDA *);
  260 (* TTOP *);
  261 (* IF *);
  262 (* THEN *);
  263 (* ELSE *);
  264 (* TRUE *);
  265 (* FALSE *);
  266 (* BOOL *);
  267 (* ERROR *);
  268 (* TRY *);
  269 (* OTHERWISE *);
  270 (* TYPE *);
  271 (* UCID *);
  272 (* LCID *);
  273 (* INTV *);
  274 (* FLOATV *);
  275 (* STRINGV *);
  276 (* APOSTROPHE *);
  277 (* DQUOTE *);
  278 (* ARROW *);
  279 (* BANG *);
  280 (* BARGT *);
  281 (* BARRCURLY *);
  282 (* BARRSQUARE *);
  283 (* COLON *);
  284 (* COLONCOLON *);
  285 (* COLONEQ *);
  286 (* COLONHASH *);
  287 (* COMMA *);
  288 (* DARROW *);
  289 (* DDARROW *);
  290 (* DOT *);
    0 (* EOF *);
  291 (* EQ *);
  292 (* EQEQ *);
  293 (* EXISTS *);
  294 (* GT *);
  295 (* HASH *);
  296 (* LCURLY *);
  297 (* LCURLYBAR *);
  298 (* LEFTARROW *);
  299 (* LPAREN *);
  300 (* LSQUARE *);
  301 (* LSQUAREBAR *);
  302 (* LT *);
  303 (* RCURLY *);
  304 (* RPAREN *);
  305 (* RSQUARE *);
  306 (* SEMI *);
  307 (* SLASH *);
  308 (* STAR *);
  309 (* TRIANGLE *);
  310 (* USCORE *);
  311 (* VBAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\004\000\004\000\
\006\000\008\000\008\000\008\000\008\000\008\000\007\000\007\000\
\003\000\003\000\003\000\003\000\003\000\009\000\009\000\010\000\
\010\000\010\000\010\000\010\000\005\000\005\000\000\000"

let yylen = "\002\000\
\001\000\003\000\002\000\001\000\002\000\002\000\002\000\002\000\
\001\000\003\000\001\000\001\000\001\000\001\000\003\000\001\000\
\001\000\006\000\006\000\006\000\004\000\001\000\002\000\003\000\
\001\000\001\000\001\000\001\000\000\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\026\000\027\000\028\000\
\000\000\000\000\000\000\001\000\000\000\031\000\000\000\004\000\
\000\000\022\000\003\000\000\000\000\000\025\000\000\000\000\000\
\000\000\006\000\000\000\000\000\005\000\000\000\000\000\023\000\
\000\000\000\000\000\000\000\000\011\000\012\000\013\000\014\000\
\000\000\030\000\009\000\000\000\007\000\008\000\024\000\002\000\
\000\000\000\000\000\000\021\000\000\000\000\000\000\000\000\000\
\000\000\010\000\015\000\018\000\019\000\020\000"

let yydgoto = "\002\000\
\014\000\015\000\016\000\029\000\026\000\042\000\043\000\044\000\
\017\000\018\000"

let yysindex = "\005\000\
\001\000\000\000\244\254\245\254\029\255\000\000\000\000\000\000\
\029\255\231\254\246\254\000\000\029\255\000\000\217\254\000\000\
\010\255\000\000\000\000\253\254\001\255\000\000\023\255\020\255\
\012\255\000\000\012\255\029\255\000\000\243\254\001\000\000\000\
\012\255\012\255\029\255\029\255\000\000\000\000\000\000\000\000\
\012\255\000\000\000\000\014\255\000\000\000\000\000\000\000\000\
\005\255\015\255\041\255\000\000\011\255\012\255\029\255\029\255\
\029\255\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\016\255\004\255\000\000\000\000\000\000\000\000\000\000\
\252\254\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\029\000\000\000\251\255\000\000\000\000\030\000\007\000\000\000\
\000\000\045\000"

let yytablesize = 300
let yytable = "\023\000\
\012\000\017\000\017\000\024\000\020\000\001\000\019\000\030\000\
\017\000\025\000\031\000\025\000\025\000\037\000\025\000\038\000\
\027\000\006\000\007\000\025\000\008\000\039\000\046\000\033\000\
\028\000\022\000\040\000\034\000\035\000\051\000\052\000\004\000\
\036\000\005\000\047\000\054\000\006\000\007\000\055\000\008\000\
\009\000\016\000\021\000\017\000\022\000\017\000\025\000\057\000\
\056\000\060\000\061\000\062\000\013\000\025\000\041\000\016\000\
\045\000\016\000\058\000\048\000\059\000\032\000\049\000\050\000\
\000\000\029\000\000\000\000\000\000\000\000\000\053\000\013\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\000\000\004\000\000\000\005\000\000\000\000\000\
\006\000\007\000\000\000\008\000\009\000\000\000\000\000\010\000\
\011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\013\000"

let yycheck = "\005\000\
\000\000\006\001\007\001\009\000\016\001\001\000\019\001\013\000\
\013\001\035\001\050\001\008\001\009\001\002\001\011\001\004\001\
\027\001\008\001\009\001\016\001\011\001\010\001\028\000\027\001\
\035\001\016\001\015\001\027\001\006\001\035\000\036\000\003\001\
\013\001\005\001\048\001\022\001\008\001\009\001\034\001\011\001\
\012\001\034\001\054\001\048\001\016\001\050\001\043\001\007\001\
\034\001\055\000\056\000\057\000\043\001\050\001\043\001\048\001\
\027\000\050\001\048\001\031\000\054\000\017\000\033\000\034\000\
\255\255\050\001\255\255\255\255\255\255\255\255\041\000\043\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\255\255\003\001\255\255\005\001\255\255\255\255\
\008\001\009\001\255\255\011\001\012\001\255\255\255\255\015\001\
\016\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\043\001"

let yynames_const = "\
  "

let yynames_block = "\
  IMPORT\000\
  TBOT\000\
  LAMBDA\000\
  TTOP\000\
  IF\000\
  THEN\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  BOOL\000\
  ERROR\000\
  TRY\000\
  OTHERWISE\000\
  TYPE\000\
  UCID\000\
  LCID\000\
  INTV\000\
  FLOATV\000\
  STRINGV\000\
  APOSTROPHE\000\
  DQUOTE\000\
  ARROW\000\
  BANG\000\
  BARGT\000\
  BARRCURLY\000\
  BARRSQUARE\000\
  COLON\000\
  COLONCOLON\000\
  COLONEQ\000\
  COLONHASH\000\
  COMMA\000\
  DARROW\000\
  DDARROW\000\
  DOT\000\
  EOF\000\
  EQ\000\
  EQEQ\000\
  EXISTS\000\
  GT\000\
  HASH\000\
  LCURLY\000\
  LCURLYBAR\000\
  LEFTARROW\000\
  LPAREN\000\
  LSQUARE\000\
  LSQUAREBAR\000\
  LT\000\
  RCURLY\000\
  RPAREN\000\
  RSQUARE\000\
  SEMI\000\
  SLASH\000\
  STAR\000\
  TRIANGLE\000\
  USCORE\000\
  VBAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 113 "parser.mly"
      ( fun ctx -> [],ctx )
# 329 "parser.ml"
               :  Syntax.context -> (Syntax.command list * Syntax.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Syntax.context -> (Syntax.command list * Syntax.context) ) in
    Obj.repr(
# 115 "parser.mly"
      ( fun ctx ->
          let cmd,ctx = _1 ctx in
          let cmds,ctx = _3 ctx in
          cmd::cmds,ctx )
# 341 "parser.ml"
               :  Syntax.context -> (Syntax.command list * Syntax.context) ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 122 "parser.mly"
                   ( fun ctx -> (Import(_2.v)),ctx )
# 349 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 124 "parser.mly"
      ( fun ctx -> (let t = _1 ctx in Eval(tmInfo t,t)),ctx )
# 356 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Binder) in
    Obj.repr(
# 126 "parser.mly"
      ( fun ctx -> ((Bind(_1.i,_1.v,_2 ctx)), addname ctx _1.v) )
# 364 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string Support.Error.withinfo) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'TyBinder) in
    Obj.repr(
# 128 "parser.mly"
      ( fun ctx -> ((Bind(_1.i, _1.v, _2 ctx)), addname ctx _1.v) )
# 372 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 133 "parser.mly"
      ( fun ctx -> VarBind (_2 ctx))
# 380 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 135 "parser.mly"
      ( fun ctx -> TmAbbBind(_2 ctx, None) )
# 388 "parser.ml"
               : 'Binder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 140 "parser.mly"
                ( _1 )
# 395 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 145 "parser.mly"
           ( _2 )
# 404 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 147 "parser.mly"
      ( fun ctx -> TyBot )
# 411 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 149 "parser.mly"
      ( fun ctx -> TyTop )
# 418 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 151 "parser.mly"
      ( fun ctx -> TyBool )
# 425 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 153 "parser.mly"
      ( fun ctx ->
          TyVar(name2index _1.i ctx _1.v, ctxlength ctx) )
# 433 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'AType) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ArrowType) in
    Obj.repr(
# 160 "parser.mly"
     ( fun ctx -> TyArr(_1 ctx, _3 ctx) )
# 442 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 162 "parser.mly"
            ( _1 )
# 449 "parser.ml"
               : 'ArrowType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 166 "parser.mly"
      ( _1 )
# 456 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string Support.Error.withinfo) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 168 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx _2.v in
          TmAbs(_1, _2.v, _4 ctx, _6 ctx1) )
# 470 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Type) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 172 "parser.mly"
      ( fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs(_1, "_", _4 ctx, _6 ctx1) )
# 484 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 176 "parser.mly"
      ( fun ctx -> TmIf(_1, _2 ctx, _4 ctx, _6 ctx) )
# 496 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 178 "parser.mly"
      ( fun ctx -> TmTry(_1, _2 ctx, _4 ctx) )
# 506 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 182 "parser.mly"
      ( _1 )
# 513 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'AppTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 184 "parser.mly"
      ( fun ctx ->
          let e1 = _1 ctx in
          let e2 = _2 ctx in
          TmApp(tmInfo e1,e1,e2) )
# 524 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 192 "parser.mly"
      ( _2 )
# 533 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 194 "parser.mly"
      ( fun ctx ->
          TmVar(_1.i, name2index _1.i ctx _1.v, ctxlength ctx) )
# 541 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 197 "parser.mly"
      ( fun ctx -> TmTrue(_1) )
# 548 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 199 "parser.mly"
      ( fun ctx -> TmFalse(_1) )
# 555 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 201 "parser.mly"
      ( fun ctx -> TmError(_1) )
# 562 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 205 "parser.mly"
      ( fun ctx -> TyVarBind )
# 568 "parser.ml"
               : 'TyBinder))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Type) in
    Obj.repr(
# 207 "parser.mly"
      ( fun ctx -> TyAbbBind(_2 ctx) )
# 576 "parser.ml"
               : 'TyBinder))
(* Entry toplevel *)
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
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  Syntax.context -> (Syntax.command list * Syntax.context) )
