# 15 "lexerES.mll"
 
	type tokenz =
		| ParentIn
		| ParentOut
		| Not
		| And
		| Or
		| Xor
		| Impl
		| Ifoif
		| TrueFacts
		| Requests
		| Fact of char
		| NewLine of int

# 18 "lexerES.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\240\255\241\255\242\255\243\255\244\255\001\000\002\000\
    \248\255\249\255\250\255\251\255\252\255\253\255\254\255\002\000\
    \247\255\029\000\246\255\002\000\253\255\254\255\255\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\014\000\010\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_default = 
   "\002\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\255\255\000\000\021\000\000\000\000\000\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\015\000\003\000\015\000\022\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \015\000\011\000\015\000\014\000\000\000\000\000\000\000\000\000\
    \013\000\012\000\000\000\010\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\006\000\007\000\017\000\005\000\
    \016\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\018\000\000\000\000\000\008\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\009\000\000\000\000\000\000\000\
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
    \001\000\000\000\020\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\015\000\019\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\015\000\000\000\255\255\255\255\255\255\255\255\
    \000\000\000\000\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\000\000\006\000\000\000\
    \007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\017\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
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
    \000\000\255\255\019\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec tokenLex line lst lexbuf =
    __ocaml_lex_tokenLex_rec line lst lexbuf 0
and __ocaml_lex_tokenLex_rec line lst lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 34 "lexerES.mll"
                  ( tokenLex line lst lexbuf )
# 127 "lexerES.ml"

  | 1 ->
# 35 "lexerES.mll"
           ( comment line lst lexbuf )
# 132 "lexerES.ml"

  | 2 ->
# 36 "lexerES.mll"
           ( tokenLex line (lst @ [ParentIn]) lexbuf )
# 137 "lexerES.ml"

  | 3 ->
# 37 "lexerES.mll"
           ( tokenLex line (lst @ [ParentOut]) lexbuf )
# 142 "lexerES.ml"

  | 4 ->
# 38 "lexerES.mll"
           ( tokenLex line (lst @ [Not]) lexbuf )
# 147 "lexerES.ml"

  | 5 ->
# 39 "lexerES.mll"
           ( tokenLex line (lst @ [And]) lexbuf )
# 152 "lexerES.ml"

  | 6 ->
# 40 "lexerES.mll"
           ( tokenLex line (lst @ [Or]) lexbuf )
# 157 "lexerES.ml"

  | 7 ->
# 41 "lexerES.mll"
           ( tokenLex line (lst @ [Xor]) lexbuf )
# 162 "lexerES.ml"

  | 8 ->
# 42 "lexerES.mll"
            ( tokenLex line (lst @ [Impl]) lexbuf )
# 167 "lexerES.ml"

  | 9 ->
# 43 "lexerES.mll"
             ( tokenLex line (lst @ [Ifoif]) lexbuf )
# 172 "lexerES.ml"

  | 10 ->
# 44 "lexerES.mll"
           ( tokenLex line (lst @ [TrueFacts]) lexbuf )
# 177 "lexerES.ml"

  | 11 ->
# 45 "lexerES.mll"
           ( tokenLex line (lst @ [Requests]) lexbuf )
# 182 "lexerES.ml"

  | 12 ->
let
# 46 "lexerES.mll"
                    fact
# 188 "lexerES.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 46 "lexerES.mll"
                         ( tokenLex line (lst @ [(Fact fact)]) lexbuf )
# 192 "lexerES.ml"

  | 13 ->
# 47 "lexerES.mll"
            ( tokenLex (line + 1) (lst @ [(NewLine line)]) lexbuf )
# 197 "lexerES.ml"

  | 14 ->
let
# 48 "lexerES.mll"
        err
# 203 "lexerES.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 48 "lexerES.mll"
               ( raise (invalid_arg ("invalid char : " ^ (Char.escaped err))) )
# 207 "lexerES.ml"

  | 15 ->
# 49 "lexerES.mll"
           ( lst )
# 212 "lexerES.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_tokenLex_rec line lst lexbuf __ocaml_lex_state

and comment line lst lexbuf =
    __ocaml_lex_comment_rec line lst lexbuf 19
and __ocaml_lex_comment_rec line lst lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 51 "lexerES.mll"
            ( tokenLex (line + 1) (lst @ [(NewLine line)]) lexbuf )
# 224 "lexerES.ml"

  | 1 ->
# 52 "lexerES.mll"
          ( comment line lst lexbuf )
# 229 "lexerES.ml"

  | 2 ->
# 53 "lexerES.mll"
           ( lst )
# 234 "lexerES.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_comment_rec line lst lexbuf __ocaml_lex_state

;;

# 55 "lexerES.mll"
 

module Tag = struct type t = Nl | Ot end

let matchTag itm =
	match itm with
	| NewLine _		-> Tag.Nl
	| _				-> Tag.Ot

let rec printMyList ls =
	match ls with
	| [] -> print_char '\n'
	| hd::tl ->
		begin
			match hd with
			| ParentIn 		-> print_string " ( " ; printMyList tl
			| ParentOut 	-> print_string " ) " ; printMyList tl
			| Not			-> print_string " NOT" ; printMyList tl
			| Or			-> print_string " OR " ; printMyList tl
			| And			-> print_string " AND " ; printMyList tl
			| Xor			-> print_string " XOR " ; printMyList tl
			| Impl			-> print_string " => " ; printMyList tl
			| Ifoif			-> print_string " <=> " ; printMyList tl
			| TrueFacts 	-> print_string " TRUE FACTS : " ; printMyList tl
			| Requests		-> print_string " REQUEST : " ; printMyList tl
			| Fact (fx) 	-> print_string (" " ^ (Char.escaped fx) ^ " ") ; printMyList tl
			| NewLine (nl)	-> print_string ("  -- line " ^ (string_of_int nl) ^ " --\n") ; printMyList tl
		end

let cleanList lst =
	let rec loop ls rl =
		match ls with
		| [] -> rl
		| hd::tl when (matchTag hd) = Tag.Nl	-> skipNL tl (rl @ [hd])
		| hd::tl 								-> loop tl (rl @ [hd])
	and skipNL ls rl =
		match ls with
		| [] -> rl
		| hd::tl when  (matchTag hd) = Tag.Nl 	-> skipNL tl rl
		| hd::tl 					-> loop tl (rl @ [hd])
	in
	skipNL lst []



let lexExpSys chan debug verbose =
	let lexbuf = Lexing.from_channel chan in
	let returnList = cleanList (tokenLex 1 [] lexbuf) in
	if verbose || debug then
		begin
			print_string "Lexing ...";
			if debug = true
			then
				begin
					print_endline "\n\nResults :";
					printMyList returnList
				end;
			print_endline "... done"
		end;
	returnList


# 304 "lexerES.ml"
