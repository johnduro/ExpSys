

(* ********** SERVICES ********** *)

(* Parenthese | Facts | Operateur | NewLine *)
module Tag = struct type t = Ps | Nt | Fs | Op | Nl end

let matchTag value =
	match value with
	| LexerES.ParentIn
	| LexerES.ParentOut -> Tag.Ps
	| LexerES.Not		-> Tag.Nt
	| LexerES.Or
	| LexerES.And
	| LexerES.Xor
	| LexerES.Impl
	| LexerES.Ifoif
	| LexerES.TrueFacts
	| LexerES.Requests	-> Tag.Op
	| LexerES.Fact _	-> Tag.Fs
	| LexerES.NewLine _	-> Tag.Nl

exception ParsingExcp of string


let getLine nl =
	match nl with
	(* | LexerES.NewLine (line)	-> (string_of_int line) *)
	| LexerES.NewLine (line)	-> line
	(* | _							-> "wut!?" *)
	| _							-> 0

let rec getPosition lst =
	match lst with
	| [] 									-> " the last line"
	(* | hd::tl when (matchTag hd) = Tag.Nl	-> (" line " ^ (getLine hd)) *)
	| hd::tl when (matchTag hd) = Tag.Nl	-> (" line " ^ (string_of_int (getLine hd)))
	| hd::tl								-> getPosition tl

module ParsingError =
	struct
		let exp err lst =
			let position = getPosition lst in
			ParsingExcp ("Parsing error : " ^ err ^ position)
		let expi err line =
			ParsingExcp ("Parsing error : " ^ err ^ " line " ^ (string_of_int line))
	end

(* ******************************************************************************** *)

(* ********** VERIFICATIONS ********** *)

let checkList lst =
	let rec loop ls =
		match ls with
		| [] -> []
		| hd::tl when hd = LexerES.TrueFacts || hd = LexerES.Requests	-> checkFactList tl
		| hd::tl when hd = LexerES.ParentIn 							-> loop (checkParenthesis tl tl)
		| hd::tl when hd = LexerES.ParentOut 							-> ls
		| hd::tl when (matchTag hd) = Tag.Op							-> checkOperator tl
		| hd::tl when hd = LexerES.Not									-> checkNot tl
		| hd::tl when (matchTag hd) = Tag.Fs							-> checkFact tl
		| hd::tl 														-> loop tl
	and checkParenthesis ls endLst =
		match ls with
		| []									-> raise (ParsingError.exp "no matching parenthesis for ( in" endLst)
		| hd::tl when hd = LexerES.ParentIn		-> checkParenthesis (checkParenthesis ls endLst) endLst
		| hd::tl when hd = LexerES.ParentOut	-> tl
		| hd::tl								-> checkParenthesis (loop tl) endLst
	and checkOperator ls =
		match ls with
		| []									-> raise (ParsingError.exp "operator at the end of line in" [])
		| hd::tl when (matchTag hd) = Tag.Op	-> raise (ParsingError.exp "operator following another" tl)
		| hd::tl when (matchTag hd) = Tag.Nl	-> raise (ParsingError.exp "operator alone at the end of" tl)
		| hd::tl when hd = LexerES.ParentOut	-> raise (ParsingError.exp "operator alone at the end of parenthesis" tl)
		| hd::tl								-> loop ls
	and checkNot ls =
		match ls with
		| []															-> raise (ParsingError.exp "Not (!) with no expression following in" [])
		| hd::tl when (matchTag hd) = Tag.Fs || hd = LexerES.ParentIn	-> loop ls
		| hd::tl														-> raise (ParsingError.exp "Not (!) is not followed by a fact or an expression" tl)
	and checkFact ls =
		match ls with
		| []									-> []
		| hd::tl when (matchTag hd) = Tag.Op	-> checkOperator tl
		| hd::tl when (matchTag hd) = Tag.Nl	-> loop tl
		| hd::tl when hd = LexerES.ParentIn		-> raise (ParsingError.exp "fact followed by a openning parenthese" tl)
		| hd::tl when (matchTag hd) = Tag.Fs	-> raise (ParsingError.exp "fact followed by another fact" tl)
		| hd::tl when hd = LexerES.Not			-> raise (ParsingError.exp "fact followed by Not (!) operator" tl)
		| hd::tl								-> loop ls
	and checkFactList ls =
		match ls with
		| [] -> []
		| hd::tl when (matchTag hd) = Tag.Fs	-> checkFactList tl
		| hd::tl when (matchTag hd) = Tag.Nl	-> loop tl
		| hd::tl								-> raise (ParsingError.exp "wrong value after true facts or request" tl)
	in
	let finalCheck ls =
		match ls with
		| [] 		-> ()
		| hd::tl 	-> raise (ParsingError.exp "no matching parenthesis for )" tl)
	in
	finalCheck (loop lst)

(* ******************************************************************************** *)

(* ********** PARSING ********** *)

(* A + B | C + D + E | F *)
(* (A + B) | ((C + D) + E) | F *)

(* A | B | C | D | E | F *)
(* ((((A | B) | C) | D) | E) | F *)

(* (RULES (RULE * NB_LINE)) * (FACTS (TRUE * FALSE)) * QUERIES *)

let parseList tokenList =
	let getFact (LexerES.Fact fct) = (ExpSys.Expertsys.Value fct) in
	let rec getRules lst ret tmp =
		match lst with
		| []									-> ret
		| hd::tl when (matchTag hd) = Tag.Nl	-> getRules tl (ret @ (tmp, getLine hd)) []
		| hd::tl								-> getRules tl ret (tmp @ hd)
	in
	let rec isInitialFacts (lst, line) =
		match lst with
		| []									-> false
		| hd::tl when hd = LexerES.TrueFacts	-> true
		| hd::tl								-> isInitialFacts (tl, line)
	in
	let rec isQueries (lst, line) =
		match lst with
		| []									-> false
		| hd::tl when hd = LexerES.Requests		-> true
		| hd::tl								-> isQueries (tl, line)
	in
	let rec addInitialFacts (lst, line) (retTrue, falseFacts) =
		(* let addRemoveFacts (LexerES.Fact fc) (tf, ff) = *)
		let addRemoveFacts newFact (tf, ff) =
			let rec removeFact nf ff ret =
				match ff with
				| []					-> ret
				| hd::tl when hd = nf	-> removeFact nf tl ret
				| hd::tl				-> removeFact nf tl (ret @ hd)
			in
			(* let newFact = (ExpSys.Expertsys.Value fc) in *)
			((tf @ newFact), (removeFact newFact ff []))
		in
		match lst with
		| []									-> (retTrue, falseFacts)
		(* | hd::tl when (matchTag hd) = Tag.Fs	-> addInitialFacts (tl, line) (addRemoveFacts hd (retTrue, falseFacts)) *)
		| hd::tl when (matchTag hd) = Tag.Fs	-> addInitialFacts (tl, line) (addRemoveFacts (getFact hd) (retTrue, falseFacts))
		| hd::tl								-> addInitialFacts (tl, line) (retTrue, falseFacts)
	in
	let rec addQueries (lst, line) ret =
		match lst with
		| []									-> ret
		| hd::tl when (matchTag hd) = Tag.Fs	-> addQueries (tl, line) (ret @ (getFact hd))
		| hd::tl								-> addQueries (tl, line) ret
	in
	let parseSingleRule (lst, line) =
		let rec getExpr ls =
			(* A FAIRE !!!!!! *)
		in
		let rec loop ls tmp =
			match ls with
			| []	-> raise (ParsingError.expi "statement with no effect (no '=>' or '<=>')" line)
			| hd::tl when hd = LexerES.Impl		-> (ExpSys.Expertsys.Impl ((getExpr tmp), (getExpr tl)), line)
			(* | hd::tl when hd = LexerES.Ifoif	-> (ExpSys.Expertsys.Ifoif ((getExpr tmp), (getExpr tl)), line) *)
			| hd::tl							-> loop tl (tmp @ hd)
		in
		loop lst []
	in
	let rec parseRules rulez (parsedRules, (trueFacts, falseFacts), queries) =
		match rulez with
		| []								-> ret
		| hd::tl when (isInitialFacts hd)	-> parseRules tl (parsedRules, (addInitialFacts hd ([], falseFacts)), queries)
		| hd::tl when (isQueries hd)		-> parseRules tl (parsedRules, (trueFacts, falseFacts), (addQueries hd []))
		| hd::tl							-> parseRules tl ((parsedRules @ (parseSingleRule hd)), (trueFacts, falseFacts), queries)
	in
	let rules = getRules tokenList [] [] in
	parseRules rules ([], ([], []), []) (* LISTE DE RETOUR = (RULES (RULE * NB_LINE)) * (FACTS (TRUE * FALSE)) * QUERIES *)


let parseExpSys tokenList =
	checkList tokenList;
	let parsedlist = parseList tokenList in
	(* checkParsing parsedList; *)
	(* parsedList *)
	print_endline "WUT ?!"
