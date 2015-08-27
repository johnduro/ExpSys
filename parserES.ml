

(* ********** SERVICES ********** *)

(* Parenthese | Facts | Operateur | NewLine *)
module Tag = struct type t = Ps | Nt | Fs | Op | Nl end

let matchTag value =
	match value with
	| LexerES.ParentIn
	| LexerES.ParentOut -> Tag.Ps
	| LexerES.Not
	| LexerES.Or
	| LexerES.And
	| LexerES.Xor
	| LexerES.Impl
	| LexerES.Ifoif
	| LexerES.TrueFacts
	| LexerES.Requests	-> Tag.Op
	| LexerES.Fact _	-> Tag.Fs
	| LexerES.NewLine _	-> Tag.Nl

let opValue value =
	match value with
	| LexerES.Not	-> 4
	| LexerES.And	-> 3
	| LexerES.Or	-> 2
	| LexerES.Xor	-> 1
	| _				-> 0


exception ParsingExcp of string


let getLine nl =
	match nl with
	| LexerES.NewLine (line)	-> line
	| _							-> 0

let rec getPosition lst =
	match lst with
	| [] 									-> " the last line"
	| hd::tl when (matchTag hd) = Tag.Nl	-> (" line " ^ (string_of_int (getLine hd)))
	| hd::tl								-> getPosition tl

module ParsingError =
	struct
		let raiseParsingExp s =
			ParsingExcp s
		let exp err lst =
			let position = getPosition lst in
			raiseParsingExp ("Parsing error : " ^ err ^ position)
		let expi err line =
			raiseParsingExp ("Parsing error : " ^ err ^ " line " ^ (string_of_int line))
		let expr err (r1, l1, s1) (r2, l2, s2) =
			raiseParsingExp ("Parsing error : " ^ err ^ " between line " ^ (string_of_int l1) ^ " and line " ^ (string_of_int l2))
	end

let getFact f1 =
	match f1 with
	| LexerES.Fact fct -> ExpSys.Expertsys.Value fct
	| _ -> raise (ParsingError.expi "getFact failed" 0)

let rec extractFacts e =
	match e with
	| ExpSys.Expertsys.Not (exp)		-> extractFacts exp
	| ExpSys.Expertsys.And (e1, e2)		-> (extractFacts e1) @ (extractFacts e2)
	| ExpSys.Expertsys.Or (e1, e2)		-> (extractFacts e1) @ (extractFacts e2)
	| ExpSys.Expertsys.Xor (e1, e2)		-> (extractFacts e1) @ (extractFacts e2)
	| ExpSys.Expertsys.Value v			-> [ExpSys.Expertsys.Value v]

(* let rec notIn el lst = *)
(* 	match lst with *)
(* 	| []					-> true *)
(* 	| hd::tl when hd = el	-> false *)
(* 	| hd::tl				-> notIn el tl *)

let tokensToString lst =
	let tokenToChar tk add =
		match tk with
		| LexerES.ParentIn	-> "("
		| LexerES.ParentOut -> ")" ^ add
		| LexerES.Not		-> "!"
		| LexerES.Or		-> "|" ^ add
		| LexerES.And		-> "+" ^ add
		| LexerES.Xor		-> "^" ^ add
		| LexerES.Impl		-> "=>" ^ add
		| LexerES.Ifoif		-> "<=>" ^ add
		(* | LexerES.TrueFacts	->  *)
		(* | LexerES.Requests	-> Tag.Op *)
		| LexerES.Fact ff	-> (Char.escaped ff) ^ add
		| _					-> "wut?!"
		(* | LexerES.NewLine _	-> Tag.Nl *)
	in
	let rec loop ls ret =
		match ls with
		| [] -> ret
		| hd::[] -> loop [] (ret ^ (tokenToChar hd ""))
		| hd::tl -> loop tl (ret ^ (tokenToChar hd " "))
	in
	loop lst ""

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
		| []														-> raise (ParsingError.exp "operator at the end of line in" [])
		| hd::tl when (matchTag hd) = Tag.Op && hd != LexerES.Not	-> raise (ParsingError.exp "operator following another" tl)
		| hd::tl when (matchTag hd) = Tag.Nl						-> raise (ParsingError.exp "operator alone at the end of" tl)
		| hd::tl when hd = LexerES.ParentOut						-> raise (ParsingError.exp "operator alone at the end of parenthesis" tl)
		| hd::tl													-> loop ls
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

let checkParsing (parsedRules, facts, queries) =
	let rec isSameFactList f1 f2 =
		match f1 with
		| [] -> true
		(* | hd::tl when (notIn hd f2) -> false *)
		| hd::tl when (Utils.notIn hd f2) -> false
		| hd::tl -> isSameFactList tl f2
	in
	let getLeftExpr (rl, line, str) =
		match rl with
		| ExpSys.Expertsys.Impl (e1, e2) -> e1
		(* | ExpSys.Expertsys.Ifoif (e1, e2) -> e1 *)
	in
	let getRightFacts (rl, line, str) =
		match rl with
		| ExpSys.Expertsys.Impl (e1, e2) -> extractFacts e2
		(* | ExpSys.Expertsys.Ifoif (e1, e2) -> extractFacts e2 *)
	in
	let checkRule rule lst =
		let leftExpr = getLeftExpr rule in
		let rightsFacts = getRightFacts rule in
		let rec loop ls =
			match ls with
			| [] -> ()
			| hd::tl when leftExpr = (getLeftExpr hd) && (isSameFactList rightsFacts (getRightFacts hd)) -> raise (ParsingError.expr "conflict, two rules have the same left expression and it impact the same facts" rule hd)
			| hd::tl -> loop tl
		in
		loop lst
	in
	let rec loopRules rules =
		match rules with
		| [] -> ()
		| hd::tl -> checkRule hd tl; loopRules tl
	in
	loopRules parsedRules

(* ******************************************************************************** *)

(* ********** PARSING ********** *)

(* PARSE UNE RULE SEULE *)
let parseSingleRule (lst, line) =
	let getExpr ls =
		let rec skipParenthesis ll =
			match ll with
			| []									-> raise (ParsingError.expi "no matching parenthesis for ( in" line)
			| hd::tl when hd = LexerES.ParentIn 	-> skipParenthesis (skipParenthesis tl)
			| hd::tl when hd = LexerES.ParentOut 	-> tl
			| hd::tl								-> skipParenthesis tl
		in
		let returnTwoFirst ll =
			match ll with
			| []			-> raise (ParsingError.expi "error occured while parsing" line)
			| hd::[]		-> raise (ParsingError.expi "error occured while parsing" line)
			| fs::sn::tl	-> (fs, sn)
		in
		let popTwoFirst ll =
			match ll with
			| []			-> raise (ParsingError.expi "error occured while parsing" line)
			| hd::[]		-> raise (ParsingError.expi "error occured while parsing" line)
			| fs::sn::tl	-> tl
		in
		let rec resolveOp stkVal stkOp nbOp =
			match nbOp with
			| 0 -> stkVal
			| _ ->
				begin
					match (List.hd stkOp) with
					| op when op = LexerES.Not	-> resolveOp ((ExpSys.Expertsys.Not (List.hd stkVal))::(List.tl stkVal)) (List.tl stkOp) (nbOp - 1)
					| op when op = LexerES.And	-> resolveOp ((ExpSys.Expertsys.And ((fst (returnTwoFirst stkVal)), (snd (returnTwoFirst stkVal))))::(popTwoFirst stkVal)) (List.tl stkOp) (nbOp - 1)
					| op when op = LexerES.Or	-> resolveOp ((ExpSys.Expertsys.Or ((fst (returnTwoFirst stkVal)), (snd (returnTwoFirst stkVal))))::(popTwoFirst stkVal)) (List.tl stkOp) (nbOp - 1)
					| op when op = LexerES.Xor	-> resolveOp ((ExpSys.Expertsys.Xor ((fst (returnTwoFirst stkVal)), (snd (returnTwoFirst stkVal))))::(popTwoFirst stkVal)) (List.tl stkOp) (nbOp - 1)
					| _							-> raise (ParsingError.expi "error occured while parsing" line)
				end
		in
		let rec loopExpr ll stkVal stkOp =
			match ll with
			| [] when (List.length stkVal) > 0 																												-> List.hd (resolveOp stkVal stkOp (List.length stkOp))
			| []																																			-> raise(ParsingError.expi "error occured while parsing" line)
			| hd::tl when hd = LexerES.ParentIn 																											-> loopExpr (skipParenthesis tl) ([(loopExpr tl [] [])] @ stkVal) stkOp
			| hd::tl when hd = LexerES.ParentOut																											-> List.hd (resolveOp stkVal stkOp (List.length stkOp))
			| hd::tl when (matchTag hd) = Tag.Fs																											-> loopExpr tl ((getFact hd)::stkVal) stkOp
			| hd::tl when (matchTag hd) = Tag.Op && (((List.length stkOp) = 0) || ((List.length stkOp) > 0 && (opValue hd) > (opValue (List.hd stkOp))))	-> loopExpr tl stkVal (hd::stkOp)
			| hd::tl when (matchTag hd) = Tag.Op && (List.length stkOp) > 0 && (opValue hd) <= (opValue (List.hd stkOp))									-> loopExpr ll (resolveOp stkVal stkOp 1) (List.tl stkOp)
			| hd::tl																																		-> raise (ParsingError.expi "error occured while parsing" line)
		in
		(* CHECK SI RESTE A CAUSE DES PARENTHESES >> RENVOYER UN TUPLE AVEC LE RESTE DE LA LISTE ??*)
		loopExpr ls [] []
	in
	let rec loop ls tmp rest =
		match ls with
		| []	-> raise (ParsingError.expi "statement with no effect (no '=>' or '<=>')" line)
		(* | hd::tl when hd = LexerES.Impl		-> (ExpSys.Expertsys.Impl ((getExpr tmp), (getExpr tl)), line) *)
		| hd::tl when hd = LexerES.Impl		-> (ExpSys.Expertsys.Impl ((getExpr tmp), (getExpr tl)), line, (tokensToString rest))
		(* | hd::tl when hd = LexerES.Ifoif	-> (ExpSys.Expertsys.Ifoif ((getExpr tmp), (getExpr tl)), line, (tokensToString rest)) *)
		| hd::tl							-> loop tl (tmp @ [hd]) rest
		(* | hd::tl							-> loop tl (tmp @ [hd]) *)
	in
	(* loop lst [] *)
	loop lst [] lst



(* PARCOURS LES RULES ET AJOUTES TOUS LES FAIT A UNE LISTE DE FALSE FACTS *)
let addFalseFacts (parsedRules, (ExpSys.Expertsys.Facts (trueFacts, falseFacts)), queries) =
	let addFF e1 e2 ff =
		let rec loop lst ret =
			match lst with
			| []													-> ret
			(* | hd::tl when (notIn hd trueFacts) && (notIn hd ret)	-> loop tl (ret @ [hd]) *)
			| hd::tl when (Utils.notIn hd trueFacts) && (Utils.notIn hd ret)	-> loop tl (ret @ [hd])
			| hd::tl												-> loop tl ret
		in
		let e1Fact = extractFacts e1 in
		let e2Fact = extractFacts e2 in
		let tmp1 = loop e1Fact ff in
		let tmp2 = loop e2Fact tmp1 in
		loop queries tmp2
	in
	let rec loop pRules ret =
		match pRules with
		| [] -> ret
		| (hd, line, str)::tl ->
			begin
				match hd with
				| ExpSys.Expertsys.Impl (e1, e2)  -> loop tl (addFF e1 e2 ret)
				(* | ExpSys.Expertsys.Ifoif (e1, e2)  -> loop tl (addFF e1 e2 ret) *)
			end
	in
	let fFacts = loop parsedRules [] in
	(parsedRules, (ExpSys.Expertsys.Facts (trueFacts, fFacts)), queries)



(* PARSE LA LISTE DE TOKENS ENVOYEE EN ARGUMENT AVEC LE RETOUR SUIVANT *)
(* (RULES (RULE * NB_LINE)) * (FACTS (TRUE * FALSE)) * QUERIES *)
let parseList tokenList =
	let rec getRules lst ret tmp =
		match lst with
		| []									-> ret
		| hd::tl when (matchTag hd) = Tag.Nl	-> getRules tl (ret @ [(tmp, getLine hd)]) []
		| hd::tl								-> getRules tl ret (tmp @ [hd])
	in
	let rec isOfType (lst, line) tp =
		match lst with
		| []									-> false
		| hd::tl when hd = tp					-> true
		| hd::tl								-> isOfType (tl, line) tp
	in
	let rec addFactsToList (lst, line) ret =
		match lst with
		| []									-> ret
		| hd::tl when (matchTag hd) = Tag.Fs	-> addFactsToList (tl, line) (ret @ [(getFact hd)])
		| hd::tl								-> addFactsToList (tl, line) ret
	in
	let rec parseRules rulez (parsedRules, (ExpSys.Expertsys.Facts (trueFacts, falseFacts)), queries) =
		match rulez with
		| []											-> (parsedRules, (ExpSys.Expertsys.Facts (trueFacts, falseFacts)), queries)
		| hd::tl when (isOfType hd LexerES.TrueFacts)	-> parseRules tl (parsedRules, (ExpSys.Expertsys.Facts ((addFactsToList hd []), falseFacts)), queries)
		| hd::tl when (isOfType hd LexerES.Requests)	-> parseRules tl (parsedRules, (ExpSys.Expertsys.Facts (trueFacts, falseFacts)), (addFactsToList hd []))
		| hd::tl										-> parseRules tl ((parsedRules @ [(parseSingleRule hd)]), (ExpSys.Expertsys.Facts (trueFacts, falseFacts)), queries)
	in
	let rules = getRules tokenList [] [] in
	let pRules = parseRules rules ([], (ExpSys.Expertsys.Facts ([], [])), []) in
	addFalseFacts pRules


(* PRINT LE RESULTAT DU PARSING *)
let printPL (parsedRules, facts, queries) =
	(* let printParsedRules (expr, line) = *)
	let printParsedRules (expr, line, str) =
		let stringOfRule exp =
			match exp with
			| ExpSys.Expertsys.Impl (e1, e2) -> ((ExpSys.Expertsys.stringOfExpr e1) ^ " => " ^ (ExpSys.Expertsys.stringOfExpr e2))
			(* | ExpSys.Expertsys.Ifoif (e1, e2) -> ((ExpSys.Expertsys.stringOfExpr e1) ^ " <=> " ^ (ExpSys.Expertsys.stringOfExpr e2)) *)
			(* | _ -> ("failed to print rule line " ^ (string_of_int line)) *)
		in
		print_endline ("Rule line " ^ (string_of_int line) ^ " { " ^ str ^ " } ");
		print_endline (stringOfRule expr)
	in
	let rec loopPR ll =
		match ll with
		| [] -> ()
		| hd::tl -> printParsedRules hd; loopPR tl
	in
	let rec printQueries qr =
		match qr with
		| []		-> print_char '\n'
		| hd::tl	-> print_string ((ExpSys.Expertsys.stringOfExpr hd) ^ " "); printQueries tl
	in
	print_endline "**************";
	print_endline "PARSED RULES :";
	loopPR parsedRules;
	print_endline "*****";
	print_endline "FACTS";
	ExpSys.Expertsys.printFacts facts;
	print_endline "*********";
	print_endline "QUERIES :";
	printQueries queries



(* A + B | C + D + E | F *)
(* (A + B) | ((C + D) + E) | F *)

(* ETAPES : *)
(* 	2 stacks : 1 operateur, 1 valeur ; quand un operateur a un valeur inferieure ou egale a celui au sommet *)
(* 	de la stack d'operateur , ce dernier est execute et son resultat est mis au sommet de la stack de valeur *)

(* == A | B | C + D + E | F == *)
(* A B C D     [*]   A B (C + D) E     [*]    A B ((C + D) + E) F     [*]    (A | (B | (((C + D) + E) | F))) *)
(* | | +       [*]   | | +             [*]    | | |                   [*] *)

(* A B  [*] (A | B) C D  [*]  (A | B) (C + D) E  [*]  (A | B) ((C + D) + E)   [*]   ((A | B) | ((C + D) + E)) F *)
(* |    [*] | +          [*]  | +                [*]  |                       [*]   | *)

(* A | B | C + D + E | F *)
(* (A | B) | ((C + D) + E) | F *)


(* == A | B + !C == *)
(* A B C   [*]  (A | (B + !C)) *)
(* | + !   [*] *)


(* == A | B + !C | D == *)
(* A B C   [*]  (A | (B + !C)) D   [*]  ((A | (B + !C)) | D) *)
(* | + !   [*]  |                  [*] *)

(* A | B | C | D | E | F *)
(* ((((A | B) | C) | D) | E) | F *)


let parseExpSys tokenList =
	checkList tokenList;
	let parsedlist = parseList tokenList in  (* LISTE DE RETOUR = (RULES (RULE * NB_LINE * ORIGINAL_STR)) * (FACTS (TRUE * FALSE)) * QUERIES *)
	printPL parsedlist;
	checkParsing parsedlist;
	parsedlist
	(* print_endline "WUT ?!" *)
