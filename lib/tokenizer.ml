[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-69"]

module Token = struct 

	module Keyword = struct
		type keyword = 
			| Struct
			| Enum
			| Tuple
			| Optional
			| Reserved
		let try_from str = 
			match str with
			| "struct" -> Some Struct
			| "enum" -> Some Enum
			| "tuple" -> Some Tuple
			| "optional" -> Some Optional
			| "reserved" -> Some Reserved
			| _ -> None
		let fprintkeyword = function
			| Struct -> "Struct"
			| Enum -> "Enum"
			| Tuple -> "Tuple"
			| Optional -> "Optional"
			| Reserved -> "Reserved"
	end

	type token = 
		| Pound
		| Dash
		| Lbracket
		| Rbracket
		| Lparen
		| Rparen
		| Lbrace
		| Rbrace
		| Semicolon
		| Arrow
		| Keyword of Keyword.keyword
		| Number of string
		| Identifier of string

	type token_source_info = {
		byte_start: int;
		byte_end: int;
	}


	let fprinttoken = function 
		| Pound -> "Token.Pound"
		| Dash -> "Token.Dash"
		| Lbracket -> "Token.Lbracket"
		| Rbracket -> "Token.Rbracket"
		| Lparen -> "Token.Lparen"
		| Rparen -> "Token.Rparen"
		| Lbrace -> "Token.Lbrace"
		| Rbrace -> "Token.Rbrace"
		| Semicolon -> "Token.Semicolon"
		| Arrow -> "Token.Arrow"
		| Keyword keyword -> "Token.Keyword " ^ Keyword.fprintkeyword keyword
		| Number string -> "Token.Number " ^ string
		| Identifier string -> "Token.Identifier " ^ string

	let source_info bs be = { byte_start = bs; byte_end = be }
	let fprintsourceinfo info = Printf.sprintf "{ byte_start = %d ; byte_end = %d }" info.byte_start info.byte_end
end


let get_opt s n = if n < String.length s && n >= 0 then Some s.[n] else None
let char_in_range lower upper ch = ch >= lower && ch <= upper
let is_start_ident ch = char_in_range 'a' 'z' ch || char_in_range 'A' 'Z' ch || ch == '_'
let is_cont_ident ch = char_in_range '0' '9' ch || is_start_ident ch
let rec eat_ident source cursor acc = 
	match get_opt source cursor with 
	| Some ch when is_cont_ident ch -> eat_ident source (cursor+1) (ch :: acc) 
	| _ -> acc
let rec eat_num source cursor acc = 
	match get_opt source cursor with
	| Some ch when char_in_range '0' '9' ch -> eat_num source (cursor+1) (ch::acc)
	| _ -> acc
let rec eat_line_comment source cursor acc =
	match get_opt source cursor with 
	| Some ch when ch != '\n' -> eat_line_comment source (cursor+1) (acc+1)
	| _ -> acc

let rec tokenizer source cursor acc = 
	let peek n = get_opt source @@ cursor + n in
	let next_token, bytes_advanced = match peek 0 with 
	| Some '\n' | Some ' ' | Some '\t' | Some '\r' -> (None, 1)
	| Some '#' -> (Some Token.Pound, 1)
	| Some '-' -> (
		match peek 1 with 
		| Some '>' -> (Some Token.Arrow, 2) 
		| Some '-' -> (None, eat_line_comment source cursor 0)
		| _ -> (Some Token.Dash, 1)
	)
	| Some '[' -> (Some Token.Lbracket, 1)
	| Some ']' -> (Some Token.Rbracket, 1)
	| Some '{' -> (Some Token.Lbrace, 1)
	| Some '}' -> (Some Token.Rbrace, 1)
	| Some '(' -> (Some Token.Lparen, 1)
	| Some ')' -> (Some Token.Rparen, 1)
	| Some ';' -> (Some Token.Semicolon, 1)
	| Some ch when is_start_ident ch -> (
		let ident = eat_ident source cursor [] in 
		let ident = String.of_seq @@ List.to_seq @@ List.rev ident in
		match Token.Keyword.try_from ident with 
		| Some keyword -> (Some (Token.Keyword keyword), String.length ident)
		| None -> (Some (Token.Identifier ident), String.length ident)
	)
	| Some '0'..'9' -> (
		let num = eat_num source cursor [] in
		let num = String.of_seq @@ List.to_seq @@ List.rev num in
		(Some (Token.Number num), String.length num)
	)
	| Some _ch -> (raise @@ Failure ("unrecognized character: [" ^ String.make 1 _ch ^ "]"))
	| None -> (None, 0) in
	match next_token, bytes_advanced with
	| (Some token, length) -> tokenizer source (cursor+length) ((token, Token.source_info cursor @@ cursor + length)::acc)
	| (None, 0) -> acc
	| (None, length) -> tokenizer source (cursor+length) acc


