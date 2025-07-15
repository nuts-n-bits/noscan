[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-21"]

module Token = Tokenizer.Token
module Keyowrd = Tokenizer.Token.Keyword

let peek tokens_array n = if n < Array.length tokens_array then Some tokens_array.(n) else None 
let (let*) x f = Option.bind x f 

module TypeExpr = struct

	type field =
		| FieldReserve of { frange: int * int; srctokens: Token.token list; }
		| Field of { fnumber: int; fname: string; ftype: type_expr; srctokens: Token.token list; }
	and tuple_field = 
		| FieldReserve of { frange: int * int; srctokens: Token.token list; }
		| Field of { fnumber: int; ftype: type_expr; optional: bool; srctokens: Token.token list;  }
	and type_expr =
		| MapLiteral of type_expr * type_expr * Token.token list
		| ListLiteral of type_expr * Token.token list
		| StructAnon of field list * Token.token list
		| EnumAnon of field list * Token.token list
		| TupleAnon of tuple_field list * Token.token list
		| PrimI8 of Token.token list
		| PrimI16 of Token.token list
		| PrimI32 of Token.token list
		| PrimI64 of Token.token list
		| PrimI128 of Token.token list
		| PrimU8 of Token.token list
		| PrimU16 of Token.token list
		| PrimU32 of Token.token list
		| PrimU64 of Token.token list
		| PrimU128 of Token.token list
		| PrimF32 of Token.token list
		| PrimF64 of Token.token list
		| PrimStr of Token.token list
		| PrimBytes of Token.token list
		| PrimBool of Token.token list
		| PrimDate of Token.token list
		| PrimDuration of Token.token list
		| Ident of string * Token.token list
end

let rec fparse_consume_type_expr tokens cursor = 
	(* Try match primitive type expression *)
	match fparse_primitive_type_expr tokens cursor with Some result -> Some result | _ -> 
	match fparse_list_expr tokens cursor with Some result -> Some result | _ -> 
	match fparse_map_expr tokens cursor with Some result -> Some result | _ -> 
	let cur_token = peek tokens cursor in 
	match cur_token with 
	| Some { token = Token.Identifier ident; src_info = src_info } -> Some (TypeExpr.Ident (ident, []), 1)
	| _ -> None

and fparse_primitive_type_expr (tokens: Token.token array) cursor = 
	match peek tokens cursor with 
	| Some (Token.Identifier "i8"      , srcloc) -> Some (TypeExpr.PrimI8       , [srcloc] , 1)
	| Some (Token.Identifier "i16"     , srcloc) -> Some (TypeExpr.PrimI16      , [srcloc] , 1)
	| Some (Token.Identifier "i32"     , srcloc) -> Some (TypeExpr.PrimI32      , [srcloc] , 1)
	| Some (Token.Identifier "i64"     , srcloc) -> Some (TypeExpr.PrimI64      , [srcloc] , 1)
	| Some (Token.Identifier "i128"    , srcloc) -> Some (TypeExpr.PrimI128     , [srcloc] , 1)
	| Some (Token.Identifier "u8"      , srcloc) -> Some (TypeExpr.PrimU8       , [srcloc] , 1)
	| Some (Token.Identifier "u16"     , srcloc) -> Some (TypeExpr.PrimU16      , [srcloc] , 1)
	| Some (Token.Identifier "u32"     , srcloc) -> Some (TypeExpr.PrimU32      , [srcloc] , 1)
	| Some (Token.Identifier "u64"     , srcloc) -> Some (TypeExpr.PrimU64      , [srcloc] , 1)
	| Some (Token.Identifier "u128"    , srcloc) -> Some (TypeExpr.PrimU128     , [srcloc] , 1)
	| Some (Token.Identifier "f32"     , srcloc) -> Some (TypeExpr.PrimF32      , [srcloc] , 1)
	| Some (Token.Identifier "f64"     , srcloc) -> Some (TypeExpr.PrimF64      , [srcloc] , 1)
	| Some (Token.Identifier "date"    , srcloc) -> Some (TypeExpr.PrimDate     , [srcloc] , 1)
	| Some (Token.Identifier "bool"    , srcloc) -> Some (TypeExpr.PrimBool     , [srcloc] , 1)
	| Some (Token.Identifier "str"     , srcloc) -> Some (TypeExpr.PrimStr      , [srcloc] , 1)
	| Some (Token.Identifier "bytes"   , srcloc) -> Some (TypeExpr.PrimBytes    , [srcloc] , 1)
	| Some (Token.Identifier "duration", srcloc) -> Some (TypeExpr.PrimDuration , [srcloc] , 1)
	| _ -> None

and hparse_consume_token tokens cursor token = 
	match peek tokens cursor with
	| Some t when t = token -> Some 1
	| _ -> None


and hparse_consume_ident tokens cursor = 
	match peek tokens cursor with 
	| Some Token.Identifier ident -> Some (ident, 1)
	| _ -> None

and hparse_consume_number tokens cursor = 
	match peek tokens cursor with
	| Some Token.Number num -> Some (num, 1)
	| _ -> None

and hparse_consume_keyword_opt tokens cursor expected_kw = 
	match peek tokens cursor with 
	| Some Token.Keyword kw when kw = expected_kw -> 1
	| _ -> 0

and hparse_consume_field tokens cursor acc = 
	let old_cursor = cursor in 
	let* advance = hparse_consume_token tokens cursor Token.Pound in
	let cursor = cursor + advance in
	let* (number, advance) = hparse_consume_number tokens cursor in
	let cursor = cursor + advance in 
	match peek tokens cursor with 
	| Some Token.Dash -> (
		let cursor = cursor + 1 in
		let* (number2, advance) = hparse_consume_number tokens cursor in
		let cursor = cursor + advance in
		let* advance = hparse_consume_keyword tokens cursor Keyowrd.Reserved in
		let cursor = cursor + advance in
		let* number = int_of_string_opt number in
		let* number2 = int_of_string_opt number2 in
		Some (TypeExpr.FieldReserve { frange = (number, number2) }, cursor - old_cursor)
	)
	| Some Token.Identifier id -> (
		let cursor = cursor + 1 in
		let advance = hparse_consume_keyword_opt tokens cursor Keyowrd.Optional in
		let field_is_optional = advance = 1 in
		let cursor = cursor + advance in
		let* (type_expr, advance) = fparse_consume_type_expr tokens cursor in
		let cursor = cursor + advance in
		let* number = int_of_string_opt number in
		Some (TypeExpr.Field { fnumber = number; optional = field_is_optional; ftype = type_expr }, cursor - old_cursor)
	)
	| Some Token.Keyword Keyowrd.Reserved -> (
		let cursor = cursor + 1 in
		let* number = int_of_string_opt number in
		Some (TypeExpr.FieldReserve { frange = (number, number) }, cursor - old_cursor)
	)
	| _ -> None 


and hparse_consume_keyword tokens cursor expected_keyword = 
	match peek tokens cursor with 
	| Some Token.Keyword kw when kw = expected_keyword -> Some 1
	| _ -> None

and fparse_map_expr tokens cursor = 
	let old_cursor = cursor in
	let* advance = hparse_consume_token tokens cursor Token.Lbracket in 
	let cursor = cursor + advance in
	let* (lhs_type, lhs_srcloclist, advance) = fparse_consume_type_expr tokens cursor in
	let cursor = cursor + advance in
	let* advance = hparse_consume_token tokens cursor Token.Arrow in
	let cursor = cursor + advance in
	let* (rhs_type, advance) = fparse_consume_type_expr tokens cursor in 
	let cursor = cursor + advance in
	let* advance = hparse_consume_token tokens cursor Token.Rbracket in
	let cursor = cursor + advance in
	Some (TypeExpr.MapLiteral (lhs_type, rhs_type), [], cursor - old_cursor)

and fparse_list_expr tokens cursor = 
	let old_cursor = cursor in
	let* advance = hparse_consume_token tokens cursor Token.Lbracket in 
	let cursor = cursor + advance in
	let* (inner_type, advance) = fparse_consume_type_expr tokens cursor in
	let cursor = cursor + advance in
	let* advance = hparse_consume_token tokens cursor Token.Rbracket in
	let cursor = cursor + advance in
	Some (TypeExpr.ListLiteral inner_type, cursor - old_cursor)

and fparse_struct_anon_expr tokens cursor = 
	let old_cursor = cursor in
	let* advance = hparse_consume_keyword tokens cursor Keyowrd.Struct in 
	let cursor = cursor + advance in
	let* advance = hparse_consume_token tokens cursor Token.Lbrace in 
	let cursor = cursor + advance in 
	raise @@ Failure "unimplemented!!"; 
	let* (inner_type, advance) = fparse_consume_type_expr tokens cursor in
	let cursor = cursor + advance in
	let* advance = hparse_consume_token tokens cursor Token.Rbracket in
	let cursor = cursor + advance in
	Some (TypeExpr.ListLiteral inner_type, cursor - old_cursor)
	

