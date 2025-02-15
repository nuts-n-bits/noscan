[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-21"]

module Token = Ns.Tokenizer.Token
module Keyowrd = Ns.Tokenizer.Token.Keyword

let peek tokens_array n = if n < Array.length tokens_array then Some tokens_array.(n) else None 
let (let*) x f = Option.bind x f 

module TypeExpr = struct
	type field =
		| FieldReserve of { frange: int * int }
		| Field of { fnumber: int; fname: string; ftype: type_expr; }
	and tuple_field = 
		| FieldReserve of { frange: int * int }
		| Field of { fnumber: int; ftype: type_expr; optional: bool; }
	and type_expr =
		| MapLiteral of type_expr * type_expr
		| ListLiteral of type_expr
		| StructAnon of field list
		| EnumAnon of field list
		| TupleAnon of tuple_field list
		| PrimI8
		| PrimI16
		| PrimI32
		| PrimI64
		| PrimI128
		| PrimU8
		| PrimU16
		| PrimU32
		| PrimU64
		| PrimU128
		| PrimF32
		| PrimF64
		| PrimStr
		| PrimBytes
		| PrimBool
		| PrimDate
		| PrimDuration
		| Ident of string
end

let rec fparse_consume_type_expr tokens cursor = 
	(* Try match primitive type expression *)
	match fparse_primitive_type_expr tokens cursor with Some result -> Some result | _ -> 
	match fparse_list_expr tokens cursor with Some result -> Some result | _ -> 
	match fparse_map_expr tokens cursor with Some result -> Some result | _ -> 
	
	match peek tokens cursor with 
	| Some Token.Identifier ident -> Some (TypeExpr.Ident ident, 1)
	| _ -> None

and fparse_primitive_type_expr tokens cursor = 
	match peek tokens cursor with 
	| Some Token.Identifier "i8"       -> Some (TypeExpr.PrimI8       , 1)
	| Some Token.Identifier "i16"      -> Some (TypeExpr.PrimI16      , 1)
	| Some Token.Identifier "i32"      -> Some (TypeExpr.PrimI32      , 1)
	| Some Token.Identifier "i64"      -> Some (TypeExpr.PrimI64      , 1)
	| Some Token.Identifier "i128"     -> Some (TypeExpr.PrimI128     , 1)
	| Some Token.Identifier "u8"       -> Some (TypeExpr.PrimU8       , 1)
	| Some Token.Identifier "u16"      -> Some (TypeExpr.PrimU16      , 1)
	| Some Token.Identifier "u32"      -> Some (TypeExpr.PrimU32      , 1)
	| Some Token.Identifier "u64"      -> Some (TypeExpr.PrimU64      , 1)
	| Some Token.Identifier "u128"     -> Some (TypeExpr.PrimU128     , 1)
	| Some Token.Identifier "f32"      -> Some (TypeExpr.PrimF32      , 1)
	| Some Token.Identifier "f64"      -> Some (TypeExpr.PrimF64      , 1)
	| Some Token.Identifier "date"     -> Some (TypeExpr.PrimDate     , 1)
	| Some Token.Identifier "bool"     -> Some (TypeExpr.PrimBool     , 1)
	| Some Token.Identifier "str"      -> Some (TypeExpr.PrimStr      , 1)
	| Some Token.Identifier "bytes"    -> Some (TypeExpr.PrimBytes    , 1)
	| Some Token.Identifier "duration" -> Some (TypeExpr.PrimDuration , 1)
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
	let* (lhs_type, advance) = fparse_consume_type_expr tokens cursor in
	let cursor = cursor + advance in
	let* advance = hparse_consume_token tokens cursor Token.Arrow in
	let cursor = cursor + advance in
	let* (rhs_type, advance) = fparse_consume_type_expr tokens cursor in 
	let cursor = cursor + advance in
	let* advance = hparse_consume_token tokens cursor Token.Rbracket in
	let cursor = cursor + advance in
	Some (TypeExpr.MapLiteral (lhs_type, rhs_type), cursor - old_cursor)

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
	

