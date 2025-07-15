[@@@ocaml.warning "-27"]


let () = print_endline "Hello, World!"

let input1 = {aaaa|

	struct User {
		#1 name str
		#2 id bytes
		#3 utags [[str -> Utag]]
	}
	
	-----------------------
	-- this is a comment --
	-----------------------
	struct Utag {
		#1 tag str
		#2 data optional str 
		#3-4 reserved
		#5 t_create date
		#6 t_effect optional date 
		#7 creator optional User
	}

	struct User {
		#1 name optional str
		#2 id optional str
		#3 recent_contact optional [str]
		#4 config optional [str -> str]
		#5 tags optional [[str -> str]]
		#6 tags optional [tuple (#1 str ; #2 str)]
		#7 rich_tags optional [struct { #1 tag str ; #2 data optional str; }]
		#8 friends optional [User]
		#9 friends optional [str -> User]
		#10 status optional enum {
			#1
		}
	}
	
|aaaa}

let main1 () = 
	let print_it (t, d) = print_endline @@ Nslib.Tokenizer.Token.fprinttoken t; 
	(* print_endline @@ "                              " ^ Nslib.Tokenizer.Token.fprintsourceinfo d;
	print_endline "" *) in
	List.map print_it @@ List.rev @@ Nslib.Tokenizer.tokenizer input1 0 []
	
let input2 = {aaaa|

-- this is a 'map type literal'
[str -> str]

|aaaa}

let main2 () = 
	let tokens = Nslib.Tokenizer.tokenizer input2 0 [] in
	let expr = Nslib.Parser.fparse_consume_type_expr 