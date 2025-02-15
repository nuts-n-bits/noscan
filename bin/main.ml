[@@@ocaml.warning "-27"]


let () = print_endline "Hello, World!"

let input = {aaaa|

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

let _ = 
	let printf (t, d) = print_endline @@ Ns.Tokenizer.Token.fprinttoken t; (*print_endline @@ Ns.Tokenizer.Token.fprintsourceinfo d*) in
	List.map printf @@ List.rev @@ Ns.Tokenizer.tokenizer input 0 []
	