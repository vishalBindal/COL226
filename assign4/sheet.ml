let inchannel = open_in Sys.argv.(4) 

let _ =
  try
    let lexbuf = Lexing.from_channel inchannel in
    while true do
      let result = Parser.main Tokenize.token lexbuf in
        flush stdout
    done
  with Tokenize.Eof ->
    exit 0