let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.main Tokenize.token lexbuf in
        flush stdout
    done
  with Tokenize.Eof ->
    exit 0