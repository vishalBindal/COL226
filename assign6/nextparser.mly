%{
  
%}

%token NEXT END UNKNOWN
%start signal
%type <int> signal

%%
signal:
  NEXT {1}
  | END {0}
  | UNKNOWN {-1}
  ;