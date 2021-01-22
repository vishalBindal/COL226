%{
	open Printf
%}	

%token EOL  /*end of line */
%token LPAREN  /*left parenthesis [  */
%token RPAREN  /*right parenthesis ]  */
%token LBR  /*left bracket (  */
%token RBR	  /*right bracket )  */
%token COMMA COLON ASSIGN	SEMICOLON
%token <float> FLOAT
%token <int*int> I /*indices */
%token <(int*int)*(int*int)> R	 /*range */
%token SUM AVG MIN MAX COUNT ROWCOUNT COLCOUNT ROWSUM COLSUM ROWAVG COLAVG ROWMIN COLMIN ROWMAX COLMAX
%token ADD SUBT MULT DIV
%start main
%type <unit> main

%%
main: 
	expr SEMICOLON EOL	{}
	| main expr SEMICOLON EOL	{}	
	;

expr:
	 /* I := FUNC R */
	I ASSIGN COUNT R {printf " (full_count) "}
	|I ASSIGN ROWCOUNT R {printf " (row_count) "}
	|I ASSIGN COLCOUNT R {printf " (col_count) "}

	|I ASSIGN SUM R {printf " (full_sum) "}
	|I ASSIGN ROWSUM R {printf " (row_sum) "}
	|I ASSIGN COLSUM R {printf " (col_sum) "}

	|I ASSIGN AVG R {printf " (full_avg) "}
	|I ASSIGN ROWAVG R {printf " (row_avg) "}
	|I ASSIGN COLAVG R {printf " (col_avg) "}

	|I ASSIGN MIN R {printf " (full_min) "}
	|I ASSIGN ROWMIN R {printf " (row_min) "}
	|I ASSIGN COLMIN R {printf " (col_min) "}
	
	|I ASSIGN MAX R {printf " (full_max) "}
	|I ASSIGN ROWMAX R {printf " (row_max) "}
	|I ASSIGN COLMAX R {printf " (col_max) "}

	 /*I := FUNC R R */
	|I ASSIGN ADD R R {printf " (add_range) "}
	|I ASSIGN SUBT R R {printf " (subt_range) "}
	|I ASSIGN MULT R R {printf " (mult_range) "}
	|I ASSIGN DIV R R {printf " (div_range) "}

	 /*I := FUNC C R */
	|I ASSIGN ADD FLOAT R {printf " (add_const) "}
	|I ASSIGN SUBT FLOAT R {printf " (subt_const) "}
	|I ASSIGN MULT FLOAT R {printf " (mult_const) "}
	|I ASSIGN DIV FLOAT R {printf " (div_const) "}

	 /*I := FUNC R C */
	|I ASSIGN ADD R FLOAT {printf " (add_const) "}
	|I ASSIGN SUBT R FLOAT {printf " (subt_const) "}
	|I ASSIGN MULT R FLOAT {printf " (mult_const) "}
	|I ASSIGN DIV R FLOAT {printf " (div_const) "}

	 /*I := FUNC I R */
	|I ASSIGN ADD I R {printf " (add_const) "}
	|I ASSIGN SUBT I R {printf " (subt_const) "}
	|I ASSIGN MULT I R {printf " (mult_const) "}
	|I ASSIGN DIV I R {printf " (div_const) "}

	 /*I := FUNC R I */
	|I ASSIGN ADD R I {printf " (add_const) "}
	|I ASSIGN SUBT R I {printf " (subt_const) "}
	|I ASSIGN MULT R I {printf " (mult_const) "}
	|I ASSIGN DIV R I {printf " (div_const) "}
	;
