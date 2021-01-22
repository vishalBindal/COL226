%{
	open Printf
	open Functions
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
	expr SEMICOLON	{}
	| main expr SEMICOLON	{}
	| main EOL {}	
	;

expr:
	 /* I := FUNC R */
	I ASSIGN COUNT R {let sheet = full_count mysheet $4 $1 in printsheet sheet}
	|I ASSIGN ROWCOUNT R {let sheet = row_count mysheet $4 $1 in printsheet sheet}
	|I ASSIGN COLCOUNT R {let sheet = col_count mysheet $4 $1 in printsheet sheet}

	|I ASSIGN SUM R {let sheet = full_sum mysheet $4 $1 in printsheet sheet}
	|I ASSIGN ROWSUM R {let sheet = row_sum mysheet $4 $1 in printsheet sheet}
	|I ASSIGN COLSUM R {let sheet = col_sum mysheet $4 $1 in printsheet sheet}

	|I ASSIGN AVG R {let sheet = full_avg mysheet $4 $1 in printsheet sheet}
	|I ASSIGN ROWAVG R {let sheet = row_avg mysheet $4 $1 in printsheet sheet}
	|I ASSIGN COLAVG R {let sheet = col_avg mysheet $4 $1 in printsheet sheet}

	|I ASSIGN MIN R {let sheet = full_min mysheet $4 $1 in printsheet sheet}
	|I ASSIGN ROWMIN R {let sheet = row_min mysheet $4 $1 in printsheet sheet}
	|I ASSIGN COLMIN R {let sheet = col_min mysheet $4 $1 in printsheet sheet}
	
	|I ASSIGN MAX R {let sheet = full_max mysheet $4 $1 in printsheet sheet}
	|I ASSIGN ROWMAX R {let sheet = row_max mysheet $4 $1 in printsheet sheet}
	|I ASSIGN COLMAX R {let sheet = col_max mysheet $4 $1 in printsheet sheet}

	 /*I := FUNC R R */
	|I ASSIGN ADD R R {let sheet = add_range mysheet $4 $5 $1 in printsheet sheet}
	|I ASSIGN SUBT R R {let sheet = subt_range mysheet $4 $5 $1 in printsheet sheet}
	|I ASSIGN MULT R R {let sheet = mult_range mysheet $4 $5 $1 in printsheet sheet}
	|I ASSIGN DIV R R {let sheet = div_range mysheet $4 $5 $1 in printsheet sheet}

	 /*I := FUNC C R */
	|I ASSIGN ADD FLOAT R {let sheet = add_const mysheet $5 $4 $1 in printsheet sheet}
	|I ASSIGN SUBT FLOAT R {let sheet = subt_const mysheet $5 $4 $1 in printsheet sheet}
	|I ASSIGN MULT FLOAT R {let sheet = mult_const mysheet $5 $4 $1 in printsheet sheet}
	|I ASSIGN DIV FLOAT R {let sheet = div_const mysheet $5 $4 $1 in printsheet sheet}

	 /*I := FUNC R C */
	|I ASSIGN ADD R FLOAT {let sheet = add_const mysheet $4 $5 $1 in printsheet sheet}
	|I ASSIGN SUBT R FLOAT {let sheet = subt_const mysheet $4 $5 $1 in printsheet sheet}
	|I ASSIGN MULT R FLOAT {let sheet = mult_const mysheet $4 $5 $1 in printsheet sheet}
	|I ASSIGN DIV R FLOAT {let sheet = div_const mysheet $4 $5 $1 in printsheet sheet}

	 /*I := FUNC I R */
	|I ASSIGN ADD I R {let sheet = add_index mysheet $5 $4 $1 in printsheet sheet}
	|I ASSIGN SUBT I R {let sheet = subt_index mysheet $5 $4 $1 in printsheet sheet}
	|I ASSIGN MULT I R {let sheet = mult_index mysheet $5 $4 $1 in printsheet sheet}
	|I ASSIGN DIV I R {let sheet = div_index mysheet $5 $4 $1 in printsheet sheet}

	 /*I := FUNC R I */
	|I ASSIGN ADD R I {let sheet = add_index mysheet $4 $5 $1 in printsheet sheet}
	|I ASSIGN SUBT R I {let sheet = subt_index mysheet $4 $5 $1 in printsheet sheet}
	|I ASSIGN MULT R I {let sheet = mult_index mysheet $4 $5 $1 in printsheet sheet}
	|I ASSIGN DIV R I {let sheet = div_index mysheet $4 $5 $1 in printsheet sheet}
	;
