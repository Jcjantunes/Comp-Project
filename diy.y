%{
/* $Id: diy.y,v 1.0 2019/02/06 17:25:13 prs Exp $ */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "node.h"
#include "tabid.h"


void variable(char*,Node*,Node*,int,int), function_decl(char*,int,Node*,Node*), extrn_func(char *name), non_void(char* name, int non_void_func);
extern int yylex();
extern int yyparse();
int yyerror(char *s);
void declare(int pub, int cnst, Node *type, Node *value);
void enter(int typ, char *name);
int checkargs(char *name, Node *args);
int nostring(Node *arg1, Node *arg2);
int intonly(Node *arg);
int noassign(Node *arg1, Node *arg2);
static int ncicl = 0, pos = 0;
static char *fpar;

int extrnFlag = 0;
int retPos = 0;
%}

%union {
	int i;			/* integer value */
	double r;		/* real value */
	char *s;		/* symbol name or string literal */
	Node *n;		/* node pointer */
};

%token <i> INT
%token <r> REAL
%token <s> ID STR
%token DO WHILE IF THEN FOR IN UPTO DOWNTO STEP BREAK CONTINUE
%token VOID INTEGER STRING NUMBER CONST PUBLIC INCR DECR
%nonassoc IFX
%nonassoc ELSE

%right ATR
%left '|'
%left '&'
%nonassoc '~'
%left '=' NE
%left GE LE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc UMINUS '!' NOT REF
%nonassoc '[' '('

%type <n> tipo init finit blocop params 
%type <n> bloco decls param base stmt step args list end brk lv expr
%type <i> ptr intp public

%token LOCAL POSINC POSDEC PTR CALL START PARAM NIL
%%

file: /*vazio*/          										                                       
	| file error ';' 											
	| file public tipo ID ';'									{ IDnew($3->value.i, $4, 0); declare($2, 0, $3, 0); variable($4,$3,0, 0,$2);}
	| file public CONST tipo ID ';'								{ IDnew($4->value.i+5, $5, 0); declare($2, 1, $4, 0); variable($5,$4, 0, 1,$2);}
	| file public tipo ID init									{ IDnew($3->value.i, $4, 0); declare($2, 0, $3, $5); variable($4, $3, $5, 0,$2);}
	| file public CONST tipo ID init							{ IDnew($4->value.i+5, $5, 0); declare($2, 1, $4, $6); variable($5, $4, $6, 1,$2);}
	| file public tipo ID 										{ enter($3->value.i, $4); } finit { function($4, $6); non_void($4,1); if(extrnFlag == 0){function_decl($4, -pos, $6,$3);} else {extrn_func($4); extrnFlag = 0;}}
	| file public VOID ID 										{ enter(4, $4); } finit { function($4, $6); non_void($4,0); if(extrnFlag == 0){function_decl($4, -pos, $6,0);} else {extrn_func($4); extrnFlag = 0;}}
	;

public: /*vazio*/              									{ $$ = 0; }
	  | PUBLIC        											{ $$ = 1; }
	  ;

ptr: /*vazio*/              									{ $$ = 0; }
   | '*'           												{ $$ = 10; }
   ;

tipo: INTEGER ptr												{ $$ = intNode(INTEGER, 1+$2); }
	| STRING ptr												{ $$ = intNode(STRING, 2+$2); }
	| NUMBER ptr												{ $$ = intNode(NUMBER, 3+$2); }
	;

init: ATR ID ';'												{ $$ = strNode(ID, $2); $$->info = IDfind($2, 0) + 10; }
	| ATR INT ';'												{ $$ = intNode(INT, $2); $$->info = 1; }
	| ATR '-' INT ';'											{ $$ = intNode(INT, -$3); $$->info = 1; }
	| ATR STR ';'												{ $$ = strNode(STR, $2); $$->info = 2; }
	| ATR CONST STR ';'											{ $$ = strNode(CONST, $3); $$->info = 2+5; }
	| ATR REAL ';'												{ $$ = realNode(REAL, $2); $$->info = 3; }
	| ATR '-' REAL ';'											{ $$ = realNode(REAL, -$3); $$->info = 3; }
    ;

finit: '(' { pos = 8; } params ')' { pos = retPos; } blocop 	{ $$ = binNode('(', $6, $3); }
	 | '(' ')' { pos = 0; } blocop        						{ $$ = binNode('(', $4, nilNode(NIL)); }
	 ;

blocop: ';' { IDpop(); extrnFlag = 1;}   						{ $$ = nilNode(NIL); }
      | bloco ';' { IDpop(); }  					 		  	{ $$ = $1; }
      ;

params: param 													
	  | params ',' param      				   					{ $$ = binNode(',', $1, $3); }
	  ;

bloco: '{' { IDpush(); } decls list end '}'  { $$ = binNode('{', $4 ? binNode(';', $4, $5) : $4, $3); IDpop(); }
	 ;

decls: /*vazio*/                      							{ $$ = nilNode(NIL); }
	| decls param ';'       									{ $$ = binNode(';', $1, $2); }
	;

param: tipo ID               									{ $$ = binNode(PARAM, $1, strNode(ID, $2));

																  if($1->value.i % 5 == 1 || $1->value.i % 5 == 2) {
																  	if(pos > 0) {
																  		IDnew($1->value.i, $2, pos);
																  		pos+=4;
																  	}
																  	else {
																  		pos-=4;
																  		IDnew($1->value.i, $2, pos);
																  	}
																  }

																  if($1->value.i % 5 == 3) {
																  	if(pos > 0) {
																  		IDnew($1->value.i, $2, pos);
																  		pos+=8;
																  	}
																  	else { 
																  		pos-=8;
																  		IDnew($1->value.i, $2, pos);
																  	}
																  }

                                  				  				  if (IDlevel() == 1) fpar[(unsigned)++fpar[0]] = $1->value.i;
                                								}
	 ;

stmt: base
	| brk
	;

base: ';'                   									{ $$ = nilNode(VOID); }
	| DO { ncicl++; } stmt WHILE expr ';' 						{ $$ = binNode(WHILE, binNode(DO, nilNode(START), $3), $5); ncicl--; }
	| FOR lv IN expr UPTO expr step DO { ncicl++; } stmt       	{ $$ = binNode(';', binNode(ATR, $4, $2), binNode(FOR, binNode(IN, nilNode(START), binNode(LE, uniNode(PTR, $2), $6)), binNode(';', $10, binNode(ATR, binNode('+', uniNode(PTR, $2), $7), $2)))); ncicl--; }
	| FOR lv IN expr DOWNTO expr step DO { ncicl++; } stmt      { $$ = binNode(';', binNode(ATR, $4, $2), binNode(FOR, binNode(IN, nilNode(START), binNode(GE, uniNode(PTR, $2), $6)), binNode(';', $10, binNode(ATR, binNode('-', uniNode(PTR, $2), $7), $2)))); ncicl--; }
	| IF expr THEN stmt %prec IFX    							{ $$ = binNode(IF, $2, $4); }
	| IF expr THEN stmt ELSE stmt    							{ $$ = binNode(ELSE, binNode(IF, $2, $4), $6); }
	| expr ';'              									{ $$ = $1; }
	| bloco                 									{ $$ = $1; }
	| lv '#' expr ';'       									{ $$ = binNode('#', $3, $1); }
	| error ';'       											{ $$ = nilNode(NIL); }
	;

end: /*vazio*/													{ $$ = nilNode(NIL); }
	| brk
	;

brk : BREAK intp ';'        									{ $$ = intNode(BREAK, $2); if ($2 <= 0 || $2 > ncicl) yyerror("invalid break argument"); }
	| CONTINUE intp ';'     									{ $$ = intNode(CONTINUE, $2); if ($2 <= 0 || $2 > ncicl) yyerror("invalid continue argument"); }
	;

step: /*vazio*/              									{ $$ = intNode(INT, 1); }
	| STEP expr     											{ $$ = $2; }
	;

intp:       													{ $$ = 1; }
	| INT
	;

list: base
	| list base     											{ $$ = binNode(';', $1, $2); }
	;

args: expr														{ $$ = binNode(',', $1, nilNode(NIL)); }
	| args ',' expr 											{ $$ = binNode(',', $3, $1); }
	;

lv: ID															{ long pos; int typ = IDfind($1, &pos);
																  /*printf("%ld\n",pos);*/
																  /*printf("%s %d\n",$1,typ);*/
                          										  if (pos == 0) $$ = strNode(ID, $1);
                          										  else $$ = intNode(LOCAL, pos);
			  													  $$->info = typ;
																}
	| ID '[' expr ']' 											{ Node *n;
                            									  long pos; int typ = IDfind($1, &pos);
                            									  if (typ / 10 != 1 && typ % 5 != 2) yyerror("not a pointer");
                            									  if (pos == 0) n = strNode(ID, $1);
                            									  else n = intNode(LOCAL, pos);
                            									  n->info = typ;
                            									  $$ = binNode('[', n, $3);
			    												  if (typ >= 10) typ -= 10;
                            									  else if (typ % 5 == 2) typ = 1;
			    												  if (typ >= 5) typ -= 5;
			    												  $$->info = typ;
			    												  /*printf("typ:%d info:%d\n",typ,n->info);*/
			  													}
	;

expr: lv														{ $$ = uniNode(PTR, $1); $$->info = $1->info; }
	| '*' lv        											{ $$ = uniNode(PTR, uniNode(PTR, $2)); if ($2->info % 5 == 2) $$->info = 1; else if ($2->info / 10 == 1) $$->info = $2->info % 10; else yyerror("can dereference lvalue"); }
	| lv ATR expr   											{ $$ = binNode(ATR, $3, $1); if ($$->info % 10 > 5) yyerror("constant value to assignment"); if (noassign($1, $3)) yyerror("illegal assignment"); $$->info = $1->info; }
	| INT           											{ $$ = intNode(INT, $1); $$->info = 1; }
	| STR           											{ $$ = strNode(STR, $1); $$->info = 2; }
	| REAL          											{ $$ = realNode(REAL, $1); $$->info = 3; }
	| '-' expr %prec UMINUS 									{ $$ = uniNode(UMINUS, $2); $$->info = $2->info; nostring($2, $2);}
	| '~' expr %prec UMINUS 									{ $$ = uniNode(NOT, $2); $$->info = intonly($2); }
	| '&' lv %prec UMINUS   									{ $$ = uniNode(REF, $2); $$->info = $2->info + 10; }
	| expr '!'             										{ $$ = uniNode('!', $1); $$->info = 3; intonly($1); }
	| INCR lv       											{ $$ = uniNode(INCR, $2); $$->info = intonly($2); }
	| DECR lv       											{ $$ = uniNode(DECR, $2); $$->info = intonly($2); }
	| lv INCR       											{ $$ = uniNode(POSINC, $1); $$->info = intonly($1); }
	| lv DECR       											{ $$ = uniNode(POSDEC, $1); $$->info = intonly($1); }
	| expr '+' expr 											{ $$ = binNode('+', $1, $3); $$->info = nostring($1, $3); }
	| expr '-' expr 											{ $$ = binNode('-', $1, $3); $$->info = nostring($1, $3); }
	| expr '*' expr 											{ $$ = binNode('*', $1, $3); $$->info = nostring($1, $3); }
	| expr '/' expr 											{ $$ = binNode('/', $1, $3); $$->info = nostring($1, $3); }
	| expr '%' expr 											{ $$ = binNode('%', $1, $3); $$->info = intonly($1); intonly($3); }
	| expr '<' expr 											{ $$ = binNode('<', $1, $3); $$->info = 1; }
	| expr '>' expr 											{ $$ = binNode('>', $1, $3); $$->info = 1; }
	| expr GE expr  											{ $$ = binNode(GE, $1, $3); $$->info = 1; }
	| expr LE expr  											{ $$ = binNode(LE, $1, $3); $$->info = 1; }
	| expr NE expr  											{ $$ = binNode(NE, $1, $3); $$->info = 1; }
	| expr '=' expr 											{ $$ = binNode('=', $1, $3); $$->info = 1; }
	| expr '&' expr 											{ $$ = binNode('&', $1, $3); $$->info = intonly($1); intonly($3); }
	| expr '|' expr 											{ $$ = binNode('|', $1, $3); $$->info = intonly($1); intonly($3); }
	| '(' expr ')' 												{ $$ = $2; $$->info = $2->info; }
	| ID '(' args ')' 											{ $$ = binNode(CALL, strNode(ID, $1), $3);
                            									  $$->info = checkargs($1, $3); }
	| ID '(' ')'    											{ $$ = binNode(CALL, strNode(ID, $1), nilNode(VOID));
                          										  $$->info = checkargs($1, 0); }
	;

%%
char **yynames =
#if YYDEBUG > 0
		 (char**)yyname;
#else
		 0;
#endif


void declare(int pub, int cnst, Node *type, Node *value)
{
  int typ;
  if (!value) {
    if (!pub && cnst) yyerror("local constants must be initialised");
    return;
  }
  if (value->attrib == INT && value->value.i == 0 && type->value.i > 10)
  	return; /* NULL pointer */
  if ((typ = value->info) % 10 > 5) typ -= 5;
  if (type->value.i != typ)
    yyerror("wrong types in initialization");
}
void enter(int typ, char *name) {
	fpar = malloc(32); /* 31 arguments, at most */
	fpar[0] = 0; /* argument count */
	if (IDfind(name, (long*)IDtest) < 20)
		IDnew(typ+20, name, (long)fpar);
	IDpush();
	if (typ != 4) { 
		if(typ % 5 == 1 || typ % 5 == 2) {
			retPos = -4;
			IDnew(typ, name, -4);
		}

		if(typ % 5 == 3) {
			retPos = -8;
			IDnew(typ, name, -8);	
		}
	}
	else {
		retPos = 0;
	}
}

int checkargs(char *name, Node *args) {
	char *arg;
	int typ;
        if ((typ = IDsearch(name, (long*)&arg,IDlevel(),1)) < 20) {
		yyerror("ident not a function");
		return 0;
	}
	if (args == 0 && arg[0] == 0)
		;
	else if (args == 0 && arg[0] != 0)
		yyerror("function requires no arguments");
	else if (args != 0 && arg[0] == 0)
		yyerror("function requires arguments");
	else {
		int err = 0, null, i = arg[0], typ;
		do {
			Node *n;
			if (i == 0) {
				yyerror("too many arguments.");
				err = 1;
				break;
			}
			n = LEFT_CHILD(args);
			typ = n->info;
			if (typ % 10 > 5) typ -= 5; /* remove CONST */
			null =  (n->attrib == INT && n->value.i == 0 && arg[i] > 10) ? 1 : 0;
			if (!null && arg[i] != typ) {
				yyerror("wrong argument type");
				err = 1;
				break;
			}
			args = RIGHT_CHILD(args);
			i--;
		} while (args->attrib != NIL);
		if (!err && i > 0)
			yyerror("missing arguments");
	}
	return typ % 20;
}

int nostring(Node *arg1, Node *arg2) {
	if (arg1->info % 5 == 2 || arg2->info % 5 == 2)
		yyerror("can not use strings");
	return arg1->info % 5 == 3 || arg2->info % 5 == 3 ? 3 : 1;
}

int intonly(Node *arg) {
	if (arg->info % 5 != 1)
		yyerror("only integers can be used");

	/*if (arg->info % 10 > 5)
		yyerror("argument is constant");*/
	return 1;
}

int noassign(Node *arg1, Node *arg2) {
	int t1 = arg1->info, t2 = arg2->info;
	if (t1 == t2) return 0;
	if (t1 == 3 && t2 == 1) return 0; /* real := int */
	if (t1 == 1 && t2 == 3) return 0; /* int := real */
	if (t1 == 2 && t2 == 11) return 0; /* string := int* */
	if (t1 == 2 && arg2->attrib == INT && arg2->value.i == 0)
		return 0; /* string := 0 */
	if (t1 > 10 && t1 < 20 && arg2->attrib == INT && arg2->value.i == 0)
		return 0; /* pointer := 0 */
	return 1;
}

void function(char *name, Node *body)
{
	Node *bloco = LEFT_CHILD(body);
	if (bloco != 0) { /* not a forward declaration */
		long par;
		int fwd = IDfind(name, &par);
		if (fwd > 40) yyerror("duplicate function");
		else IDreplace(fwd+40, name, par);
	}
}

/*
int yyerror(char *s) { 
	extern int yylineno; 
	printf("linha de erro: %d erro: %s\n",yylineno,s); 
	return 1; 
}

int main(int argc, char *argv[]) {
		yyparse();
		return 0;
}*/
