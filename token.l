%{
#include <stdio.h>
#include<stdlib.h>
#include "y.tab.h"
void yyerror(char *);
int lineno =1;
%}

fname     [#]([a-zA-Z0-9]*)
id        [_]([a-zA-Z0-9]|[_])*
wspc      [ \t]+
semi      [;]
comma     [,]
digit     [0-9]+
string    ["](.*)["]
comment   [`][`](.*)

%%

"<>"        { 	yylval.str ="<>";  ECHO;
		return NE; }
"="         {   yylval.str ="=";ECHO;
		return EQ;  }
">"         {	yylval.str =">";ECHO;
		return GT;  }
"<"         { 	yylval.str ="<";ECHO;
		return LT;  }
">="        { 	yylval.str =">=";ECHO;
		return GE; }
"<="        { 	yylval.str ="<=";ECHO;
		return LE; } 


":="        {ECHO; return EQU; }



"*"         { yylval.str ="*";ECHO;
		return MUL;}
"/"         {   yylval.str ="/";ECHO;
		return DIV; }
"+"         {   yylval.str ="+";ECHO;
		return ADD; }
"-"         {   yylval.str ="-";ECHO;
		return SUB; }
"%"	    {   yylval.str="%";ECHO;
		return MOD; }
"^"	    {   yylval.str="^";ECHO;
		return POW; }

		
"null"	      { ECHO;  return NUL;}
"num"         {ECHO; return NUM; }
"char"        {ECHO; return CHAR;}
"point"       {ECHO; return POINT;} 


"and"	      {ECHO; return AND; }
"or"          {ECHO; return OR; }
"not"         {ECHO; return NOT; }


{id}        { yylval.str=strdup(yytext);ECHO; return ID;}
{fname}	    {ECHO;yylval.str=strdup(yytext); return FNAME;}
{digit}     { yylval.str = strdup(yytext);ECHO;
	      return DIGIT; }

"\n"        { lineno++;printf("\n");}
[()] 	    { ECHO;return *yytext; }
[{}]	    { ECHO;return *yytext; }
":"         { ECHO;return *yytext; }
{wspc}      {;}
{comma}     { ECHO;return COMMA; }        
{semi}      {ECHO; return SEMI; }
{comment}   {ECHO;}



if          {ECHO; return IF; }
else        {ECHO; return ELSE; }
input       {ECHO; return INPUT; }
output      {ECHO; return OUTPUT; }
plug        {ECHO; return PLUG; }
socket      {ECHO; return SOCKET; }
for	    {ECHO; return FOR; }
while       {ECHO; return WHILE; }
repeat      {ECHO; return REPEAT; }
until       {ECHO; return UNTIL; }
eject       {ECHO; return EJECT; }
"start"	    {ECHO;   return START;}
"implicit"  {ECHO; return IMPLICIT;}


{string}    { yylval.str = strdup(yytext); ECHO;return STRING;}



%%
int yywrap(void)
{
return 1;
}
