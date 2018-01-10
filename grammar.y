%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <assert.h>

extern FILE *yyin,*yyout,*fp1;
FILE *fp,*fp1;
extern int lineno;

int yylex(void);
void yyerror(char *);
void lookup(char *ident,char *type);
void assembly();
int var_present(char *id);

struct vsymb
{ 
  char *fname;
  char *vname;
  char *vtype;
  int vsize;
  int value;
  int fix;
  struct vsymb *next;
};

struct fsymb
{ 
  char *fname;
  char *ftype;
  char *fargs;
  int def;//value will be '1' if defined else '0'
  struct fsymb *next;
};

struct quad
{
  char *arg1;
  char *arg2;
  char *op;
  char *r;
  int type;/*type=2-input or output stmt
	     type=1-only 3 arguments in quadruple
             type=0-normal*/
}q[250];

struct vsymb *vstart=NULL;
struct vsymb *vptr=NULL;
struct vsymb *vnew=NULL;

struct fsymb *fstart=NULL;
struct fsymb *fptr=NULL;
struct fsymb *fnew=NULL;


//variables used in the yacc program
char *func_arg;//temporary variable used to store func arguments in the function in string form in "reverse manner"
char *func_type;//temporary variable for function return type
char *func_name;//temporary variable for function return name 
int flag;
char *ident;//temporary variable used to store identifiers of a type in revesre defined order
char *expr;
char *expr1;
char ch,*iter,*iter1;
char *err;
int quadindex;//refering quadruple index
int temp=0;
int loop1=0,loop2=0,loop3=0,loop4=0,loop5=0,loop6=0,loop7=0,loop8=0;
int i,k;

//const char num='0';//used for indexing the temporary variables generated during compilation
char *t;//used for denoting the temprary variables generated during compilation
int rsvar=-1;//used for indexing *rs(result set)
int num=48;
char infix[100];
char qno[30];//convert the quadindex to string
%}

%union{

char *str;
int num;

}

%start j3

%token <str> ID COMMA  SEMI IF EQU NE GE LE MUL DIV SUB ADD POW MOD DIGIT ELSE INPUT OUTPUT EQ GT LT STRING START FNAME PLUG SOCKET FOR WHILE REPEAT  UNTIL EJECT IMPLICIT AND OR NOT NUM CHAR POINT NUL
//%token <num>  

%type <str> type fun_type arg stmt arith_oper var var1 ip op fun_call cond rel_oper iter


%right EQU
%left '+' '-'
%left '*' '/' '%'

%%
//program defintion
j3:
	|fun_decl funs
	|funs
	|fun_decl
	;

//function declaration at the top
fun_decl:fun_decl fun_type FNAME '(' arg ')' SEMI {  
						func_type=strdup($2);
						func_name=strdup($3);
						if(func_check(func_type,func_name,func_arg))//return 1 if the function is already declared 
						{
							yyerror(strcat($3," redeclared"));
						}
					    // else the function gets added to the function symbol table in the func_check function itself 
					 	 }
	|fun_type FNAME '(' arg ')' SEMI  {  
						func_type=strdup($1);
						func_name=strdup($2);
						
						if(func_check(func_type,func_name,func_arg))//return 1 if the function is already declared 
						{
							yyerror(strcat($2," redeclared"));
						}
					    // else the function gets added to the function symbol table in the func_check function itself 
					  }
							
	;

//possible function return types
fun_type:NUL {$$=strdup("null");}
	|NUM {$$=strdup("num");}
	|POINT {$$=strdup("point");}
	;

//function arguments(same as decl declared below)
//func_arg stores the arguments in reverse order
arg:    {func_arg=strdup("empty");}
	|type ID COMMA arg1 {func_arg=strdup($1);strcat(strcat(strcat(func_arg," "),$2),",");}
	|type ID  {func_arg=strdup($1);strcat(strcat(func_arg," "),$2);}
	;
arg1:type ID COMMA arg1 {strcat(strcat(strcat(strcat(func_arg,$1)," "),$2),",");}
	|type ID {func_arg=strdup($1);strcat(strcat(strcat(func_arg," "),$2),",");}
	;

//possible datatypes
type:NUM {$$=strdup("num");}
	|POINT {$$=strdup("point");}
	|CHAR {$$=strdup("char");}
	;
//function defintion(includes the function definition for start() too...)
funs:fun_type FNAME '(' arg ')' '{' fun_stmt '}' {
						func_type=strdup($1);
						func_name=strdup($2);
						flag=func_check(func_type,func_name,func_arg);//return1 if the function isalready declared 
						
						fptr=fstart;
							while(fptr!=NULL)
							{
							flag=0;
							if(!strcmp(fptr->fname,func_name))
								if(!strcmp(fptr->ftype,func_type))
									if(!strcmp(fptr->fargs,func_arg))
									{
									if(fptr->def==1)
									yyerror(strcat(func_name," defined already"));
									else fptr->def=1;//def=1 implies the function is defined
									break;
									}
							fptr=fptr->next;
							}//while
						
						vptr=vstart;
						while(vptr!=NULL)
						{
							if(!strcmp(vptr->fname,"undef"))
								vptr->fname=strdup(func_name);
							vptr=vptr->next;
						}
						//quadruples
						temp=0;
						while(temp<quadindex)
						{
							if(!strcmp(q[temp].r,"function"))
								if(!strcmp(q[temp].op,"undef"))
								{
									q[temp].op=strdup(func_name);
									break;
								}
						}
						
						//function end quadruple
					q[quadindex].arg1=strdup("");
					q[quadindex].arg2=strdup("");
					q[quadindex].op=strdup("end");
					q[quadindex++].r=strdup("function");
						 }
	|funs fun_type FNAME '(' arg ')' '{' fun_stmt '}' 
						 {
						func_type=strdup($2);
						func_name=strdup($3);
						flag=func_check(func_type,func_name,func_arg);//return1 if the function is alredy declared 
						//the above function call insert entry for undeclared functions in fun table
							fptr=fstart;
							while(fptr!=NULL)
							{
							flag=0;
							if(!strcmp(fptr->fname,func_name))
								if(!strcmp(fptr->ftype,func_type))
									if(!strcmp(fptr->fargs,func_arg))
									{
									if(fptr->def==1)
									yyerror(strcat(func_name," defined already"));
									else fptr->def=1;//def=1 implies the function is defined
									break;
									}
							fptr=fptr->next;
							}//while
						
						vptr=vstart;
						while(vptr!=NULL)
						{
							if(!strcmp(vptr->fname,"undef"))
								vptr->fname=strdup(func_name);
							vptr=vptr->next;
						}
						//quadruples
						temp=0;
						while(temp<quadindex)
						{
							if(!strcmp(q[temp].r,"function"))
								if(!strcmp(q[temp].op,"undef"))
								{
									q[temp].op=strdup(func_name);
									break;
								}
						}
						
						//function end quadruple
					q[quadindex].arg1=strdup("");
					q[quadindex].arg2=strdup("");
					q[quadindex].op=strdup("end");
					q[quadindex++].r=strdup("function");
						 }
	;

//content of a function
fun_stmt:
	|decl
	|stmt
	|decl stmt
	;

decl:decl type ID SEMI {     ident=strdup($3);
			lookup(ident,$2);//check whether identifier is already defined
		  }
	|decl type ID COMMA decl1 {  strcat(strcat(ident," "),$3);
				lookup(ident,$2);//check whether identifier is already defined
		  	     }
	|type ID SEMI {     ident=strdup($2);
			lookup(ident,$1);//check whether identifier is already defined
		  }
	|type ID COMMA decl1 {  strcat(strcat(ident," "),$2);
				lookup(ident,$1);//check whether identifier is already defined
		  	     }
	;

decl1:ID SEMI                { ident=strdup($1);}
	|ID COMMA decl1      { strcat(strcat(ident," "),$1);} 
	;
//possible set of statements
stmt:ID EQU var arith_oper var SEMI {  	//code for quadruple of fuction header
				
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("function");
				
				//adding the code for quadruple generation for the given cfg
				        q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup($5);
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup($1); 
				
			  	    }
	|ID EQU var SEMI  {  	//quadruple for fuction header
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("function");
				//for the given cfg
					q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($1);
				//checking variable defined or not
				/*	if(var_present($1))
					{
						sprintf(err,"variable-%s undefined",$1);
						yyerror(err);
					}	*/		  
			}
	|IF'('cond')' '{' stmt1 '}' control_if {loop1=1;//the function header must be defined in the innerloop
//quadruple for the given cfg must be included before the inner loop quads
//assuming that one blank quad is left free for filling at this stage i.e. jfalse to current line
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner"))
					{
					sprintf(qno,"%d",quadindex);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($3);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
						   }	
	|IF'('cond')' '{' '}' control_if       {  //quadruple for fuction header	
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("function");
					//quadruple for the given cfg
			  			}
	|INPUT'(' ip ')' SEMI  			{  
					//quadruple for fuction header
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("function");
					//quadruple for the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("input");
			  			}
	|OUTPUT'(' op ')' SEMI  		 { //quadruple for fuction header 	
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("function");
					//quadruple for the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("output");
			  			}
	|FOR '(' init SEMI cond SEMI iter ')' '{' stmt1 '}' 
					{//quadruple for fuction header must be included in the inner loops since they r executing first	
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner"))
					{
					sprintf(qno,"%d",quadindex+2);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($5);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					//if(strcmp($7,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup("JTrue");
			  			}
	|FOR '(' init SEMI cond SEMI iter ')' SEMI
						 { //quadruple for fuction header  	
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("function");
					//quadruple for the given cfg
					sprintf(qno,"%d",quadindex+2);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup("JFalse");
					//iter1=strdup($7);
					//if(strcmp(iter1,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					
					/*for(i=2,k=0;i<strlen(iter1);i++,k++)
					{
						*(iter+k)=*(iter1+i);	
					}
						*(iter+k)='\0';
					ch=*(iter1+0);
					if(ch=='1')
						q[quadindex].arg1=strdup(iter);
						q[quadindex].arg2=strdup("1");
						q[quadindex].op=strdup("+");
						q[quadindex++].r=strdup(iter);
					if(ch=='2')
						q[quadindex].arg1=strdup(iter);
						q[quadindex].arg2=strdup("1");
						q[quadindex].op=strdup("-");
						q[quadindex++].r=strdup(iter);*/
			  		}
						}
	|WHILE '(' cond ')' '{' stmt1 '}' 
					{//quadruple for fuction header must be included in the inner loops since they r executing first	
					//quadruple for the given cfg
//quadruple for the given cfg must be included before the inner loop quads
//assuming that one blank quad is left free for filling at this stage i.e. jfalse to current line
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner"))
					{
					sprintf(qno,"%d",quadindex+1);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($3);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for

					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("JTrue");
			  			}
	|REPEAT '{' stmt1 '}' UNTIL '(' cond ')' SEMI 
					{//quadruple for fuction header must be included in the inner loops since they r executing first	
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner"))
					{
					break;
					}
					}
					for(i=temp;i<quadindex-1;i++)
					{
						q[i].r=q[i+1].r;
						q[i].arg1=q[i+1].arg1;
						q[i].arg2=q[i+1].arg2;
						q[i].op=q[i+1].op;
					}
					quadindex--;
					sprintf(qno,"%d",temp);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($7);
					q[quadindex++].r=strdup("JTrue");
			  			}
	|PLUG '(' var1 ')' '{' plug_stmt '}'
						 {  //quadruple for fuction header	
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("function");
					//quadruple for the given cfg
			  			}
	|fun_call				 { //quadruple for fuction header 	
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("function");
				
					//quadruple the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($1);
					q[quadindex++].r=strdup("funcall");
					//quadruple for the given cfg
			  			}
	|stmt ID EQU var arith_oper var SEMI { 				        
					q[quadindex].arg1=strdup($4);
					q[quadindex].arg2=strdup($6);
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup($2);
					
				}
	|stmt ID EQU var SEMI	{	//for the given cfg
					q[quadindex].arg1=strdup($4);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($2);
					//checking variable defined or not
					/*if(var_present($2))
					{
						sprintf(err,"variable-%s undefined",$1);
						yyerror(err);
					}*/

				}
	|stmt IF'('cond')' '{' stmt1 '}' control_if {//quadruple for the given cfg must be included before the inner loop quads
//assuming that one blank quad is left free for filling at this stage i.e. jfalse to current line
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner"))
					{
					sprintf(qno,"%d",quadindex);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($4);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
						   }

	|stmt IF'('cond')' '{' '}' control_if 
	|stmt INPUT'(' ip ')' SEMI 	{
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("input");
					}
	|stmt OUTPUT'(' op ')' SEMI     {
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("output");
					}
	|stmt FOR '(' init SEMI cond SEMI iter ')' '{' stmt1 '}'{
//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner"))
					{
					sprintf(qno,"%d",quadindex+2);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($6);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					//if(strcmp($7,"null"))
					
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JTrue");
								}
	|stmt FOR '(' init SEMI cond SEMI iter ')' SEMI{
					sprintf(qno,"%d",quadindex+3);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JFalse");
					//if(strcmp($7,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
					sprintf(qno,"%d",quadindex-1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JTrue");
							}
					
	|stmt WHILE '(' cond ')' '{' stmt1 '}'
					{//quadruple for fuction header must be included in the inner loops since they r executing first	
					//quadruple for the given cfg
						loop1=1;//the function header must be defined in the innerloop
//quadruple for the given cfg must be included before the inner loop quads
//assuming that one blank quad is left free for filling at this stage i.e. jfalse to current line
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner"))
					{
					sprintf(qno,"%d",quadindex+1);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($4);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for

					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("JTrue");
			  			}
	|stmt REPEAT '{' stmt1 '}' UNTIL '(' cond ')' SEMI{//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner"))
					{
					break;
					}
					}
					for(i=temp;i<quadindex-1;i++)
					{
						q[i].r=q[i+1].r;
						q[i].arg1=q[i+1].arg1;
						q[i].arg2=q[i+1].arg2;
						q[i].op=q[i+1].op;
					}
					quadindex--;
					sprintf(qno,"%d",temp);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($8);
					q[quadindex++].r=strdup("JTrue");
								}
	|stmt PLUG '(' var1 ')' '{' plug_stmt '}'  
	|stmt fun_call 			{
					//quadruple the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($2);
					q[quadindex++].r=strdup("funcall");
					//quadruple for the given cfg
			  			}
	;

//inner loop1
stmt1:ID EQU var arith_oper var SEMI	{	//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner");
					//quadruple for current cfg
				        q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup($5);
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup($1); 

					}


	|ID EQU var SEMI	{	//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner");

					//quadruple for current cfg
					q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($1);
				}
	|IF'('cond')' '{' stmt2 '}' control_if1   {

					//quadruple for fuction header

					//quadruple for current cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner1"))
					{
					sprintf(qno,"%d",quadindex+1);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($3);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					
					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner");
						   }
	|IF'('cond')' '{' '}' control_if1{//the function header must be defined in the innerloop
//quadruple for the given cfg must be included before the inner loop quads
//assuming that one blank quad is left free for filling at this stage i.e. jfalse to current line
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner1"))
					{
					sprintf(qno,"%d",quadindex+1);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($3);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					
					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner");
						   }
	|INPUT'(' ip ')' SEMI {		//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner");

					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("input");
				}
	|OUTPUT'(' op ')' SEMI  {	//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner");

					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("output");
				}
	|FOR '(' init SEMI cond SEMI iter ')' '{' stmt2 '}'{	
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner1"))
					{
					sprintf(qno,"%d",quadindex+3);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($5);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					//if(strcmp($7,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner");
			  			}
	|FOR '(' init SEMI cond SEMI iter ')' SEMI {

					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner");

					//quadruple for the given cfg
					sprintf(qno,"%d",quadindex+2);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup("JFalse");
					//iter1=strdup($7);
					//if(strcmp(iter1,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
							}
	|WHILE '(' cond ')' '{' stmt2 '}' {
					//quadruple for the given cfg
//quadruple for the given cfg must be included before the inner loop quads
//assuming that one blank quad is left free for filling at this stage i.e. jfalse to current line
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner1"))
					{
					sprintf(qno,"%d",quadindex+2);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($3);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for

					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner");
			  			}
	|REPEAT '{' stmt2 '}' UNTIL '(' cond ')' SEMI {
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner1"))
					{
					break;
					}
					}
					for(i=temp;i<quadindex-1;i++)
					{
						q[i].r=q[i+1].r;
						q[i].arg1=q[i+1].arg1;
						q[i].arg2=q[i+1].arg2;
						q[i].op=q[i+1].op;
					}
					quadindex--;
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($7);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner");
			  			}
	|PLUG '(' var1 ')' '{' plug_stmt '}'
	|fun_call				 { //quadruple for fuction header 
					

					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner");

					//quadruple the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($1);
					q[quadindex++].r=strdup("funcall");
			  			}
	|stmt1 ID EQU var arith_oper var SEMI { 				        
					q[quadindex].arg1=strdup($4);
					q[quadindex].arg2=strdup($6);
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup($2);
					      }
	|stmt1 ID EQU var SEMI {
					//quadruple for current cfg
					q[quadindex].arg1=strdup($4);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($2);
				}
	|stmt1 IF'('cond')' '{' stmt2 '}' control_if1 {//quadruple for the given cfg must be included before the inner loop quads
//assuming that one blank quad is left free for filling at this stage i.e. jfalse to current line
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner1"))
					{
					sprintf(qno,"%d",quadindex);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($4);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
						   }
	|stmt1 IF'('cond')' '{' '}' control_if1
	|stmt1 INPUT'(' ip ')' SEMI  {
					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("input");
				    }
	|stmt1 OUTPUT'(' op ')' SEMI {
					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("output");
				   }
	|stmt1 FOR '(' init SEMI cond SEMI iter ')' '{' stmt2 '}'{
//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner1"))
					{
					sprintf(qno,"%d",quadindex+2);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($6);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					//if(strcmp($7,"null"))
					
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JTrue");
								}
	|stmt1 FOR '(' init SEMI cond SEMI iter ')' SEMI{
					sprintf(qno,"%d",quadindex+3);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JFalse");
					//if(strcmp($7,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
					sprintf(qno,"%d",quadindex-1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JTrue");
							}
	|stmt1 WHILE '(' cond ')' '{' stmt2 '}'{
					//quadruple for the given cfg
//quadruple for the given cfg must be included before the inner loop quads
//assuming that one blank quad is left free for filling at this stage i.e. jfalse to current line
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner1"))
					{
					sprintf(qno,"%d",quadindex+1);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($4);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for

					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("JTrue");
			  			}
	|stmt1 REPEAT '{' stmt2 '}' UNTIL '(' cond ')' SEMI{//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner1"))
					{
					break;
					}
					}
					for(i=temp;i<quadindex-1;i++)
					{
						q[i].r=q[i+1].r;
						q[i].arg1=q[i+1].arg1;
						q[i].arg2=q[i+1].arg2;
						q[i].op=q[i+1].op;
					}
					quadindex--;
					sprintf(qno,"%d",temp);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($8);
					q[quadindex++].r=strdup("JTrue");
								}
	|stmt1 PLUG '(' var1 ')' '{' plug_stmt '}' 
	|stmt1 fun_call 			{ 

					//quadruple the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($2);
					q[quadindex++].r=strdup("funcall");
			  			}
	;

//inner loop2
stmt2:ID EQU var arith_oper var SEMI	{
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner1");
					//quadruple for current cfg
				        q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup($5);
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup($1); 

					}

	|ID EQU var SEMI{	//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner1");

					//quadruple for current cfg
					q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($1);
				}
	|IF'('cond')' '{' stmt3 '}' control_if2  {

					//quadruple for fuction header

					//quadruple for current cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner2"))
					{
					sprintf(qno,"%d",quadindex+1);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($3);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					
					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner1");
						   }
	|IF'('cond')' '{' '}' control_if2 
	|INPUT'(' ip ')' SEMI {		//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner1");

					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("input");
				}
	|OUTPUT'(' op ')' SEMI  {		//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner1");

					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("output");
				}
	|FOR '(' init SEMI cond SEMI iter ')' '{' stmt3 '}'{	
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner2"))
					{
					sprintf(qno,"%d",quadindex+3);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($5);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					//if(strcmp($7,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner1");
			  			}

	|FOR '(' init SEMI cond SEMI iter ')' SEMI{

					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner1");

					//quadruple for the given cfg
					sprintf(qno,"%d",quadindex+2);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup("JFalse");
					//iter1=strdup($7);
					//if(strcmp(iter1,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
							}
	|WHILE '(' cond ')' '{' stmt3 '}' {
					//quadruple for the given cfg
//quadruple for the given cfg must be included before the inner loop quads
//assuming that one blank quad is left free for filling at this stage i.e. jfalse to current line
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner2"))
					{
					sprintf(qno,"%d",quadindex+2);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($3);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for

					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner1");
			  			}
	|REPEAT '{' stmt3 '}' UNTIL '(' cond ')' SEMI  {
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner2"))
					{
					break;
					}
					}
					for(i=temp;i<quadindex-1;i++)
					{
						q[i].r=q[i+1].r;
						q[i].arg1=q[i+1].arg1;
						q[i].arg2=q[i+1].arg2;
						q[i].op=q[i+1].op;
					}
					quadindex--;
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($7);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner1");
			  			}
	|PLUG '(' var1 ')' '{' plug_stmt '}'
	|fun_call				 { //quadruple for fuction header 
					

					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner1");

					//quadruple the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($1);
					q[quadindex++].r=strdup("funcall");
			  			}
	|stmt2 ID EQU var arith_oper var SEMI { 				        
					q[quadindex].arg1=strdup($4);
					q[quadindex].arg2=strdup($6);
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup($2);
					      }
	|stmt2 ID EQU var SEMI  {
					//quadruple for current cfg
					q[quadindex].arg1=strdup($4);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($2);
				}

	|stmt2 IF'('cond')' '{' stmt3 '}' control_if2 {//quadruple for the given cfg must be included before the inner loop quads
//assuming that one blank quad is left free for filling at this stage i.e. jfalse to current line
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner2"))
					{
					sprintf(qno,"%d",quadindex);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($4);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
						   }
	|stmt2 IF'('cond')' '{' '}' control_if2
	|stmt2 INPUT'(' ip ')' SEMI {
					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("input");
				    }
	|stmt2 OUTPUT'(' op ')' SEMI{
					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("output");
				    }
	|stmt2 FOR '(' init SEMI cond SEMI iter ')' '{' stmt3 '}'{
//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner2"))
					{
					sprintf(qno,"%d",quadindex+2);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($6);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					//if(strcmp($7,"null"))
					
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JTrue");
								}
	|stmt2 FOR '(' init SEMI cond SEMI iter ')' SEMI{
					sprintf(qno,"%d",quadindex+3);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JFalse");
					//if(strcmp($7,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
					sprintf(qno,"%d",quadindex-1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JTrue");
							}
	|stmt2 WHILE '(' cond ')' '{' stmt3 '}'{
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner2"))
					{
					sprintf(qno,"%d",quadindex+1);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($4);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for

					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("JTrue");
			  			}
	|stmt2 REPEAT '{' stmt3 '}' UNTIL '(' cond ')' SEMI{//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner2"))
					{
					break;
					}
					}
					for(i=temp;i<quadindex-1;i++)
					{
						q[i].r=q[i+1].r;
						q[i].arg1=q[i+1].arg1;
						q[i].arg2=q[i+1].arg2;
						q[i].op=q[i+1].op;
					}
					quadindex--;
					sprintf(qno,"%d",temp);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($8);
					q[quadindex++].r=strdup("JTrue");
								}
	|stmt2 PLUG '(' var1 ')' '{' plug_stmt '}' 
	|stmt2 fun_call { 

					//quadruple the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($2);
					q[quadindex++].r=strdup("funcall");
			 }
	;

//inner loop3
stmt3:ID EQU var arith_oper var SEMI{
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner2");
					//quadruple for current cfg
				        q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup($5);
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup($1); 

					}
	|ID EQU var SEMI	{	//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner2");

					//quadruple for current cfg
					q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($1);
				}
	|IF'('cond')' '{' stmt4 '}' control_if3{

					//quadruple for fuction header

					//quadruple for current cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner3"))
					{
					sprintf(qno,"%d",quadindex+1);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($3);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					
					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner2");
						   }
	|IF'('cond')' '{' '}' control_if3 
	|INPUT'(' ip ')' SEMI {		//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner2");

					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("input");
				}
	|OUTPUT'(' op ')' SEMI  {		//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner2");

					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("output");
				}
	|FOR '(' init SEMI cond SEMI iter ')' '{' stmt4 '}'{	
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner3"))
					{
					sprintf(qno,"%d",quadindex+3);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($5);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					//if(strcmp($7,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner2");
			  			}
	|FOR '(' init SEMI cond SEMI iter ')' SEMI{

					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner2");

					//quadruple for the given cfg
					sprintf(qno,"%d",quadindex+2);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup("JFalse");
					//iter1=strdup($7);
					//if(strcmp(iter1,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
							}
	|WHILE '(' cond ')' '{' stmt4 '}' {
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner3"))
					{
					sprintf(qno,"%d",quadindex+2);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($3);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for

					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner2");
			  			}
	|REPEAT '{' stmt4 '}' UNTIL '(' cond ')' SEMI  {
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner3"))
					{
					break;
					}
					}
					for(i=temp;i<quadindex-1;i++)
					{
						q[i].r=q[i+1].r;
						q[i].arg1=q[i+1].arg1;
						q[i].arg2=q[i+1].arg2;
						q[i].op=q[i+1].op;
					}
					quadindex--;
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($7);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner2");
			  			}
	|PLUG '(' var1 ')' '{' plug_stmt '}'
	|fun_call			{ //quadruple for fuction header 
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner2");

					//quadruple the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($1);
					q[quadindex++].r=strdup("funcall");
			  		}
	|stmt3 ID EQU var arith_oper var SEMI { 				        
					q[quadindex].arg1=strdup($4);
					q[quadindex].arg2=strdup($6);
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup($2);
					      }
	|stmt3 ID EQU var SEMI  {
					//quadruple for current cfg
					q[quadindex].arg1=strdup($4);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($2);
				}
	|stmt3 IF'('cond')' '{' stmt4 '}' control_if3 {//quadruple for the given cfg 

					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner3"))
					{
					sprintf(qno,"%d",quadindex);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($4);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
						   }
	|stmt3 IF'('cond')' '{' '}' control_if3
	|stmt3 INPUT'(' ip ')' SEMI {
					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("input");
				    }
	|stmt3 OUTPUT'(' op ')' SEMI{
					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("output");
				    }
	|stmt3 FOR '(' init SEMI cond SEMI iter ')' '{' stmt4 '}'{
//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner3"))
					{
					sprintf(qno,"%d",quadindex+2);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($6);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					//if(strcmp($7,"null"))
					
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JTrue");
								}
	|stmt3 FOR '(' init SEMI cond SEMI iter ')' SEMI{
					sprintf(qno,"%d",quadindex+3);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JFalse");
					//if(strcmp($7,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
					sprintf(qno,"%d",quadindex-1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JTrue");
							}
	|stmt3 WHILE '(' cond ')' '{' stmt4 '}'{
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner3"))
					{
					sprintf(qno,"%d",quadindex+1);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($4);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for

					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("JTrue");
			  			}
	|stmt3 REPEAT '{' stmt4 '}' UNTIL '(' cond ')' SEMI{//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner3"))
					{
					break;
					}
					}
					for(i=temp;i<quadindex-1;i++)
					{
						q[i].r=q[i+1].r;
						q[i].arg1=q[i+1].arg1;
						q[i].arg2=q[i+1].arg2;
						q[i].op=q[i+1].op;
					}
					quadindex--;
					sprintf(qno,"%d",temp);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($8);
					q[quadindex++].r=strdup("JTrue");
								}
	|stmt3 PLUG '(' var1 ')' '{' plug_stmt '}' 
	|stmt3 fun_call { 

					//quadruple the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($2);
					q[quadindex++].r=strdup("funcall");
			 }
	;

//inner loop4
stmt4:ID EQU var arith_oper var SEMI{
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner3");
					//quadruple for current cfg
				        q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup($5);
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup($1); 

					}
	|ID EQU var SEMI	{	//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner3");

					//quadruple for current cfg
					q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($1);
				}
	|IF'('cond')' '{' stmt5 '}' control_if4  {

					//quadruple for fuction header

					//quadruple for current cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner4"))
					{
					sprintf(qno,"%d",quadindex+1);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($3);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					
					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner3");
						   }
	|IF'('cond')' '{' '}' control_if4
	|INPUT'(' ip ')' SEMI 	{		//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner3");

					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("input");
				}
	|OUTPUT'(' op ')' SEMI  {		//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner3");

					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("output");
				}
	|FOR '(' init SEMI cond SEMI iter ')' '{' stmt5 '}'{	
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner4"))
					{
					sprintf(qno,"%d",quadindex+3);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($5);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					//if(strcmp($7,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner3");
			  			}
	|FOR '(' init SEMI cond SEMI iter ')' SEMI{

					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner3");

					//quadruple for the given cfg
					sprintf(qno,"%d",quadindex+2);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup("JFalse");
					//iter1=strdup($7);
					//if(strcmp(iter1,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
							}
	|WHILE '(' cond ')' '{' stmt5 '}' {
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner4"))
					{
					sprintf(qno,"%d",quadindex+2);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($3);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for

					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner3");
			  			}
	|REPEAT '{' stmt5 '}' UNTIL '(' cond ')' SEMI  {
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner4"))
					{
					break;
					}
					}
					for(i=temp;i<quadindex-1;i++)
					{
						q[i].r=q[i+1].r;
						q[i].arg1=q[i+1].arg1;
						q[i].arg2=q[i+1].arg2;
						q[i].op=q[i+1].op;
					}
					quadindex--;
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($7);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner3");
			  			}
	|PLUG '(' var1 ')' '{' plug_stmt '}'
	|fun_call			{ //quadruple for fuction header 
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner3");

					//quadruple the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($1);
					q[quadindex++].r=strdup("funcall");
			  		}
	|stmt4 ID EQU var arith_oper var SEMI { 				        
					q[quadindex].arg1=strdup($4);
					q[quadindex].arg2=strdup($6);
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup($2);
					      }
	|stmt4 ID EQU var SEMI  {
					//quadruple for current cfg
					q[quadindex].arg1=strdup($4);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($2);
				}
	|stmt4 IF'('cond')' '{' stmt5 '}' control_if4 {//quadruple for the given cfg 

					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner4"))
					{
					sprintf(qno,"%d",quadindex);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($4);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
						   }
	|stmt4 IF'('cond')' '{' '}' control_if4
	|stmt4 INPUT'(' ip ')' SEMI {
					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("input");
				    }
	|stmt4 OUTPUT'(' op ')' SEMI{
					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("output");
				    }
	|stmt4 FOR '(' init SEMI cond SEMI iter ')' '{' stmt5 '}'{
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner4"))
					{
					sprintf(qno,"%d",quadindex+2);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($6);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					//if(strcmp($7,"null"))
					
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JTrue");
								}
	|stmt4 FOR '(' init SEMI cond SEMI iter ')' SEMI{
					sprintf(qno,"%d",quadindex+3);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JFalse");
					//if(strcmp($7,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
					sprintf(qno,"%d",quadindex-1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JTrue");
							}
	|stmt4 WHILE '(' cond ')' '{' stmt5 '}'{
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner4"))
					{
					sprintf(qno,"%d",quadindex+1);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($4);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for

					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("JTrue");
			  			}
	|stmt4 REPEAT '{' stmt5 '}' UNTIL '(' cond ')' SEMI{//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner4"))
					{
					break;
					}
					}
					for(i=temp;i<quadindex-1;i++)
					{
						q[i].r=q[i+1].r;
						q[i].arg1=q[i+1].arg1;
						q[i].arg2=q[i+1].arg2;
						q[i].op=q[i+1].op;
					}
					quadindex--;
					sprintf(qno,"%d",temp);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($8);
					q[quadindex++].r=strdup("JTrue");
								}
	|stmt4 PLUG '(' var1 ')' '{' plug_stmt '}' 
	|stmt4 fun_call { 

					//quadruple the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($2);
					q[quadindex++].r=strdup("funcall");
			 }
	;

//inner loop5
stmt5:ID EQU var arith_oper var SEMI{
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner4");
					//quadruple for current cfg
				        q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup($5);
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup($1); 

					}
	|ID EQU var SEMI	{	//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner4");

					//quadruple for current cfg
					q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($1);
				}
	|IF'('cond')' '{' stmt6 '}' control_if5  {

					//quadruple for fuction header

					//quadruple for current cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner5"))
					{
					sprintf(qno,"%d",quadindex+1);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($3);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					
					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner4");
						   }
	|IF'('cond')' '{' '}' control_if5
	|INPUT'(' ip ')' SEMI 	{		//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner4");

					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("input");
				}
	|OUTPUT'(' op ')' SEMI  {		//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner4");

					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("output");
				}
	|FOR '(' init SEMI cond SEMI iter ')' '{' stmt6 '}'{	
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner5"))
					{
					sprintf(qno,"%d",quadindex+3);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($5);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					//if(strcmp($7,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner4");
			  			}
	|FOR '(' init SEMI cond SEMI iter ')' SEMI{

					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner4");

					//quadruple for the given cfg
					sprintf(qno,"%d",quadindex+2);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup("JFalse");
					//iter1=strdup($7);
					//if(strcmp(iter1,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
							}
	|WHILE '(' cond ')' '{' stmt6 '}' {
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner5"))
					{
					sprintf(qno,"%d",quadindex+2);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($3);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for

					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner4");
			  			}
	|REPEAT '{' stmt6 '}' UNTIL '(' cond ')' SEMI  {
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner5"))
					{
					break;
					}
					}
					for(i=temp;i<quadindex-1;i++)
					{
						q[i].r=q[i+1].r;
						q[i].arg1=q[i+1].arg1;
						q[i].arg2=q[i+1].arg2;
						q[i].op=q[i+1].op;
					}
					quadindex--;
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($7);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner4");
			  			}
	|PLUG '(' var1 ')' '{' plug_stmt '}'
	|fun_call			{ //quadruple for fuction header 
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner4");

					//quadruple the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($1);
					q[quadindex++].r=strdup("funcall");
			  		}
	|stmt5 ID EQU var arith_oper var SEMI { 				        
					q[quadindex].arg1=strdup($4);
					q[quadindex].arg2=strdup($6);
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup($2);
					      }
	|stmt5 ID EQU var SEMI  {
					//quadruple for current cfg
					q[quadindex].arg1=strdup($4);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($2);
				}
	|stmt5 IF'('cond')' '{' stmt6 '}' control_if5 {//quadruple for the given cfg 

					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner5"))
					{
					sprintf(qno,"%d",quadindex);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($4);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
						   }
	|stmt5 IF'('cond')' '{' '}' control_if5
	|stmt5 INPUT'(' ip ')' SEMI {
					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("input");
				    }
	|stmt5 OUTPUT'(' op ')' SEMI{
					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("output");
				    }
	|stmt5 FOR '(' init SEMI cond SEMI iter ')' '{' stmt6 '}'{
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner5"))
					{
					sprintf(qno,"%d",quadindex+2);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($6);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					//if(strcmp($7,"null"))
					
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JTrue");
								}
	|stmt5 FOR '(' init SEMI cond SEMI iter ')' SEMI{
					sprintf(qno,"%d",quadindex+3);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JFalse");
					//if(strcmp($7,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
					sprintf(qno,"%d",quadindex-1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JTrue");
							}
	|stmt5 WHILE '(' cond ')' '{' stmt6 '}'{
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner5"))
					{
					sprintf(qno,"%d",quadindex+1);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($4);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for

					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("JTrue");
			  			}
	|stmt5 REPEAT '{' stmt6 '}' UNTIL '(' cond ')' SEMI{//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner5"))
					{
					break;
					}
					}
					for(i=temp;i<quadindex-1;i++)
					{
						q[i].r=q[i+1].r;
						q[i].arg1=q[i+1].arg1;
						q[i].arg2=q[i+1].arg2;
						q[i].op=q[i+1].op;
					}
					quadindex--;
					sprintf(qno,"%d",temp);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($8);
					q[quadindex++].r=strdup("JTrue");
								}
	|stmt5 PLUG '(' var1 ')' '{' plug_stmt '}' 
	|stmt5 fun_call { 

					//quadruple the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($2);
					q[quadindex++].r=strdup("funcall");
			 }
	;


//inner loop6
stmt6:ID EQU var arith_oper var SEMI{
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner5");
					//quadruple for current cfg
				        q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup($5);
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup($1); 

					}
	|ID EQU var SEMI	{	//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner5");

					//quadruple for current cfg
					q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($1);
				}
	|IF'('cond')' '{' stmt7 '}' control_if6  {

					//quadruple for fuction header

					//quadruple for current cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner6"))
					{
					sprintf(qno,"%d",quadindex+1);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($3);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					
					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner5");
						   }
	|IF'('cond')' '{' '}' control_if6
	|INPUT'(' ip ')' SEMI 	{		//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner5");

					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("input");
				}
	|OUTPUT'(' op ')' SEMI  {		//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner5");

					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("output");
				}
	|FOR '(' init SEMI cond SEMI iter ')' '{' stmt7 '}'{	
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner6"))
					{
					sprintf(qno,"%d",quadindex+3);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($5);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					//if(strcmp($7,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner5");
			  			}
	|FOR '(' init SEMI cond SEMI iter ')' SEMI{

					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner5");

					//quadruple for the given cfg
					sprintf(qno,"%d",quadindex+2);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup("JFalse");
					//iter1=strdup($7);
					//if(strcmp(iter1,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
							}
	|WHILE '(' cond ')' '{' stmt7 '}' {
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner6"))
					{
					sprintf(qno,"%d",quadindex+2);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($3);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for

					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner5");
			  			}
	|REPEAT '{' stmt7 '}' UNTIL '(' cond ')' SEMI  {
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner6"))
					{
					break;
					}
					}
					for(i=temp;i<quadindex-1;i++)
					{
						q[i].r=q[i+1].r;
						q[i].arg1=q[i+1].arg1;
						q[i].arg2=q[i+1].arg2;
						q[i].op=q[i+1].op;
					}
					quadindex--;
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($7);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner5");
			  			}
	|PLUG '(' var1 ')' '{' plug_stmt '}'
	|fun_call			{ //quadruple for fuction header 
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner5");

					//quadruple the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($1);
					q[quadindex++].r=strdup("funcall");
			  		}
	|stmt6 ID EQU var arith_oper var SEMI { 				        
					q[quadindex].arg1=strdup($4);
					q[quadindex].arg2=strdup($6);
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup($2);
					      }
	|stmt6 ID EQU var SEMI  {
					//quadruple for current cfg
					q[quadindex].arg1=strdup($4);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($2);
				}
	|stmt6 IF'('cond')' '{' stmt7 '}' control_if6 {//quadruple for the given cfg 

					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner6"))
					{
					sprintf(qno,"%d",quadindex);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($4);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
						   }
	|stmt6 IF'('cond')' '{' '}' control_if6
	|stmt6 INPUT'(' ip ')' SEMI {
					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("input");
				    }
	|stmt6 OUTPUT'(' op ')' SEMI{
					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("output");
				    }
	|stmt6 FOR '(' init SEMI cond SEMI iter ')' '{' stmt7 '}'{
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner6"))
					{
					sprintf(qno,"%d",quadindex+2);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($6);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					//if(strcmp($7,"null"))
					
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JTrue");
								}
	|stmt6 FOR '(' init SEMI cond SEMI iter ')' SEMI{
					sprintf(qno,"%d",quadindex+3);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JFalse");
					//if(strcmp($7,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
					sprintf(qno,"%d",quadindex-1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JTrue");
							}
	|stmt6 WHILE '(' cond ')' '{' stmt7 '}'{
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner6"))
					{
					sprintf(qno,"%d",quadindex+1);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($4);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for

					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("JTrue");
			  			}
	|stmt6 REPEAT '{' stmt7 '}' UNTIL '(' cond ')' SEMI{//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner6"))
					{
					break;
					}
					}
					for(i=temp;i<quadindex-1;i++)
					{
						q[i].r=q[i+1].r;
						q[i].arg1=q[i+1].arg1;
						q[i].arg2=q[i+1].arg2;
						q[i].op=q[i+1].op;
					}
					quadindex--;
					sprintf(qno,"%d",temp);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($8);
					q[quadindex++].r=strdup("JTrue");
								}
	|stmt6 PLUG '(' var1 ')' '{' plug_stmt '}' 
	|stmt6 fun_call { 

					//quadruple the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($2);
					q[quadindex++].r=strdup("funcall");
			 }
	;


//inner loop7
stmt7:ID EQU var arith_oper var SEMI{
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner6");
					//quadruple for current cfg
				        q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup($5);
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup($1); 

					}
	|ID EQU var SEMI	{	//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner6");

					//quadruple for current cfg
					q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($1);
				}
	|IF'('cond')' '{' stmt8 '}' control_if7  {

					//quadruple for fuction header

					//quadruple for current cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner7"))
					{
					sprintf(qno,"%d",quadindex+1);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($3);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					
					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner6");
						   }
	|IF'('cond')' '{' '}' control_if7
	|INPUT'(' ip ')' SEMI 	{		//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner6");

					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("input");
				}
	|OUTPUT'(' op ')' SEMI  {		//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner6");

					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("output");
				}
	|FOR '(' init SEMI cond SEMI iter ')' '{' stmt8 '}'{	
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner7"))
					{
					sprintf(qno,"%d",quadindex+3);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($5);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					//if(strcmp($7,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner6");
			  			}
	|FOR '(' init SEMI cond SEMI iter ')' SEMI{

					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner6");

					//quadruple for the given cfg
					sprintf(qno,"%d",quadindex+2);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup("JFalse");
					//iter1=strdup($7);
					//if(strcmp(iter1,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
							}
	|WHILE '(' cond ')' '{' stmt8 '}' {
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner7"))
					{
					sprintf(qno,"%d",quadindex+2);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($3);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for

					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner6");
			  			}
	|REPEAT '{' stmt8 '}' UNTIL '(' cond ')' SEMI  {
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner7"))
					{
					break;
					}
					}
					for(i=temp;i<quadindex-1;i++)
					{
						q[i].r=q[i+1].r;
						q[i].arg1=q[i+1].arg1;
						q[i].arg2=q[i+1].arg2;
						q[i].op=q[i+1].op;
					}
					quadindex--;
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($7);
					q[quadindex++].r=strdup("JTrue");

					for(i=quadindex;i>temp;i--)
					{
						q[i].r=q[i-1].r;
						q[i].arg1=q[i-1].arg1;
						q[i].arg2=q[i-1].arg2;
						q[i].op=q[i-1].op;
					}
					quadindex++;

					q[temp].arg1=strdup("undef");
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup("undef");
					q[temp].r=strdup("inner6");
			  			}
	|PLUG '(' var1 ')' '{' plug_stmt '}'
	|fun_call			{ //quadruple for fuction header 
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner6");

					//quadruple the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($1);
					q[quadindex++].r=strdup("funcall");
			  		}
	|stmt7 ID EQU var arith_oper var SEMI { 				        
					q[quadindex].arg1=strdup($4);
					q[quadindex].arg2=strdup($6);
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup($2);
					      }
	|stmt7 ID EQU var SEMI  {
					//quadruple for current cfg
					q[quadindex].arg1=strdup($4);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($2);
				}
	|stmt7 IF'('cond')' '{' stmt8 '}' control_if7 {//quadruple for the given cfg 

					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner7"))
					{
					sprintf(qno,"%d",quadindex);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($4);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
						   }
	|stmt7 IF'('cond')' '{' '}' control_if7
	|stmt7 INPUT'(' ip ')' SEMI {
					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("input");
				    }
	|stmt7 OUTPUT'(' op ')' SEMI{
					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("output");
				    }
	|stmt7 FOR '(' init SEMI cond SEMI iter ')' '{' stmt8 '}'{
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner7"))
					{
					sprintf(qno,"%d",quadindex+2);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($6);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for
					//if(strcmp($7,"null"))
					
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					
					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JTrue");
								}
	|stmt7 FOR '(' init SEMI cond SEMI iter ')' SEMI{
					sprintf(qno,"%d",quadindex+3);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JFalse");
					//if(strcmp($7,"null"))
					{
					q[quadindex].arg1=q[249].arg1;
					q[quadindex].arg2=q[249].arg2;
					q[quadindex].op=q[249].op;
					q[quadindex++].r=q[249].r;
					}
					sprintf(qno,"%d",quadindex-1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($6);
					q[quadindex++].r=strdup("JTrue");
							}
	|stmt7 WHILE '(' cond ')' '{' stmt8 '}'{
					//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner7"))
					{
					sprintf(qno,"%d",quadindex+1);
					q[temp].arg1=strdup(qno);
					q[temp].arg2=strdup("undef");
					q[temp].op=strdup($4);
					q[temp].r=strdup("JFalse");
					break;
					}
					}//end for

					sprintf(qno,"%d",temp+1);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("JTrue");
			  			}
	|stmt7 REPEAT '{' stmt8 '}' UNTIL '(' cond ')' SEMI{//quadruple for the given cfg
					for(temp=quadindex-1;temp>=0;temp--)
					{
					if(!strcmp(q[temp].r,"inner7"))
					{
					break;
					}
					}
					for(i=temp;i<quadindex-1;i++)
					{
						q[i].r=q[i+1].r;
						q[i].arg1=q[i+1].arg1;
						q[i].arg2=q[i+1].arg2;
						q[i].op=q[i+1].op;
					}
					quadindex--;
					sprintf(qno,"%d",temp);
					q[quadindex].arg1=strdup(qno);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($8);
					q[quadindex++].r=strdup("JTrue");
								}
	|stmt7 PLUG '(' var1 ')' '{' plug_stmt '}' 
	|stmt7 fun_call { 

					//quadruple the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($2);
					q[quadindex++].r=strdup("funcall");
			 }
	;


//inner loop8
stmt8:ID EQU var arith_oper var SEMI{
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner7");
					//quadruple for current cfg
				        q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup($5);
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup($1); 

					}
	|ID EQU var SEMI	{	//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner7");

					//quadruple for current cfg
					q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($1);
				}
	|INPUT'(' ip ')' SEMI{		//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner7");

					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("input");
				}
	|OUTPUT'(' op ')' SEMI  {		//quadruple for fuction header
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner7");

					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($3);
					q[quadindex++].r=strdup("output");
				} 
	|fun_call			{ //quadruple for fuction header 
					
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("undef");
					q[quadindex++].r=strdup("inner7");

					//quadruple the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($1);
					q[quadindex++].r=strdup("funcall");
			  		}
	|stmt8 ID EQU var arith_oper var SEMI { 				        
					q[quadindex].arg1=strdup($4);
					q[quadindex].arg2=strdup($6);
					q[quadindex].op=strdup($5);
					q[quadindex++].r=strdup($2);
					      }
	|stmt8 ID EQU var SEMI  {
					//quadruple for current cfg
					q[quadindex].arg1=strdup($4);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($2);
				}
	|stmt8 INPUT'(' ip ')' SEMI {
					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("input");
				    }
	|stmt8 OUTPUT'(' op ')' SEMI{
					//quadruple for current cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($4);
					q[quadindex++].r=strdup("output");
				    }
	|stmt8 fun_call { 

					//quadruple the given cfg
					q[quadindex].arg1=strdup("undef");
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup($2);
					q[quadindex++].r=strdup("funcall");
			 }
	;

//function call statement
fun_call:FNAME '(' param ')' SEMI {$$=strdup($1);}
	;
//plug statement
plug_stmt:
	|plug_stmt SOCKET DIGIT ':' stmt EJECT SEMI
	|plug_stmt SOCKET DIGIT ':' stmt
	|plug_stmt IMPLICIT ':' stmt
	;
//for loop
init:init COMMA ID EQU var{		q[quadindex].arg1=strdup($5);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($3);
		     	  }	 
	|ID EQU var {			q[quadindex].arg1=strdup($3);
					q[quadindex].arg2=strdup("undef");
					q[quadindex].op=strdup("=");
					q[quadindex++].r=strdup($1);
		     }	
	| 
	;

iter:ID ADD ADD      {			q[249].arg1=strdup($1);
					q[249].arg2=strdup("1");
					q[249].op=strdup("+");
					q[249].r=strdup($1);
					$$=strdup("1 ");strcat($$,$1);
		     }	
	|ID SUB SUB  {			q[249].arg1=strdup($1);
					q[249].arg2=strdup("1");
					q[249].op=strdup("-");
					q[249].r=strdup($1);
					$$=strdup("2 ");strcat($$,$1);
		     }	
	|ID EQU var1 arith_oper var1
	|  {$$=strdup("null");}
	;

//conditions
cond:var rel_oper var log_cond{ $$=strdup($1);
				strcat(strcat($$,$2),$3);//complete the conditons including logical operators
			      }
	|NOT var log_cond
	|{$$=strdup("undef");}
	;

log_cond:
	|log_oper var rel_oper var log_cond
	|log_oper NOT var log_cond
	;

log_oper:AND
	|OR
	;

rel_oper:NE{$$=strdup("<>");}
	|EQ{$$=strdup("=");}
	|GT{$$=strdup(">");}
	|LT{$$=strdup("<");}
	|GE{$$=strdup(">=");}
	|LE{$$=strdup("<=");}
	;

//expression that can appear in the statements



arith_oper:ADD {$$=strdup("+");}
	|SUB {$$=strdup("-");}
	|DIV {$$=strdup("/");}
	|MUL {$$=strdup("*");}
	|MOD {$$=strdup("%");}
	|POW {$$=strdup("^");}
	;

var:ID {$$=strdup($1);}
	|DIGIT {$$=strdup($1);}
	//|FNAME'('param')'
	;

param:param var1
	|
	;

var1:ID {$$=strdup($1);}
	|DIGIT {$$=strdup($1);}
	;

//statement for elseif or else statements
control_if:
	| control_if ELSE IF '(' cond ')' '{' stmt1 '}'
	| control_if ELSE '{' stmt1 '}'
	; 
control_if1:
	| control_if1 ELSE IF '(' cond ')' '{' stmt2 '}'
	| control_if1 ELSE '{' stmt2 '}'
	;
control_if2:
	| control_if2 ELSE IF '(' cond ')' '{' stmt3 '}'
	| control_if2 ELSE '{' stmt3 '}'
	;
control_if3:
	| control_if3 ELSE IF '(' cond ')' '{' stmt4 '}'
	| control_if3 ELSE '{' stmt4 '}'
	;
control_if4:
	| control_if4 ELSE IF '(' cond ')' '{' stmt5 '}'
	| control_if4 ELSE '{' stmt5 '}'
	;
control_if5:
	| control_if5 ELSE IF '(' cond ')' '{' stmt6 '}'
	| control_if5 ELSE '{' stmt6 '}'
	;
control_if6:
	| control_if6 ELSE IF '(' cond ')' '{' stmt7 '}'
	| control_if6 ELSE '{' stmt7 '}'
	;
control_if7:
	| control_if7 ELSE IF '(' cond ')' '{' stmt8 '}'
	| control_if7 ELSE '{' stmt8 '}'
	;

//arguments of input command
ip:ip COMMA ID {strcat(strcat($$," "),$3);}
	|ID {$$=strdup($1);}
	;

//arguments of ouput command
op:op ADD  ID {strcat(strcat($$," "),$3);}
	|op ADD STRING
	|ID {$$=strdup($1);}
	|STRING
	| {$$=strdup("");}
	;
%%
//function for generating quadruples

//check whether the identifier is already defined
void lookup(char *ident,char *type)
{
int i=0,k=0;
char *id;
for(i=0,k=0;*(ident+i)!='\0';i++)
{
	if(*(ident+i)!=' ')
	{
		*(id+k++)=*(ident+i);
	}
	else
	{
		*(id+k)='\0';
		vptr=vstart;
		flag=0;
		while(vptr!=NULL)
		{
			flag=0;
			if(!strcmp(vptr->fname,"undef"))
				if(!strcmp(vptr->vname,id))
				{
					yyerror(strcat(id," redeclared"));
					flag=1;
					break;
				}
			vptr=vptr->next;
		}//while
		if(flag==0)
		{
		vnew=(struct vsymb*)malloc(sizeof(struct vsymb));
		vnew->fname=strdup("undef");
		vnew->vname=strdup(id);
		vnew->vtype=strdup(type);
		if(!strcmp(type,"num"))vnew->vsize=2;
		else vnew->vsize=4;
		vnew->next=NULL;
			if(vstart==NULL)
			{
				vstart=vnew;
			}
			else
			{
				vptr=vstart;
				while(vptr->next!=NULL)vptr=vptr->next;
				vptr->next=vnew;
			}
		}//if flag=0
	k=0;
	}//else case
}//for loop
		
//checking the previous case(i.e. lookup operation) for the last symbol
		*(id+k)='\0';
		vptr=vstart;
		flag=0;
		while(vptr!=NULL)
		{
			flag=0;
			if(!strcmp(vptr->fname,"undef"))
				if(!strcmp(vptr->vname,id))
				{
					yyerror(strcat(id," redeclared"));
					flag=1;
					break;
				}
			vptr=vptr->next;
		}//while
		if(flag==0)
		{
		vnew=(struct vsymb*)malloc(sizeof(struct vsymb));
		vnew->fname=strdup("undef");
		vnew->vname=strdup(id);
		vnew->vtype=strdup(type);
		if(!strcmp(type,"num"))vnew->vsize=2;
		else vnew->vsize=4;
		vnew->next=NULL;
			if(vstart==NULL)
			{
				vstart=vnew;
			}
			else
			{
				vptr=vstart;
				while(vptr->next!=NULL)vptr=vptr->next;
				vptr->next=vnew;
			}
		}//if flag=0
}//lookup function

//checking undefined symbols
int var_present(char *id)
{
*vptr=*vstart;

while(vptr!=NULL)
{
flag=0;
printf("\n%s\t%s\n",id,vptr->vname);
if(!strcmp(vptr->vname,id))
	{
	flag=1;
	break;
	}
vptr=vptr->next;
}
if(flag)
return 0;//already defined
else
return 1;
}// end of function
		
//check whether the function has already been defined else its added to the function table 
int func_check(char *type,char *fname,char *arg)
{
fptr=fstart;

		while(fptr!=NULL)
		{
			flag=0;
			if(!strcmp(fptr->fname,fname))
				if(!strcmp(fptr->ftype,type))
					if(!strcmp(fptr->fargs,arg))
					{
						flag =1;break;
					}
			fptr=fptr->next;
		}
		if(flag==1)
		{
			return 1;
		}
		else
		{
		fnew=(struct fsymb*) malloc(sizeof(struct fsymb));
		fnew->fname=strdup(fname);
		fnew->ftype=strdup(type);
		fnew->fargs=strdup(arg);
		fnew->def=0;
		fnew->next=NULL;
		if(fstart==NULL)
		fstart=fnew;
		else
		{
			fptr=fstart;
			while(fptr->next!=NULL)fptr=fptr->next;
			fptr->next=fnew;
		}
		return 0;
		}
}


void yyerror(char *s)
{
printf("\nline no:%d::%s\n",lineno,s);
}


int main(int argc,char *argv[2])
{
int i=0;
char *temp;
int temp1;

temp=argv[1];

//opening input file
yyin=fopen(temp,"r");
if(yyin==NULL)
{
printf("\nfile not found\n");
exit(1);
}

for(i=0; *(argv[1]+i)!='.' && *(argv[1]+i)!='\0';i++);
if(*(argv[1]+i)=='.')
{
*(temp+i)='.';
*(temp+i+1)='q';
*(temp+i+2)='\0';
}
else if(*(argv[1]+i)=='\0')
{
*(temp+i)='.';
*(temp+i+1)='q';
*(temp+i+2)='\0';
}


//opening output file
//yyout=fopen(temp,"w");
//num='0';//used in icg

yyparse();

//FSYMB
printf("\n\nFUNCTION SYMBOL TABLE\n");
fptr=fstart;
while(fptr!=NULL)
{
printf("%s %s %s %d\n",fptr->ftype,fptr->fname,fptr->fargs,fptr->def);
fptr=fptr->next;
}

//VSYMB
printf("\n\nVARIABLE SYMBOL TABLE\n");
vptr=vstart;
while(vptr!=NULL)
{
printf("%s %s %s %d\n",vptr->fname,vptr->vname,vptr->vtype,vptr->vsize);
vptr=vptr->next;
}


//quadruples
printf("\n\nQUADRUPLES\n\n");
temp1=0;
while(temp1<quadindex)
{
printf("%d\t%s\t%s\t%s\t%s\n",temp1,q[temp1].r,q[temp1].op,q[temp1].arg1,q[temp1].arg2);
temp1++;
}


fclose(yyin);
//fclose(yyout);
//integrating with job's program
fp1=fopen("read.txt","w");
for(i=0;i<quadindex;i++)
{
	if(!strcmp(q[i].r,"function"))
		if(!strcmp(q[i].op,"end"))
			fprintf(fp1,"fend\n");
		else
			fprintf(fp1,"function %s\n",q[i].op);
	else if(!strcmp(q[i].r,"input"))
		fprintf(fp1,"input , %s\n",q[i].op);
	else if(!strcmp(q[i].r,"output"))
		fprintf(fp1,"output , %s\n",q[i].op);
	else if(!strcmp(q[i].r,"funcall"))
		fprintf(fp1,"funcall , %s\n",q[i].op);
	else
		fprintf(fp1,"%s , %s , %s , %s\n",q[i].op,q[i].arg1,q[i].arg2,q[i].r);
}
fclose(fp1);
assembly();
return 0;
}

void assembly()
{
   int i=0,j=-1,g,l=0,v=0,count=0,d,k=0,z,p=0,s=0;
   char c,b[100][100],m[100][100],id[100][100],id1[100][100],hi[100][100],fun[100][100],temp[20][20];
   FILE *fp;
   fp=fopen("read.txt","r");
   c=fgetc(fp);
   while(!feof(fp))
   { 
    // printf("%c",c);
    //b[i][j]=c;
    j++;
    if(c=='\n')
    {
     b[i][j]='\0';
     i++;
     j=-1;
    }
    b[i][j]=c;
    c=fgetc(fp); 
   }
   g=i;
 // printf("INTERMEDIATE CODE: \n ");
   //for(i=0;i<g;i++)
   //printf("\n %s",b[i]);
   i=0;
   fseek(fp, 0, SEEK_SET);
   printf("\n");
   while(fscanf(fp,"%s",m[i])!=EOF)
   {
     //printf("\n %s",m[i]);
     i++; 
   }
   z=i;
   for(j=0;j<i;j++)
   {
    v=0;
    if(m[j]=="function"||m[j]==","||m[j][0]=='#')
    continue;
    if(m[j][0]=='_')
    {
     for(v=0;v<=l;v++)
      {
       if(strcmp(id[v],m[j])!=0)
       strcpy(id1[l],m[j]);
       else 
       break;
      }
    l++;
    }
   }
   for(k=0;k<l;k++)
    {
       for(d=0;d<count;d++)
        {
          if(strcmp(id1[k],id[d])==0)
          break;
        }
      if(d==count)
       {
         strcpy(id[count],id1[k]);
         count++;
       }
    }  
   printf("\n  ASSEMBLY CODE \n ***************\n");
   printf(" \n ASSUME CS:CODE, DS:DATA\n DATA SEGMENT \n");
   for(i=0;i<count;i++)
   printf(" \n %s DW ? ",id[i]);
  
     for(i=0;i<z;i++)
	{
	if(strcmp(m[i],"input")==0)
	   { i++;
             for(p=0;p<count;p++)
		{ 
	        if(m[i][0]=='_')
		{s=s+1;
                 //printf("\ \n    MOV AH,01H \n    INT 21H",m[i]);
		 printf("\n MSG%d db 13, 10, 'enter the input for %s','$' ",s,m[i]);
		i++;}
                if(m[i][0]==',')
		{i++;
                continue;}
		if((m[i]=="+")||(m[i]=="-")||(m[i]=="*")||(m[i]=="/")||(m[i]=="=")||(m[i]=="function")||(m[i]=="output"))
		break;
               }
	  }
	} 
   printf("\n RESULT  db 13, 10, 'RESULT IS  ','$' " );  
   printf("\n DATA ENDS\n CODE SEGMENT\n");
   fseek(fp,0,SEEK_SET);
   i=0;
   c=fgetc(fp);
   for(i=0;i<z;i++)
    {
     if(strcmp(m[i],"function")==0)
      { 
	i++;
        if(strcmp(m[i],"#start")!=0)
	 {
	  for(j=1,k=0;m[i][j]!='\0';j++,k++)
          temp[0][k]=m[i][j];
	  temp[0][k]='\0';
          printf("proc %s",temp[0]);
          i++;
          do
     	   { 
		//printf("%s",m[i]);
     		 if (strcmp(m[i],"+")==0)
        	  { 
        	   printf("\n    MOV %s , %s",m[i+6],m[i+2]);
		   printf("\n    ADD %s , %s",m[i+6],m[i+4]);i=i+7;
		  }
      		 if (strcmp(m[i],"-")==0)
        	  {
        	   printf("\n    MOV %s , %s",m[i+6],m[i+2]);
		   printf("\n    SUB %s , %s",m[i+6],m[i+4]);
                   i=i+7;    
		  }
		 if (strcmp(m[i],"*")==0)
        	  {
        	   printf("\n    MOV %s , %s",m[i+6],m[i+2]);
		   printf("\n    MUL %s , %s",m[i+6],m[i+4]);
                   i=i+7;
		  }
		 if (strcmp(m[i],"/")==0)
        	  {
        	   printf("\n    MOV %s , %s",m[i+6],m[i+2]);
		   printf("\n    DIV %s , %s",m[i+6],m[i+4]);
                   i=i+7;
		  }
	         if (strcmp(m[i],"=")==0)
        	  {
        	   printf("\n    MOV %s , %s",m[i+6],m[i+2]);
                   i=i+7;
		  }
 		if(strcmp(m[i],"input")==0)
	        { i++;
	        s=0;
                for(p=0;p<count;p++)
		{ 
	        if(m[i][0]=='_')
		{s++;
                printf("\n\n    LEA DX,MSG%d \n    MOV AH,09H \n    INT 21H\n    MOV AH,01H \n    INT 21H",s);
		i++;}
                if(m[i][0]==',')
		{i++;
                continue;}
		if((m[i]=="+")||(m[i]=="-")||(m[i]=="*")||(m[i]=="/")||(m[i]=="=")||(m[i]=="function")||(m[i]=="output"))
		break;
               }
	    }  
            if(strcmp(m[i],"output")==0)
	   { i++;
             for(p=0;p<count;p++)
		{ 
	        if(m[i][0]=='_')
		{s++;
                printf("\n\n    LEA DX,RESULT \n    MOV AH,09H \n    INT 21H\n    MOV AH,4CH \n    INT 21H");
		i++;}
                if(m[i][0]==',')
		{i++;
                continue;}
		if((m[i]=="+")||(m[i]=="-")||(m[i]=="*")||(m[i]=="/")||(m[i]=="=")||(m[i]=="function")||(m[i]=="output"))
		break;
               }
	    }
          }while(strcmp(m[i],"fend")!=0); 
         printf("\nENDPROC %s \n",temp[0]);
	}
    else if(strcmp(m[i],"#start")==0)
    {
       printf("START: \n    MOV AX,@data \n    MOV DS,AX");
       i++;
       //if(m[i]=="+"||m[i]=="-"||m[i]=="*"||m[i]=="/"||m[i]=="=")
       //{
    	do
     	{
	  //printf("%s",m[i]);
      	   if (strcmp(m[i],"+")==0)
            { 
             printf("\n    MOV %s , %s",m[i+6],m[i+2]);
	     printf("\n    ADD %s , %s",m[i+6],m[i+4]);
	     i=i+7;
	    }
      	   if (strcmp(m[i],"-")==0)
            {
             printf("\n    MOV %s , %s",m[i+6],m[i+2]);
	     printf("\n    SUB %s , %s",m[i+6],m[i+4]);
             i=i+7;
	    }
           if (strcmp(m[i],"*")==0)
           {
            printf("\n    MOV %s , %s",m[i+6],m[i+2]);
	    printf("\n    MUL %s , %s",m[i+6],m[i+4]);
	    i=i+7;
	   }
          if (strcmp(m[i],"/")==0)
           {
            printf("\n    MOV %s , %s",m[i+6],m[i+2]);
	    printf("\n    DIV %s , %s",m[i+6],m[i+4]);
	    i=i+7;
	   }
          if (strcmp(m[i],"=")==0)
           {
            printf("\n    MOV %s , %s",m[i+6],m[i+2]);
            i=i+7;
	   }
          if(strcmp(m[i],"input")==0)
	   { i++;
	     s=0;
             for(p=0;p<count;p++)
		{ 
	        if(m[i][0]=='_')
		{s++;
                printf("\n\n    LEA DX,MSG%d \n    MOV AH,09H \n    INT 21H\n    MOV AH,01H \n    INT 21H",s);
		i++;}
                if(m[i][0]==',')
		{i++;
                continue;}
		if((m[i]=="+")||(m[i]=="-")||(m[i]=="*")||(m[i]=="/")||(m[i]=="=")||(m[i]=="function")||(m[i]=="output"))
		break;
               }
	  }  
	 if(strcmp(m[i],"output")==0)
	   { i++;
             for(p=0;p<count;p++)
		{ 
	        if(m[i][0]=='_')
		{s++;
                printf("\n\n    LEA DX,RESULT \n    MOV AH,09H \n    INT 21H\n    MOV AH,4CH \n    INT 21H");
		i++;}
                if(m[i][0]==',')
		{i++;
                continue;}
		if((m[i]=="+")||(m[i]=="-")||(m[i]=="*")||(m[i]=="/")||(m[i]=="=")||(m[i]=="function")||(m[i]=="output"))
		break;
               }
	    }
      }while((strcmp(m[i],"fend")!=0)); 
     printf("\nCODE ENDS \nEND START \n");
    }
   }
  } 
}


