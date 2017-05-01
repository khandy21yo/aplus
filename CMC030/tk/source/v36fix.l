/*
 * Parse.l - basic lexical analyzer
 */

%{
#include <stdio.h>
#include <string.h>

FILE* OutFile;

void ModPrint();
void ModScope();
void ModLong(char* Text);
void ModQString(char* Text);

int RemFlag;

int XMax = 31;

%}


%%
	/*
	 * White-space characters
	 */
"\"V3.0\""		{ fprintf(OutFile, "%cV3.6 Calico%c", '"', '"'); return 1; }
"\"V3.3\""		{ fprintf(OutFile, "%cV3.6 Calico%c", '"', '"'); return 2; }
"\"V6.0\""		{ fprintf(OutFile, "%cV3.6 Calico%c", '"', '"'); RemFlag = -1; return 2; }
"\"V3.6\""		{ fprintf(OutFile, "%cV3.6 Calico%c", '"', '"'); 
			RemFlag = -1; return 31; }
"\"V3.6 Calico\""	{ fprintf(OutFile, "%s", yytext);
			RemFlag = -1; return 31; }

"\"FUNC_INCLUDE:SCOPE.COM\"" { ModScope(); return 27; }
"\"FUNC_INCLUDE:CODES.INC\"" { ModScope(); return 28; }
"\"FUNC_INCLUDE:SCOPEDEF.COM\"" {fprintf(OutFile, "%cFUNC_INCLUDE:FUNCTION.HB%c",
			'"', '"'); return 29; }

"\""([^\"]*)"\""	{ fprintf(OutFile, "%s", yytext); return 3; }
"'"([^']*)"'"		{ ModQString(yytext); return 4; }

[DBOX]?"\""([^\"]*)"\""[BCWLFDGHP]	{ fprintf(OutFile, "%s", yytext); return 5; }
"'"([^']*)"'"[L]	{ ModLong(yytext); return 6; }
[DBOX]?"'"([^']*)"'"[BCWLFDGHP]	{ fprintf(OutFile, "%s", yytext); return 6; }

"!--\n"			{ ModPrint(); return 7; }
"!"[^\n\f\!]*"!"	{ fprintf(OutFile, "%s", yytext); return 8; }
"!"[^\n\f\!]*"&"[\n\f]	{ fprintf(OutFile, "%s", yytext); return 9; }
"!"[^\n\f\!]*/[\n\f]	{ fprintf(OutFile, "%s", yytext); return 10; }

[ \t]*"&\n"		{ fprintf(OutFile, " &\n", yytext); return 30; }


	/*
	 * Single character operations
	 */
	/*
	 * Statements (usually)
	 */
"SCOPE.EXIT%"		{ fprintf(OutFile, "SCOPE::SCOPE_EXIT"); return 11; }
"SCOPE.TIMEOUT%"	{ fprintf(OutFile, "SCOPE::SCOPE_TIMEOUT"); return 12; }
"SMG_KBID"		{ fprintf(OutFile, "SCOPE::SMG_KBID"); return 13; }
"SMG_PBID"		{ fprintf(OutFile, "SCOPE::SMG_PBID"); return 14; }
"SMG_OPTION"		{ fprintf(OutFile, "SCOPE::SMG_OPTION"); return 15; }
"SMG_MESSAGE"		{ fprintf(OutFile, "SCOPE::SMG_MESSAGE"); return 16; }
"MACROFLAG"		{ fprintf(OutFile, "SCOPE::MACROFLAG"); return 17; }
"PRG_COMPANY"		{ fprintf(OutFile, "SCOPE::PRG_COMPANY"); return 18; }
"PRG_IDENT"		{ fprintf(OutFile, "SCOPE::PRG_IDENT"); return 19; }
"PRG_PROGRAM"		{ fprintf(OutFile, "SCOPE::PRG_PROGRAM"); return 20; }
"PRG_ITEM"		{ fprintf(OutFile, "SCOPE::PRG_ITEM"); return 21; }
"SCREEN.WIDTH"		{ fprintf(OutFile, "SCOPE::SCREEN_WIDTH"); return 22; }
"IMENU_LEVELS"		{ fprintf(OutFile, "SCOPE::IMENU_LEVELS"); return 23; }

	/*
	 * Numbers, variables, etc.
	 */
[A-Z0-9:%_$]*		{ fprintf(OutFile, "%s", yytext); return 24; }
"\n"			{ fprintf(OutFile, "\n"); return 26; }
.			{ fprintf(OutFile, "%s", yytext); return 26; }

%%

main(int argc, char *argv[])
{
	int test;

	if ((yyin = fopen( argv[1], "r" )) == 0)
	{
		printf("Bad source name\n");
		exit(1);
	}

	if ((OutFile = fopen( argv[1], "w" )) == 0)
	{
		printf("Bad output name\n");
		exit(1);
	}

	RemFlag = 0;
        
	while ((test = yylex()) != 0)
	{
/*		printf("Token: %d\n", test); */
	}
}

void ModPrint()
{
	if (RemFlag)
	{
		fprintf(OutFile, "!--\n");
	}
	else
	{
		fprintf(OutFile, "!\n\t!\t04/15/95 - Kevin Handy\n"
			"\t!\t\t(V3.6)\n"
			"\t!\t\tUpdate to V3.6 coding standards\n"
			"\t!--\n");
	}
	RemFlag = -1;
}

/*
 * Change include file
 */
void ModScope()
{
	fprintf(OutFile, "%cFUNC_INCLUDE:FUNCTION.HB%c\n\n"
		"\tMAP (SCOPE) SCOPE_STRUCT SCOPE\n", '"', '"');
}

/*
 * Change number of 'xx'L format to xx% format
 */
void ModLong(char* Text)
{
	char NewText[128];
	strcpy(NewText, Text + 1);
	NewText[strlen(NewText) - 2] = '\0';

	fprintf(OutFile, "%s%c", NewText, '%');
}

/*
 * Change string of 'xxx' format to "xxx" format
 */
void ModQString(char* Text)
{
	char NewText[128];
	strcpy(NewText, Text + 1);
	NewText[strlen(NewText) - 1] = '\0';

	if (strchr(NewText, '"') == 0)
	{
		fprintf(OutFile, "\"%s\"", NewText);
	}
	else
	{
		fprintf(OutFile, "'%s'", NewText);
	}
}
