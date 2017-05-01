/*
 * Parse.l - basic lexical analyzer
 *
 *	08/14/98 - Kevin Handy
 *		Debug the ugly thing
 */

%{
#include <stdio.h>
#include <string.h>

FILE* OutFile;

void ModPrint();
void ModScope();
void ModLong(const char* Text);
void ModQString(const char* Text);
void MungCdd(const char* yytext);

int RemFlag;

%}


%%
	/*
	 * Version Number
	 */
"\"V3.6 Calico\""	{ fprintf(OutFile, "%cV3.6a Calico%c", '"', '"'); 
			return 31; }

	/*
	 * CDD Definition
	 */
"\t%INCLUDE"([ \t]*)"%FROM"([ \t]*)"%CDD"([ \t]*)"\""([^\"]*)"\"" {
			MungCdd(yytext); return 100; }

	/*
	 * Quoted srings
	 */
"\""([^\"]*)"\""	{ fprintf(OutFile, "%s", yytext); return 3; }
"'"([^']*)"'"		{ fprintf(OutFile, "%s", yytext); return 3; }

	/*
	 * Comments
	 */
"!--\n"			{ ModPrint(); return 7; }
"!"[^\n\f\!]*"!"	{ fprintf(OutFile, "%s", yytext); return 8; }
"!"[^\n\f\!]*"&"[\n\f]	{ fprintf(OutFile, "%s", yytext); return 9; }
"!"[^\n\f\!]*/[\n\f]	{ fprintf(OutFile, "%s", yytext); return 10; }

[ \t]*"&\n"		{ fprintf(OutFile, " &\n", yytext); return 30; }


	/*
	 * Numbers, variables, etc.
	 */
[A-Z0-9:.%_$]*		{ fprintf(OutFile, "%s", yytext); return 24; }
"\n"			{ fprintf(OutFile, "\n"); return 26; }
.			{ fprintf(OutFile, "%s", yytext); return 26; }

%%

main(int argc, char *argv[])
{
	int test;
	int counter = 0;

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
		counter++;
/*		printf("Token: %d\n", test); */
	}

	fprintf(stderr, "Total scanned %d\n", counter);
}

void ModPrint()
{
	if (RemFlag)
	{
		fprintf(OutFile, "!--\n");
	}
	else
	{
		fprintf(OutFile, "!\n\t!\t08/22/98  - Kevin Handy\n"
			"\t!\t\t(V3.6a Calico)\n"
			"\t!\t\tUpdate to V3.6a coding standards\n"
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
void ModLong(const char* Text)
{
	char NewText[128];
	strcpy(NewText, Text + 1);
	NewText[strlen(NewText) - 2] = '\0';

	fprintf(OutFile, "%s%c", NewText, '%');
}

/*
 * Change string of 'xxx' format to "xxx" format
 */
void ModQString(const char* Text)
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


void MungCdd(const char* yytext)
{
	char NewText[128];		/* Used to grab the file name */
	char* ptr1;			/* Pointer to first dot */
	char* ptr2;			/* Pointer to second dot */

	ptr1 = strchr(yytext, '.');
	ptr2 = strchr(ptr1 + 1, '.');

	if ((ptr1 == NULL) || (ptr2 == NULL))
	{
		fprintf(OutFile, "'%s'\n", NewText);
	}
	else
	{
		/*
		 * Output old line
		 */
		fprintf(OutFile, " !%s\n", yytext);

		/*
		 * Start of include
		 */
		fprintf(OutFile, "\t%cINCLUDE \"SOURCE:[", '%');

		/*
		 * System
		 */
		strncpy(NewText, ptr1 + 1, ptr2 - ptr1 - 1);
		NewText[ptr2 - ptr1 - 1] = '\0';
		fprintf(OutFile, "%s.OPEN]", NewText);

		/*
		 * File
		 */
		strncpy(NewText, ptr2 + 1, strlen(ptr2) - 2);
		NewText[strlen(ptr2) - 2] = '\0';
		fprintf(OutFile, "%s.HB\"", NewText);
	}
}
