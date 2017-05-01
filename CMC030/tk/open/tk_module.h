/*
 * File Layout for: TK.TK_MODULE on 21-May-01
 *
 * Module Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tk_module_cdd
{
/* Element =
   Description = Module name */
	char modname[39];
/* Element =
   Description = */
	char description[60];
/* Element =
   Description = */
	char category[6];
/* Element =
   Description = Module extension */
	char extension[10];
/* Element =
   Description = Language module is written in */
	char language[8];
/* Element =
   Description = Directory Location */
	char directory[39];
/* Element =
   Description = */
	char modtype[4];
/* Element = MODNUM
   Description = Module id number */
	char modnum[6];
/* Element = DATE
   Description = Creation date */
	char cdate[8];
/* Element = TIME
   Description = Creation time */
	char ctime[6];
/* Element =
   Description = Is module sharable */
	char shareable[1];
};
#pragma member_alignment restore
