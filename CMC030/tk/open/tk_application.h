/*
 * File Layout for: TK.TK_APPLICATION on 21-May-01
 *
 * TK Application File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tk_application_cdd
{
/* Element =
   Description = Module number */
	char mnumb[15];
/* Element =
   Description = Module name */
	char mname[50];
/* Element =
   Description = Module description */
	char mdesc[50];
/* Element =
   Description = Module catagory */
	char mcata[8];
/* Element =
   Description = Module location */
	char mloca[64];
};
#pragma member_alignment restore
