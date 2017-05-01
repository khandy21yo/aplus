/*
 * File Layout for: TK.TK_MODINFO on 21-May-01
 *
 * AUTHOR/MODIFICATION INFORMATION ABOUT A PROGRAM
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tk_modinfo_cdd
{
/* Element =
   Description = Program name */
	char progname[40];
/* Element = SYSTEMID
   Description = System id. */
	char system[6];
/* Element =
   Description = Name of Author/Modifier */
	char programmer[40];
/* Element = DATE
   Description = Modification Date (YYYYMMDD) */
	char moddate[8];
/* Element =
   Description = Author/Modifier Flag */
	char modflag[1];
/* Element =
   Description = Description of modification */
	char moddescr[21][80];
};
#pragma member_alignment restore
