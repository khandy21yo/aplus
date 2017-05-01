/*
 * File Layout for: TK.TK_MODDATE on 21-May-01
 *
 * Check Modification History Dates
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tk_moddate_cdd
{
/* Element =
   Description = Program Editor's Name */
	char prog_editor[30];
/* Element =
   Description = Date Program was Writen */
	char date[8];
/* Element =
   Description = Name of Program that is being checked */
	char prog_name[40];
/* Element =
   Description = Program Editor who Modified program */
	char mod_editor[30];
/* Element =
   Description = Date Program was Modified */
	char mod_date[8];
/* Element =
   Description = Description of Modification */
	char mod_description[80];
/* Element =
   Description = Counter to keep modifications sorted */
	char xx_field[2];
};
#pragma member_alignment restore
