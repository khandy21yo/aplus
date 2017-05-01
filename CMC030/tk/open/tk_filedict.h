/*
 * File Layout for: TK.TK_FILEDICT on 21-May-01
 *
 * File Structure Dictionary
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tk_filedict_cdd
{
/* Element =
   Description = File Name */
	char filename[39];
/* Element =
   Description = File Description */
	char descr[60];
/* Element =
   Description = System maintaining file */
	char system[3];
};
#pragma member_alignment restore
