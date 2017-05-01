/*
 * File Layout for: MO.MO_MODELLINE on 21-May-01
 *
 * Model Line Master File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_modelline_cdd
{
/* Element =
   Description = Model Code */
	char modelcode[4];
/* Element =
   Description = Make Size */
	char msize[4];
/* Element =
   Description = Make Class */
	char class[4];
/* Element =
   Description = Option Group */
	char optgroup[2];
/* Element =
   Description = Option Code */
	char optn[4];
};
#pragma member_alignment restore
