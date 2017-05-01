/*
 * File Layout for: PD.PD_LABEL on 21-May-01
 *
 * Product Label Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pd_label_cdd
{
/* Element = CODE
   Description = Label code */
	char code[4];
/* Element = DESCRIPTION
   Description = Description */
	char description[20];
};
#pragma member_alignment restore
