/*
 * File Layout for: PD.PD_PRODTYPE on 21-May-01
 *
 * Product Type Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pd_prodtype_cdd
{
/* Element = CODE
   Description = Code */
	char code[2];
/* Element =
   Description = Type description */
	char description[20];
};
#pragma member_alignment restore
