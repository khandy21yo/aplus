/*
 * File Layout for: PD.PD_CATEGORY on 21-May-01
 *
 * Product Category Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pd_category_cdd
{
/* Element = CODE
   Description = Code */
	char code[4];
/* Element = DESCRIPTION
   Description = Description */
	char description[20];
};
#pragma member_alignment restore
