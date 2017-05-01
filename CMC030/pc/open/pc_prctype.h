/*
 * File Layout for: PC.PC_PRCTYPE on 21-May-01
 *
 * Price Type Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pc_prctype_cdd
{
/* Element = CODE
   Description = Price type */
	char code[2];
/* Element = DESCRIPTION
   Description = Description */
	char description[20];
};
#pragma member_alignment restore
