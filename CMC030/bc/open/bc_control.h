/*
 * File Layout for: BC.BC_CONTROL on 21-May-01
 *
 * Billing to Customer Control File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bc_control_cdd
{
/* Element =
   Description = Last period closed */
	int lastperclose;
/* Element =
   Description = Year closed */
	char year[4];
/* Element = INVOICE
   Description = Invoice number */
	char inv_num[8];
};
#pragma member_alignment restore
