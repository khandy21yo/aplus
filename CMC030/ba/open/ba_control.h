/*
 * File Layout for: BA.BA_CONTROL on 21-May-01
 *
 * Billing to Agency Control File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ba_control_cdd
{
/* Element =
   Description = Last Period Closed */
	int lastperclose;
/* Element =
   Description = Year closed */
	char year[4];
/* Element = INVOICE
   Description = Invoice number */
	char inv_num[8];
/* Element =
   Description = Billing Number */
	char billnum[10];
};
#pragma member_alignment restore
