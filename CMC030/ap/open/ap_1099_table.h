/*
 * File Layout for: AP.AP_1099_TABLE on 21-May-01
 *
 * 1099 Definition Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ap_1099_table_cdd
{
/* Element =
   Description = Table code */
	char code[2];
/* Element =
   Description = Table Description */
	char descr[20];
/* Element =
   Description = Base amount */
	double baseamt;
/* Element =
   Description = Form Number */
	char frmnum[1];
/* Element =
   Description = Form location */
	char frmloc[2];
};
#pragma member_alignment restore
