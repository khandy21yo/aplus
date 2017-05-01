/*
 * File Layout for: AP.AP_PJL on 21-May-01
 *
 * Purchase Journal Line File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ap_pjl_cdd
{
/* Element =
   Description = */
	char trankey[6];
/* Element =
   Description = Must be reset */
	char sline[4];
/* Element =
   Description = */
	char ponum[10];
/* Element =
   Description = */
	char po_line[4];
/* Element =
   Description = */
	char acct[18];
/* Element =
   Description = */
	char subacc[10];
/* Element =
   Description = */
	char operation[8];
/* Element =
   Description = */
	double units;
/* Element =
   Description = */
	double amount;
/* Element =
   Description = */
	double discamt;
/* Element =
   Description = Flag it either (y/N) */
	char use_tax_flag[1];
};
#pragma member_alignment restore
