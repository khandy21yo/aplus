/*
 * File Layout for: AP.AP_OPEN_DIST on 21-May-01
 *
 * Accounts Payable Open Distribution
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ap_open_dist_cdd
{
/* Element =
   Description = Transaction key */
	char trankey[6];
/* Element =
   Description = Must be reset */
	char sline[4];
/* Element =
   Description = PO number */
	char ponum[10];
/* Element =
   Description = Must be reset */
	char po_line[4];
/* Element =
   Description = Account number */
	char acct[18];
/* Element =
   Description = Subaccount */
	char subacc[10];
/* Element =
   Description = Operation */
	char operation[8];
/* Element =
   Description = Units */
	double units;
/* Element =
   Description = Amount */
	double amount;
/* Element =
   Description = Discount amount */
	double discamt;
/* Element =
   Description = Flag is either (y/N) */
	char use_tax_flag[1];
/* Element =
   Description = Batch update number */
	char bthnum[6];
};
#pragma member_alignment restore
