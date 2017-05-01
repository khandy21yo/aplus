/*
 * File Layout for: AP.AP_CDJ on 21-May-01
 *
 * Cash Disbursements Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ap_cdj_cdd
{
/* Element =
   Description = Vendor number */
	char vennum[10];
/* Element =
   Description = Transaction key */
	char trankey[6];
/* Element =
   Description = Invoice number */
	char invnum[15];
/* Element =
   Description = Invoice date */
	char invdat[8];
/* Element =
   Description = Invoice amount */
	double invamt;
/* Element =
   Description = Check amount */
	double ckamt;
/* Element =
   Description = Check number */
	char cknum[6];
/* Element =
   Description = Check date */
	char ckdat[8];
/* Element =
   Description = Check description */
	char ckdesc[20];
/* Element =
   Description = Discount date */
	char discdat[8];
/* Element =
   Description = Discount amount */
	double disamt;
/* Element =
   Description = Amount discount lost */
	double disc_lost_amt;
/* Element =
   Description = Discount lost to account */
	char disclost_acct[18];
/* Element =
   Description = Due date */
	char duedat[8];
/* Element =
   Description = Purchase order number */
	char ponum[10];
/* Element =
   Description = Accounts Payable Account */
	char ap_acct[18];
/* Element =
   Description = Cash Account */
	char cash_acct[18];
};
#pragma member_alignment restore
