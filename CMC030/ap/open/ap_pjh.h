/*
 * File Layout for: AP.AP_PJH on 21-May-01
 *
 * Purchase Journal Header
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ap_pjh_cdd
{
/* Element =
   Description = */
	char trankey[6];
/* Element =
   Description = */
	char vennum[10];
/* Element =
   Description = */
	char trankey_date[8];
/* Element =
   Description = */
	char invnum[15];
/* Element =
   Description = */
	char invdat[8];
/* Element =
   Description = */
	double invamt;
/* Element =
   Description = */
	char code_1099[2];
/* Element =
   Description = */
	double amt_1099;
/* Element =
   Description = */
	char use_job_num[10];
/* Element =
   Description = */
	double use_amt;
/* Element =
   Description = */
	char discdat[8];
/* Element =
   Description = */
	double discamt;
/* Element =
   Description = */
	char duedat[8];
/* Element =
   Description = */
	char ponum[10];
/* Element =
   Description = */
	char ap_acct[18];
/* Element =
   Description = */
	char cash_acct[18];
/* Element =
   Description = */
	char cknum[6];
/* Element =
   Description = */
	char ckdat[8];
/* Element =
   Description = */
	char descr[20];
/* Element =
   Description = */
	double ckamt;
};
#pragma member_alignment restore
