/*
 * File Layout for: PP.PP_CONTROL on 21-May-01
 *
 * Pacific Pride Control File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pp_control_cdd
{
/* Element = CUSTOMER
   Description = System Customer Number */
	char cusnum[10];
/* Element = ACCOUNT
   Description = A/R General Ledger Account Number */
	char ar_account[18];
/* Element =
   Description = Host Number */
	char host_num[4];
/* Element =
   Description = Dsicount Days */
	char dis_days[2];
/* Element = DATE
   Description = Date (YYYYMMDD) */
	char invdate[8];
/* Element =
   Description = Last Invoice Number */
	char last_inv[8];
};
#pragma member_alignment restore
