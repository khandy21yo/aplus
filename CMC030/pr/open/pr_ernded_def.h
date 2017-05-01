/*
 * File Layout for: PR.PR_ERNDED_DEF on 21-May-01
 *
 * Payroll ERNDED Definition File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_ernded_def_cdd
{
/* Element =
   Description = Payment,Deduction,noncompensaTion,Memo */
	char etype[1];
/* Element =
   Description = ERNDED code */
	char code[2];
/* Element =
   Description = Description of ERNDED */
	char descr[30];
/* Element =
   Description = Debit/Credit account for ERNDED */
	char drcr_acct[18];
/* Element =
   Description = Accrual account to credir if accrual */
	char accrual_acct[18];
/* Element =
   Description = Post to GL in summary (Y/N) */
	char summary[1];
/* Element =
   Description = Subject to federal taxes (Y/N) */
	char taxable_fwh[1];
/* Element =
   Description = Subject to fica employee taxes */
	char taxable_fie[1];
/* Element =
   Description = Subject to FICA employer taxes (Y/N) */
	char taxable_fir[1];
/* Element =
   Description = Subject to federal unempl. taxes (Y/N) */
	char taxable_fui[1];
/* Element =
   Description = Subject to state taxes (Y/N) */
	char taxable_swh[1];
/* Element =
   Description = Subject to state unempl. taxes (Y/N) */
	char taxable_sui[1];
/* Element =
   Description = Subject to other state taxes (Y/N) */
	char taxable_ost[1];
/* Element =
   Description = Subject to city taxes (Y/N) */
	char taxable_cwh[1];
/* Element =
   Description = Subject to county taxes (Y/N) */
	char taxable_dwh[1];
/* Element =
   Description = Subject to school taxes (Y/N) */
	char taxable_ewh[1];
/* Element =
   Description = Reportable to federal (Y/N) */
	char reportable_fwh[1];
/* Element =
   Description = Reportable to SOC. SEC. admin (Y/N0 */
	char reportable_fie[1];
/* Element =
   Description = Reportable to SOC. SEC. admin */
	char reportable_fir[1];
/* Element =
   Description = Reportable to federal unemployment */
	char reportable_fui[1];
/* Element =
   Description = Reportable to state */
	char reportable_swh[1];
/* Element =
   Description = Reportable to state employement */
	char reportable_sui[1];
/* Element =
   Description = Reportable to state */
	char reportable_ost[1];
/* Element =
   Description = Reportable to city */
	char reportable_cwh[1];
/* Element =
   Description = Reportable to county */
	char reportable_dwh[1];
/* Element =
   Description = Reportable to school */
	char reportable_ewh[1];
/* Element = FLAG
   Description = Subject to wc */
	char subj_wc[1];
/* Element =
   Description = Location to be displayed on W2's */
	char w2location[4];
};
#pragma member_alignment restore
