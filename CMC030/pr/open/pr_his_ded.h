/*
 * File Layout for: PR.PR_HIS_DED on 21-May-01
 *
 * Payroll Deduction History
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_his_ded_cdd
{
/* Element = EMPNUM
   Description = Employee number */
	char empnum[10];
/* Element = DATE
   Description = Date */
	char pr_end_date[8];
/* Element =
   Description = C-calculated tax, D-deduction */
	char dtype[1];
/* Element =
   Description = Tax or deduction code */
	char code[2];
/* Element = AMOUNT
   Description = Deduction/Tax amount */
	double amount;
/* Element =
   Description = Tax code */
	char tax_code[2];
/* Element =
   Description = Withholding status */
	char sstatus[1];
/* Element = EXEMPT
   Description = Number of exemptions */
	int exempt;
/* Element =
   Description = Update flag */
	int update_flag;
/* Element = BATCH
   Description = Batch number used for posting */
	char batch[6];
/* Element =
   Description = Taxable Basis */
	double taxable;
/* Element =
   Description = Reportable basis */
	double reportable;
/* Element = EXEMPT
   Description = Number of exemptions */
	int addexempt;
};
#pragma member_alignment restore
