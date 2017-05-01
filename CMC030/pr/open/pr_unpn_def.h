/*
 * File Layout for: PR.PR_UNPN_DEF on 21-May-01
 *
 * Payroll Union Pension Definition File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_unpn_def_cdd
{
/* Element =
   Description = Union Pension Code */
	char code[2];
/* Element =
   Description = Union Pension Type */
	char dtype[2];
/* Element =
   Description = Description */
	char descr[30];
/* Element =
   Description = Paid by Whom */
	char paid_by[1];
/* Element =
   Description = Employee Rate */
	double empe_rate;
/* Element =
   Description = Employer Rate */
	double empr_rate;
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char lia_acct[18];
/* Element =
   Description = Basis */
	char basis[1];
/* Element =
   Description = Deduction Code */
	char ded_code[2];
};
#pragma member_alignment restore
