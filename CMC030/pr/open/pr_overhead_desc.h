/*
 * File Layout for: PR.PR_OVERHEAD_DESC on 21-May-01
 *
 * Payroll Overhead Description File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_overhead_desc_cdd
{
/* Element =
   Description = Overhead key */
	char ovh_key[6];
/* Element =
   Description = Description */
	char descr[30];
/* Element =
   Description = Overhead rate */
	double rate;
/* Element =
   Description = Basis (1-hours, 2-amount) */
	char basis[1];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char prem_acct[18];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char ovrhd_acct[18];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char ex_acct[18];
};
#pragma member_alignment restore
