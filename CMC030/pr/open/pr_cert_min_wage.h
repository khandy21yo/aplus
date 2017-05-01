/*
 * File Layout for: PR.PR_CERT_MIN_WAGE on 21-May-01
 *
 * Certificate of Minimum Wage
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_cert_min_wage_cdd
{
/* Element = DATE
   Description = Date */
	char eff_date[8];
/* Element =
   Description = Minimum wage rate */
	double rate;
};
#pragma member_alignment restore
