/*
 * File Layout for: PR.PR_RATERANGE on 21-May-01
 *
 * Payroll Rate Range
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_raterange_cdd
{
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element =
   Description = Age */
	char age[3];
/* Element =
   Description = Minimum Rate allowed */
	double min_rate;
/* Element =
   Description = Maximum Rate Allowed */
	double max_rate;
};
#pragma member_alignment restore
