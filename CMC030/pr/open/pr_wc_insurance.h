/*
 * File Layout for: PR.PR_WC_INSURANCE on 21-May-01
 *
 * Workman Comp Insurance Rate
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_wc_insurance_cdd
{
/* Element =
   Description = */
	char code[6];
/* Element =
   Description = */
	char state[2];
/* Element =
   Description = Insurance type (liability ins, etc.) */
	char ins_type[2];
/* Element = DATE
   Description = Date (YYYYMMDD) */
	char effdat[8];
/* Element =
   Description = Method (1-hour, 2-day) */
	char method[1];
/* Element =
   Description = Employee rate */
	double emple_rate;
/* Element =
   Description = Employer rate */
	double emplr_rate;
/* Element =
   Description = */
	double maxqhours;
};
#pragma member_alignment restore
