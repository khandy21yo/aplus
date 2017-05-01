/*
 * File Layout for: BI.BI_RATELINE on 21-May-01
 *
 * CPT Time Rate Line File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bi_rateline_cdd
{
/* Element = RATETABLE
   Description = CPT Time Rate Table */
	char ratetable[6];
/* Element =
   Description = Time Interval */
	char interval[2];
/* Element =
   Description = Rate */
	double rate;
};
#pragma member_alignment restore
