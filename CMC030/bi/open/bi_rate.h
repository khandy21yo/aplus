/*
 * File Layout for: BI.BI_RATE on 21-May-01
 *
 * Medical Service Rate
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bi_rate_cdd
{
/* Element = CPT
   Description = Current Procedural Terminology Code */
	char cpt[5];
/* Element = DATE
   Description = Effective Date (YYYYMMDD) */
	char effdate[8];
/* Element =
   Description = Flat Rate */
	double rate;
/* Element = RATETABLE
   Description = CPT Time Rate Table */
	char ratetable[6];
};
#pragma member_alignment restore
