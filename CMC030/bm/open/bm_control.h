/*
 * File Layout for: BM.BM_CONTROL on 21-May-01
 *
 * BOM Control File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bm_control_cdd
{
/* Element =
   Description = STD Burden Hourly Rate */
	double burdenrate;
/* Element =
   Description = Component Types */
	char prodtype[20];
/* Element =
   Description = STD Labor Hourly Rate */
	double laborrate;
/* Element =
   Description = Raw Material Type */
	char rmat[20];
/* Element =
   Description = Burden percentage of Labor */
	double burdenperc;
};
#pragma member_alignment restore
