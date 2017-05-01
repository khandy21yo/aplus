/*
 * File Layout for: BI.BI_DIAG on 21-May-01
 *
 * Diagnosis Code Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bi_diag_cdd
{
/* Element = DIAGNOSIS
   Description = Diagnosis Code */
	char diagnosis[6];
/* Element =
   Description = Description */
	char description[40];
};
#pragma member_alignment restore
