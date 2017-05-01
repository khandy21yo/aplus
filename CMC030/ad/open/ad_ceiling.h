/*
 * File Layout for: AD.AD_CEILING on 21-May-01
 *
 * Cost Recovery Ceiling Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_ceiling_cdd
{
/* Element = CEILTABLE
   Description = Ceiling table code */
	char ceiltable[6];
/* Element = DATE
   Description = Effective Date (YYYYMMDD) */
	char effdate[8];
/* Element =
   Description = Dimension (1 or 2) */
	char dimen[1];
};
#pragma member_alignment restore
