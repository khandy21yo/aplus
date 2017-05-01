/*
 * File Layout for: AD.AD_TABLETWO on 21-May-01
 *
 * Two Dimensional Optional Depreciation Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_tabletwo_cdd
{
/* Element = OPTTABLE
   Description = Depreciation optional table code */
	char opttable[6];
/* Element = DATE
   Description = Effective Date (YYYYMMDD) */
	char effdate[8];
/* Element =
   Description = Number of year */
	char years[4];
/* Element =
   Description = Depreciation years */
	char dep_year[2];
/* Element =
   Description = Percentage */
	int percentage[13];
};
#pragma member_alignment restore
