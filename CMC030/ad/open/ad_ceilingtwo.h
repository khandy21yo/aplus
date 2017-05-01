/*
 * File Layout for: AD.AD_CEILINGTWO on 21-May-01
 *
 * Two Dimensional Optional Ceiling Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_ceilingtwo_cdd
{
/* Element = OPTTABLE
   Description = Depreciation optional table code */
	char opttable[6];
/* Element = DATE
   Description = Effective Date (YYYYMMDD) */
	char effdate[8];
/* Element =
   Description = Depreciation years */
	char dep_year[2];
/* Element =
   Description = Ceiling amount */
	double ceiling[13];
};
#pragma member_alignment restore
