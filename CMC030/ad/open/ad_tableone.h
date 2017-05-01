/*
 * File Layout for: AD.AD_TABLEONE on 21-May-01
 *
 * One Dimensional Optional Depreciation Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_tableone_cdd
{
/* Element = OPTTABLE
   Description = Depreciation optional table code */
	char opttable[6];
/* Element = DATE
   Description = Effective Date (YYYYMMDD) */
	char effdate[8];
/* Element =
   Description = Number of years */
	char years[4];
/* Element =
   Description = Deprecition year */
	char dep_year[2];
/* Element =
   Description = Percentage */
	int percentage;
};
#pragma member_alignment restore
