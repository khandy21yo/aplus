/*
 * File Layout for: AD.AD_CEILINGONE on 21-May-01
 *
 * One Dimensional Optional Ceiling Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_ceilingone_cdd
{
/* Element = OPTTABLE
   Description = Depreciation optional table code */
	char opttable[6];
/* Element = DATE
   Description = Effective Date (YYYYMMDD) */
	char effdate[8];
/* Element =
   Description = Deprecition year */
	char dep_year[2];
/* Element =
   Description = Ceiling amount */
	double ceiling;
};
#pragma member_alignment restore
