/*
 * File Layout for: AD.AD_TABLE on 21-May-01
 *
 * Optional Depreciation Tables
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_table_cdd
{
/* Element = OPTTABLE
   Description = Depreciation optional table code */
	char opttable[6];
/* Element = DATE
   Description = Effective Date (YYYYMMDD) */
	char effdate[8];
/* Element =
   Description = Depreciation life */
	char years[4];
/* Element =
   Description = Dimension (1 or 2) */
	char dimen[1];
};
#pragma member_alignment restore
