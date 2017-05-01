/*
 * File Layout for: AD.AD_RETIRED on 21-May-01
 *
 * Retired Asset
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_retired_cdd
{
/* Element = ASSET_NUM
   Description = Asset number */
	char asset_num[10];
/* Element = DATE
   Description = Date when asset has been retired */
	char ret_date[8];
/* Element =
   Description = Amount of disposition */
	double proceeds;
/* Element =
   Description = Notes */
	char notes[40];
};
#pragma member_alignment restore
