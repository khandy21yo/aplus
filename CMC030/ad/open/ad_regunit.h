/*
 * File Layout for: AD.AD_REGUNIT on 21-May-01
 *
 * Depreciation Unit Register
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_regunit_cdd
{
/* Element = ASSET_NUM
   Description = Asset number */
	char asset_num[10];
/* Element = DEP_OBJECT
   Description = Depreciation object */
	char dep_object[1];
/* Element = PERIOD
   Description = Period (YYYYPP) */
	char period[6];
/* Element = DATE
   Description = Date */
	char action_date[8];
/* Element =
   Description = Station man */
	char stationman[10];
/* Element =
   Description = Quantity, units */
	double quantity;
/* Element = DATE
   Description = Post date */
	char post_date[8];
/* Element = TIME
   Description = Time (HHMMSS) */
	char post_time[6];
/* Element = BATCH
   Description = Batch number used for posting */
	char batch[6];
};
#pragma member_alignment restore
