/*
 * File Layout for: AD.AD_JOURNAL on 21-May-01
 *
 * Depreciation Units Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_journal_cdd
{
/* Element = DEP_OBJECT
   Description = Depreciation object */
	char dep_object[1];
/* Element = DATE
   Description = Date */
	char action_date[8];
/* Element = STATIONMAN
   Description = Station man (operator) */
	char stationman[10];
};
#pragma member_alignment restore
