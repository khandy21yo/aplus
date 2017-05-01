/*
 * File Layout for: RM.RM_HISTORY on 21-May-01
 *
 * Restaurant Sales and Labor History
 */

#pragma member_alignment save
#pragma nomember_alignment

struct rm_history_cdd
{
/* Element =
   Description = Record type */
	char category[2];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = DATE
   Description = Date (MMDDYYYY) */
	char action_date[8];
/* Element = TIME
   Description = Time (HHMMSS) */
	char time_from[6];
/* Element = TIME
   Description = Time (HHMMSS) */
	char time_to[6];
/* Element =
   Description = Rate */
	double rate;
/* Element =
   Description = Store quantity/amount flag */
	char rec_type[1];
/* Element =
   Description = Amount/Quantity */
	int amount_qty[48];
};
#pragma member_alignment restore
