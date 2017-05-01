/*
 * File Layout for: TV.TV_LOG_CLASS on 21-May-01
 *
 * TV Log Class Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_log_class_cdd
{
/* Element =
   Description = Class code */
	char class[04];
/* Element =
   Description = Class description */
	char descr[40];
};
#pragma member_alignment restore
