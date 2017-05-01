/*
 * File Layout for: TV.TV_LOG_SOURCE on 21-May-01
 *
 * TV Log Source Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_log_source_cdd
{
/* Element =
   Description = Log source code */
	char source[4];
/* Element =
   Description = Log source description */
	char descr[40];
};
#pragma member_alignment restore
