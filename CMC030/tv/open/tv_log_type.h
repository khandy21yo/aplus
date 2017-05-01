/*
 * File Layout for: TV.TV_LOG_TYPE on 21-May-01
 *
 * TV Log Type Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_log_type_cdd
{
/* Element =
   Description = Log type code */
	char ltype[4];
/* Element =
   Description = Type description */
	char descr[40];
};
#pragma member_alignment restore
