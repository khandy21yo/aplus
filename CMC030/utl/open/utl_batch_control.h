/*
 * File Layout for: UTL.UTL_BATCH_CONTROL on 21-May-01
 *
 * Batch Control for Posting.
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_batch_control_cdd
{
/* Element =
   Description = */
	char batch[6];
/* Element =
   Description = */
	char programname[40];
/* Element =
   Description = */
	char bfile[8];
/* Element =
   Description = */
	char dstart[8];
/* Element =
   Description = */
	char tstart[6];
/* Element =
   Description = */
	char ustatus[1];
/* Element =
   Description = */
	char descr[20];
/* Element =
   Description = */
	char utlfile[7];
/* Element =
   Description = */
	char u1file[7];
};
#pragma member_alignment restore
