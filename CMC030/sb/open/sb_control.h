/*
 * File Layout for: SB.SB_CONTROL on 21-May-01
 *
 * Subaccount Control File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct sb_control_cdd
{
/* Element = SYSTEM
   Description = Software System code */
	char system[2];
/* Element = PERIOD
   Description = Fiscal year (YYYY) and Cycle (PP) closed */
	char period[6];
/* Element = CONTROLFLAG
   Description = Status flag in the control files */
	char controlflag[1];
/* Element = DATE
   Description = Date (YYYYMMDD) */
	char cdate[8];
/* Element = TIME
   Description = Time (HHMMSS) */
	char ctime[6];
/* Element = BATCH
   Description = Batch number used for process (post,clos */
	char batch[6];
/* Element = SUBJECT
   Description = Subaccount subject */
	char subject[1];
/* Element =
   Description = Default number */
	char defnumber[10];
};
#pragma member_alignment restore
