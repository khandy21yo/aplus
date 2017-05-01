/*
 * File Layout for: SB.SB_SUBACCOUNT on 21-May-01
 *
 * Subaccount Description File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct sb_subaccount_cdd
{
/* Element =
   Description = Subject type */
	char subject[1];
/* Element = SUBACCT
   Description = Sub account (job number) */
	char subaccount[10];
/* Element = DESCRIPTION6
   Description = Description */
	char descr[40];
/* Element =
   Description = Type */
	char ttype[2];
/* Element = CLASS
   Description = Class */
	char class[4];
/* Element = DATE
   Description = Date (YYYYMMDD) */
	char bdate[8];
/* Element =
   Description = Status */
	char sstatus[1];
/* Element = DATE
   Description = Date (YYYYMMDD) */
	char edate[8];
/* Element =
   Description = Extra Fields for a Subaccount */
	char extrafields[110];
};
#pragma member_alignment restore
