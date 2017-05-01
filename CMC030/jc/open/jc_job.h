/*
 * File Layout for: JC.JC_JOB on 21-May-01
 *
 * Job Description Master File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct jc_job_cdd
{
/* Element =
   Description = Subject type for Job "J" */
	char subject[1];
/* Element = SUBACCT
   Description = Job number */
	char job[10];
/* Element = DESCRIPTION6
   Description = Job Description */
	char descr[40];
/* Element =
   Description = Job Type */
	char ttype[2];
/* Element = CLASS
   Description = Job Class */
	char class[4];
/* Element = DATE
   Description = Creation Date (YYYYMMDD) */
	char bdate[8];
/* Element =
   Description = Job Status */
	char sstatus[1];
/* Element = DATE
   Description = Closed Date (YYYYMMDD) */
	char edate[8];
/* Element = LOCATION
   Description = Job Location */
	char location[4];
/* Element = OPERATOR
   Description = Operator */
	char operator[10];
/* Element = REFNO
   Description = Reference number */
	char refno[16];
/* Element = BATCH
   Description = Batch No */
	char batch[6];
/* Element = TIME
   Description = Post Time */
	char post_time[6];
/* Element = DATE
   Description = Post Date */
	char post_date[8];
};
#pragma member_alignment restore
