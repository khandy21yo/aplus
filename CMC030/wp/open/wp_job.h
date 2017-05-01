/*
 * File Layout for: WP.WP_JOB on 21-May-01
 *
 * Manufacturing Work In Process Journal Header File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct wp_job_cdd
{
/* Element = SUBACCT
   Description = Job number */
	char job[10];
/* Element = DESCRIPTION
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
/* Element = LOCATION
   Description = Job Location */
	char location[4];
/* Element = OPERATOR
   Description = Operator */
	char operator[10];
/* Element = REFNO
   Description = Reference number */
	char refno[16];
/* Element =
   Description = Notes */
	char notes[2][40];
};
#pragma member_alignment restore
