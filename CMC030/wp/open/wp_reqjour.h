/*
 * File Layout for: WP.WP_REQJOUR on 21-May-01
 *
 * Requisition Journal Header File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct wp_reqjour_cdd
{
/* Element = JOB
   Description = Job number */
	char job[10];
/* Element =
   Description = Job Line NUmber */
	char lline[4];
/* Element = DATE
   Description = Requisition Date (YYYYMMDD) */
	char reqdate[8];
/* Element = NOTES
   Description = Notes */
	char notes[2][40];
/* Element = OPERATOR
   Description = Operator */
	char operator[10];
};
#pragma member_alignment restore
