/*
 * File Layout for: WP.WP_ISSJOUR on 21-May-01
 *
 * Material Issue Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct wp_issjour_cdd
{
/* Element = REQNUM
   Description = Requisition Number */
	char reqnum[10];
/* Element = JOB
   Description = Job number */
	char job[10];
/* Element = LINE
   Description = Line */
	char lline[4];
/* Element = OPERATOR
   Description = Operator */
	char operator[10];
};
#pragma member_alignment restore
