/*
 * File Layout for: PR.PR_OPER on 21-May-01
 *
 * Operations Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_oper_cdd
{
/* Element = OPERATION
   Description = Operation */
	char oper[8];
/* Element = DATE
   Description = Date */
	char effdate[8];
/* Element =
   Description = Piece rate */
	double piece_rate;
/* Element =
   Description = Hourly rate */
	double hour_rate;
};
#pragma member_alignment restore
