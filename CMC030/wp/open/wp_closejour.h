/*
 * File Layout for: WP.WP_CLOSEJOUR on 21-May-01
 *
 * Job Close Journal Header
 */

#pragma member_alignment save
#pragma nomember_alignment

struct wp_closejour_cdd
{
/* Element = JOB
   Description = Job number */
	char job[10];
/* Element = DATE
   Description = Closing Date (YYYYMMDD) */
	char closedate[8];
/* Element = OPERATOR
   Description = Operator */
	char operator[10];
/* Element =
   Description = STD Inventory Parts */
	double stdparts;
/* Element =
   Description = Actual Inventory Parts */
	double actparts;
/* Element =
   Description = STD Raw Material */
	double stdrawmat;
/* Element =
   Description = Actual raw Material */
	double actrawmat;
/* Element =
   Description = STD Labor */
	double stdlabor;
/* Element =
   Description = Actual labor */
	double actlabor;
/* Element =
   Description = Standard Burden */
	double stdburden;
/* Element =
   Description = Actual Burden */
	double actburden;
/* Element =
   Description = Variance Flag */
	char varflag[1];
};
#pragma member_alignment restore
