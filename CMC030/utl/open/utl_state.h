/*
 * File Layout for: UTL.UTL_STATE on 21-May-01
 *
 * State/Providence/...
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_state_cdd
{
/* Element = COUNTRY
   Description = Country */
	char country[2];
/* Element = STATE
   Description = State */
	char state[2];
/* Element =
   Description = Description */
	char descr[40];
/* Element =
   Description = FIPS Code */
	char fips[2];
};
#pragma member_alignment restore
