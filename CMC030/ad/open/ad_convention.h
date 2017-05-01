/*
 * File Layout for: AD.AD_CONVENTION on 21-May-01
 *
 * Convention Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_convention_cdd
{
/* Element =
   Description = Convention code */
	char convention[2];
/* Element =
   Description = Description */
	char description[40];
/* Element =
   Description = Number months dep in the first year */
	int coeff;
/* Element =
   Description = Specification (regardless,..) */
	char specific[1];
};
#pragma member_alignment restore
