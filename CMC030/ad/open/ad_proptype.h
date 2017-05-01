/*
 * File Layout for: AD.AD_PROPTYPE on 21-May-01
 *
 * Property Type Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_proptype_cdd
{
/* Element = PROPTYPE
   Description = Property type code */
	char proptype[2];
/* Element =
   Description = Description */
	char description[40];
};
#pragma member_alignment restore
