/*
 * File Layout for: AD.AD_ASSTYPE on 21-May-01
 *
 * Asset Type
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_asstype_cdd
{
/* Element =
   Description = Asset type */
	char asset_type[2];
/* Element =
   Description = Description */
	char description[20];
};
#pragma member_alignment restore
