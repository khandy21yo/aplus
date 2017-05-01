/*
 * File Layout for: AD.AD_CLASS on 21-May-01
 *
 * Asset Classes
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_class_cdd
{
/* Element =
   Description = Asset class */
	char assclass[6];
/* Element =
   Description = Description */
	char description[40];
/* Element =
   Description = Class life */
	int life;
/* Element =
   Description = General depreciation system */
	int gds;
/* Element =
   Description = Alternative depreciation system */
	int ads;
};
#pragma member_alignment restore
