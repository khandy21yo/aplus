/*
 * File Layout for: AD.AD_CONTROLOBJ on 21-May-01
 *
 * Object Control File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_controlobj_cdd
{
/* Element = DEP_OBJECT
   Description = Depreciation object */
	char dep_object[1];
/* Element = ERA
   Description = Era code */
	char era[2];
/* Element =
   Description = Last period updated */
	char lastper[6];
/* Element =
   Description = Last period depreciated */
	char lastdep[6];
/* Element = STATUS_FLAG
   Description = Status flag in the control files */
	char status_flag[1];
};
#pragma member_alignment restore
