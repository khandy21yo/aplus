/*
 * File Layout for: IC.IC_CONTROL on 21-May-01
 *
 * Inventory Control
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ic_control_cdd
{
/* Element = ERA
   Description = Era code */
	char era[2];
/* Element = PERIOD
   Description = Last period closed */
	char period[6];
/* Element = CONTROLFLAG
   Description = Status flag in the control files */
	char controlflag[1];
};
#pragma member_alignment restore
