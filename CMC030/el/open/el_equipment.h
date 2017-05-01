/*
 * File Layout for: EL.EL_EQUIPMENT on 21-May-01
 *
 * Equipment Ledger Descriptons Master File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct el_equipment_cdd
{
/* Element =
   Description = Subject type for equipment "E" */
	char subject[1];
/* Element = SUBACCT
   Description = Equipment Number */
	char eqnum[10];
/* Element = DESCRIPTION
   Description = Description */
	char descr[40];
/* Element =
   Description = Equipment Type */
	char ttype[2];
/* Element = CLASS
   Description = Equipment Class */
	char class[4];
/* Element = DATE
   Description = Creation Date */
	char bdate[8];
/* Element =
   Description = Equipment Status */
	char sstatus[1];
/* Element = DATE
   Description = Closed Date */
	char edate[8];
/* Element = LOCATION
   Description = Equipment Location */
	char location[4];
/* Element = OPERATOR
   Description = Operator */
	char operator[10];
/* Element = REFNO
   Description = Reference Number */
	char refno[16];
};
#pragma member_alignment restore
