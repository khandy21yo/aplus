/*
 * File Layout for: OE.OE_CREASON on 02-Jul-03
 *
 * Reason Code Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_creason_cdd
{
/* Element =
   Description = */
	char creason[2];
/* Element =
   Description = DESCRIPTION */
	char descr[40];
/* Element = TAXABLE
   Description = Taxable Flag */
	char taxable[1];
/* Element =
   Description = Unused space */
	char cr_acct[17];
};
#pragma member_alignment restore
