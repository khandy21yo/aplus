/*
 * File Layout for: OE.OE_REASONACCT on 21-May-01
 *
 * Reason Account Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_reasonacct_cdd
{
/* Element =
   Description = Reason Code */
	char creason[2];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
};
#pragma member_alignment restore
