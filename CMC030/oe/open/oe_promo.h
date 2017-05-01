/*
 * File Layout for: OE.OE_PROMO on 21-May-01
 *
 * Promotional Sales Master File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_promo_cdd
{
/* Element = REFNO
   Description = Reference number */
	char refpromo[16];
/* Element = DATE
   Description = From Date (YYYYMMDD) */
	char fromdate[8];
/* Element = DATE
   Description = To Date (YYYYMMDD) */
	char todate[8];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
/* Element = DESCRIPTION
   Description = Description */
	char description[40];
};
#pragma member_alignment restore
