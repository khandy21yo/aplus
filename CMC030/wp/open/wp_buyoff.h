/*
 * File Layout for: WP.WP_BUYOFF on 21-May-01
 *
 * Manufacturing WIP Buyoff Journal Header File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct wp_buyoff_cdd
{
/* Element =
   Description = WIP Job Number */
	char job[10];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char acct[18];
/* Element = LOCATION
   Description = From Location number */
	char tolocation[4];
/* Element = OPERATOR
   Description = Operator */
	char operator[10];
};
#pragma member_alignment restore
