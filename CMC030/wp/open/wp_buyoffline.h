/*
 * File Layout for: WP.WP_BUYOFFLINE on 21-May-01
 *
 * Manufacturing WIP Buyoff line file
 */

#pragma member_alignment save
#pragma nomember_alignment

struct wp_buyoffline_cdd
{
/* Element =
   Description = WIP Job Number */
	char job[10];
/* Element =
   Description = Line Number */
	char lline[4];
/* Element =
   Description = Completed Quantity */
	double compqty;
/* Element =
   Description = Cancelled Quantity */
	double cancelqty;
/* Element =
   Description = Unit Cost */
	double cost;
/* Element = DATE
   Description = Date of buyoff(YYYYMMDD) */
	char bdate[8];
/* Element = ACCOUNT
   Description = Buyoff General Ledger Account Number */
	char acct[18];
/* Element =
   Description = Finished Goods Id No. */
	char fg_id_no[10];
};
#pragma member_alignment restore
