/*
 * File Layout for: OE.OE_CONTROL on 21-May-01
 *
 * Sales Order Controlling File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_control_cdd
{
/* Element = ORDNUM
   Description = Control Order Number */
	char ordnum[10];
/* Element = DATE
   Description = Last Purge Date (YYYYMMDD) */
	char purgdate[8];
/* Element =
   Description = Activity status */
	char status_flag[1];
/* Element =
   Description = For the memo form to communicate with */
	char last_memo[8];
/* Element =
   Description = For the invoice form to communicate with */
	char last_inv[8];
/* Element =
   Description = Aging Days in the Period */
	int ageper[5];
/* Element =
   Description = Name of Backorder Period */
	char agenam[5][16];
/* Element = PRICETYPE
   Description = Price type for Miscellaneous Charges */
	char misctype[2];
/* Element = FLAG
   Description = Misc Charges Flag */
	char miscflag[1];
/* Element = FLAG
   Description = Display Price (Y/N) */
	char dsplprice[1];
/* Element = FLAG
   Description = Display Balances (Y/N) */
	char dsplqty[1];
/* Element = PRICETYPE
   Description = List Price Type Code */
	char listcode[2];
};
#pragma member_alignment restore
