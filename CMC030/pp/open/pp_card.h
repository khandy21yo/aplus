/*
 * File Layout for: PP.PP_CARD on 21-May-01
 *
 * Pacific Pride Card Number File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pp_card_cdd
{
/* Element = CUSTOMER
   Description = Customer Number */
	char cusnum[10];
/* Element =
   Description = Card Number */
	char card[8];
/* Element =
   Description = Card Type (1=drv,2=2nd drv,3=veh) */
	char ctype[1];
/* Element = DESCRIPTION
   Description = Description */
	char description[40];
/* Element =
   Description = Beginning Odometer Reading */
	double odometer;
/* Element =
   Description = Pacific Pride Customer Number */
	char syscus[08];
/* Element =
   Description = Discount Code */
	char discount[4];
};
#pragma member_alignment restore
