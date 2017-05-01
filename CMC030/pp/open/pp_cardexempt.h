/*
 * File Layout for: PP.PP_CARDEXEMPT on 21-May-01
 *
 * Pacific Pride Card Tax Exemption File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pp_cardexempt_cdd
{
/* Element = CUSTOMER
   Description = Customer Number */
	char cusnum[10];
/* Element = CARD
   Description = Card Number */
	char card[8];
/* Element = STATE
   Description = State */
	char state[2];
/* Element =
   Description = Authority */
	char authority[5];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
};
#pragma member_alignment restore
