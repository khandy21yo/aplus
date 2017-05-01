/*
 * File Layout for: PC.PC_DEAL_PRODUCT on 22-Apr-02
 *
 * Products in a deal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pc_deal_product_cdd
{
/* Element =
   Description = Deal number */
	char deal[20];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = AMOUNT
   Description = Price in deal */
	double price;
/* Element = PERCENTAGE
   Description = Percentage Discount */
	double percent;
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
};
#pragma member_alignment restore
