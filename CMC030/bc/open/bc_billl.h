/*
 * File Layout for: BC.BC_BILLL on 21-May-01
 *
 * Billing to Customer Billing Journal Line
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bc_billl_cdd
{
/* Element =
   Description = Order Number */
	char order[8];
/* Element =
   Description = Line Number */
	char lineno[4];
/* Element =
   Description = Amount Ordered */
	double ordamt;
/* Element =
   Description = Amount Shipped */
	double shpamt;
/* Element =
   Description = Amount Back-Ordered */
	double boamt;
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element =
   Description = Description */
	char descr[40];
/* Element = UOM
   Description = Units of measure code */
	char unitme[2];
/* Element =
   Description = Unit Price */
	double unipri;
/* Element =
   Description = Total Amount */
	double amount;
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
/* Element =
   Description = Line Type */
	char ltype[1];
/* Element =
   Description = Tax Type */
	char taxtyp[1];
};
#pragma member_alignment restore
