/*
 * File Layout for: IC.IC_LAYER on 21-May-01
 *
 * Product Cost Layer
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ic_layer_cdd
{
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = DATE
   Description = Date (YYYYMMDD) */
	char transdate[8];
/* Element =
   Description = Inventory cost */
	double cost;
/* Element =
   Description = Quantity */
	double quantity;
/* Element =
   Description = Check number */
	char check[6];
/* Element =
   Description = Vendor Name */
	char vendoralf[15];
/* Element =
   Description = Invoice Number */
	char invoice[15];
/* Element = DATE
   Description = Post Date (YYYYMMDD) */
	char postdate[8];
/* Element = TIME
   Description = Post Time (HHMMSS) */
	char posttime[6];
/* Element = BATCH
   Description = Batch number used for posting */
	char batch[6];
};
#pragma member_alignment restore
