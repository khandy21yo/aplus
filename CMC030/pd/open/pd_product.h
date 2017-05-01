/*
 * File Layout for: PD.PD_PRODUCT on 21-May-01
 *
 * Product Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pd_product_cdd
{
/* Element = PRODUCT_NUM
   Description = Product number */
	char product_num[14];
/* Element =
   Description = Description */
	char description[40];
/* Element =
   Description = Product type */
	char prod_type[2];
/* Element =
   Description = Category */
	char category[4];
/* Element =
   Description = Unit of measure */
	char uom[2];
/* Element =
   Description = Unused... */
	char pack[4];
/* Element =
   Description = Label */
	char label[4];
/* Element = COSTMETHOD
   Description = Costing method */
	char method[4];
/* Element =
   Description = On set Date */
	char bdate[8];
/* Element =
   Description = Activity Status */
	char sstatus[1];
/* Element =
   Description = End Date */
	char edate[8];
/* Element =
   Description = Secondary Code */
	char secondary_code[10];
/* Element =
   Description = Weight of one unit */
	double weight;
/* Element =
   Description = Pack unit of measure */
	char bomuom[2];
/* Element =
   Description = Product units in one pack */
	double product_factor;
};
#pragma member_alignment restore
