/*
 * File Layout for: BM.BM_RELATION on 21-May-01
 *
 * Product Structure File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bm_relation_cdd
{
/* Element = PRODUCT_NUM
   Description = Product number */
	char product[14];
/* Element =
   Description = Item number */
	char itemnum[4];
/* Element = PRODUCT_NUM
   Description = Product component */
	char component[14];
/* Element =
   Description = Quantity */
	double quantity;
/* Element =
   Description = Operation */
	char operation[8];
/* Element =
   Description = Scrap percentage */
	int scrap;
};
#pragma member_alignment restore
