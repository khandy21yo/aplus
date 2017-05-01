/*
 * File Layout for: BM.BM_MAXMIN on 02-Jun-00
 *
 * File to define MAX/MIN build information for reports
 */

struct bm_maxmin_cdd
{
/* Element = PRODUCT
   Description = Built Product Number */
	char product[14];
/* Element =
   Description = Grouping Indicator */
	char mgroup[4];
/* Element = QUANTITY
   Description = Maximum Quantity to build */
	double maxqty;
/* Element = QUANTITY
   Description = Minimum Quantity To Build */
	double minqty;
};
