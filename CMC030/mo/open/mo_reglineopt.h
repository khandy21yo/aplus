/*
 * File Layout for: MO.MO_REGLINEOPT on 21-May-01
 *
 * Manufacturing Order Register Line Option File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_reglineopt_cdd
{
/* Element =
   Description = Order Number */
	char ordnum[10];
/* Element =
   Description = Model record line number */
	char lin[4];
/* Element =
   Description = Option Record line number */
	char optlin[4];
/* Element =
   Description = Option Group */
	char optgroup[2];
/* Element =
   Description = Option Code */
	char optn[4];
/* Element =
   Description = Order Quantity */
	double qty;
/* Element =
   Description = Cost Per Unit */
	double cost;
/* Element =
   Description = Price Per Unit */
	double price;
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element =
   Description = Transaction Code */
	char trantype[2];
/* Element =
   Description = Batch Number */
	char batch[6];
/* Element =
   Description = Ship No */
	char shipno[2];
/* Element = DATE
   Description = Date (YYYYMMDD) */
	char postdate[8];
/* Element = TIME
   Description = Time (HHMMSS) */
	char posttime[6];
/* Element =
   Description = Option Description */
	char optdescr[40];
/* Element = DATE
   Description = Transaction Date (YYYYMMDD) */
	char tdate[8];
/* Element = PERIOD
   Description = Fiscal year (YYYY) and Cycle (PP) */
	char period[6];
};
#pragma member_alignment restore
