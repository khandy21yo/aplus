/*
 * File Layout for: PP.PP_SITE_PRODUCT on 21-May-01
 *
 * Products Available at this Site
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pp_site_product_cdd
{
/* Element =
   Description = Host Number */
	char host[4];
/* Element =
   Description = site code */
	char site[4];
/* Element =
   Description = Site Type */
	char stype[1];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element =
   Description = Federal INTP */
	char fed_intp[1];
/* Element =
   Description = Federal Rate */
	double fed_rate;
/* Element = ACCOUNT
   Description = Federal General Ledger Account Number */
	char fed_account[18];
/* Element =
   Description = State INTP */
	char sta_intp[1];
/* Element =
   Description = State Rate */
	double sta_rate;
/* Element = ACCOUNT
   Description = State General Ledger Account Number */
	char sta_account[18];
/* Element =
   Description = County INTP */
	char cou_intp[1];
/* Element =
   Description = County Rate */
	double cou_rate;
/* Element = ACCOUNT
   Description = County General Ledger Account Number */
	char cou_account[18];
/* Element =
   Description = City INTP */
	char cty_intp[1];
/* Element =
   Description = City Rate */
	double cty_rate;
/* Element = ACCOUNT
   Description = City General Ledger Account Number */
	char cty_account[18];
/* Element =
   Description = Sales Tax INTP */
	char stx_intp[1];
/* Element =
   Description = Sales Tax Rate */
	double stx_rate;
/* Element = ACCOUNT
   Description = Sales Tax General Ledger Account Number */
	char stx_account[18];
/* Element = DATE
   Description = Effective Date (YYYYMMDD) */
	char effdate[8];
};
#pragma member_alignment restore
