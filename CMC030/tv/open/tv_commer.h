/*
 * File Layout for: TV.TV_COMMER on 21-May-01
 *
 * Commercial Order Header
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_commer_cdd
{
/* Element = TV_FRMNUM
   Description = Form Number */
	char frmnum[8];
/* Element = TV_CUSNUM
   Description = Customer Number */
	char cusnum[10];
/* Element =
   Description = Commercial CLASS (PO-political, etc.) */
	char class[4];
/* Element =
   Description = Source (LV-live, etc.) */
	char source[4];
/* Element =
   Description = Priority (1-5) */
	int priority;
/* Element =
   Description = Start date */
	char start_date[8];
/* Element =
   Description = End date */
	char end_date[8];
/* Element =
   Description = Cancelled (Y,N) */
	char cancelled[1];
/* Element = TV_CUSNUM
   Description = Agency Number */
	char agency_num[10];
/* Element =
   Description = Agency Commission Percentage */
	int agency_pct;
/* Element =
   Description = Date billed */
	char date_billed[8];
/* Element =
   Description = Match code */
	char match[6];
/* Element = TV_CUSNUM
   Description = Rep number */
	char rep_num[10];
/* Element =
   Description = Rep commission rate */
	int rep_pct;
/* Element = TV_SALNUM
   Description = Salesperson number */
	char sales_num[10];
/* Element =
   Description = Salesman Commission */
	int sales_pct;
/* Element = NAME
   Description = Name */
	char contact[25];
/* Element =
   Description = Billing type */
	char bill_type[2];
/* Element =
   Description = Spot seperation */
	char spot_sep[6];
/* Element =
   Description = Product Seperation */
	char prod_sep[6];
/* Element =
   Description = PO Number */
	char po[10];
/* Element =
   Description = Bill flag. 1-Normal. 2-Flat rate. */
	char bill_flag[1];
/* Element =
   Description = Confirmation printed (Y/N) */
	char confirm[1];
/* Element =
   Description = Confirmation number */
	int mod_num;
/* Element =
   Description = Single Conflict code for this order */
	char conflict[8];
/* Element =
   Description = Description */
	char descr[30];
};
#pragma member_alignment restore
