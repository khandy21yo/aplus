/*
 * File Layout for: AP.AP_VENDOR on 21-May-01
 *
 * Vendor Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ap_vendor_cdd
{
/* Element =
   Description = Vendor number */
	char vennum[10];
/* Element =
   Description = Vendor name */
	char vennam[40];
/* Element =
   Description = Vendor remittance address line 1 */
	char add1[25];
/* Element =
   Description = Vendor remittance address line 2 */
	char add2[21];
/* Element =
   Description = Vendor remittance city */
	char city[15];
/* Element =
   Description = Vendor remittance state */
	char state[2];
/* Element =
   Description = Vendor remittance zip */
	char zip[10];
/* Element =
   Description = Vendor remittance country */
	char country[8];
/* Element =
   Description = Vendor phone number */
	char phone[10];
/* Element =
   Description = Vendor address to send PO, line 1 */
	char poadd1[25];
/* Element =
   Description = Vendor address to send PO, line 2 */
	char poadd2[21];
/* Element =
   Description = Vendor city to send PO */
	char pocity[15];
/* Element =
   Description = Vendor state to send PO */
	char postate[2];
/* Element =
   Description = Vendor Zip Code to send PO */
	char pozip[10];
/* Element =
   Description = Vendor Country to send PO */
	char pocountry[8];
/* Element =
   Description = Vendor Phone number to send PO */
	char pophone[10];
/* Element =
   Description = Purge (Y/N) */
	char purge[1];
/* Element =
   Description = */
	char fedid[13];
/* Element =
   Description = 1099 Flag (Y/N) */
	char flg1099[1];
/* Element =
   Description = Number of days until payment due */
	int duedays;
/* Element =
   Description = Date payment is due */
	char duedate[2];
/* Element =
   Description = Number of days until discount is lost */
	int disdays;
/* Element =
   Description = Date discount is lost */
	char disdate[2];
/* Element =
   Description = Discount percentage */
	int discper;
/* Element =
   Description = Sort Key */
	char alpsrt[15];
};
#pragma member_alignment restore
