/*
 * File Layout for: WP.WP_CONTROL on 21-May-01
 *
 * WIP Controlling File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct wp_control_cdd
{
/* Element = ORDNUM
   Description = Order Number */
	char ordnum[10];
/* Element = DATE
   Description = Last Purge Date (YYYYMMDD) */
	char purgdate[8];
/* Element =
   Description = Activity Flag */
	char status_flag[1];
/* Element = REQNUM
   Description = Requisition Number */
	char reqnum[10];
/* Element = ACCOUNT
   Description = Inventory Material Price Variance */
	char invmatpvar[18];
/* Element = ACCOUNT
   Description = Inventory Material Usage Variance */
	char invmatuvar[18];
/* Element = ACCOUNT
   Description = Inventory Labor Rate Variance */
	char invlabrvar[18];
/* Element = ACCOUNT
   Description = Inventory Labor Efficiency Variance */
	char invlabevar[18];
/* Element = ACCOUNT
   Description = Inventory Buÿrdn Variance */
	char invburvar[18];
/* Element = ACCOUNT
   Description = Equipment Material Price Variance */
	char eqmatpvar[18];
/* Element = ACCOUNT
   Description = Equipment Material Usage Variance */
	char eqmatuvar[18];
/* Element = ACCOUNT
   Description = Equipment Labor Rate Variance */
	char eqlabrvar[18];
/* Element = ACCOUNT
   Description = Equipment Labor Efficiency Variance */
	char eqlabevar[18];
/* Element = ACCOUNT
   Description = Equipment Burden Variance */
	char eqburvar[18];
};
#pragma member_alignment restore
