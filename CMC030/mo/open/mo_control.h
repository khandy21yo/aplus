/*
 * File Layout for: MO.MO_CONTROL on 21-May-01
 *
 * Manufacture Order Controling File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_control_cdd
{
/* Element = ORDNUM
   Description = Control Order Number */
	char ordnum[10];
/* Element = DATE
   Description = Last Close/Purge Date (YYYYMMDD) */
	char purgdate[8];
/* Element =
   Description = Activity status */
	char status_flag[1];
/* Element = INVOICE
   Description = Invoice number */
	char last_inv[8];
};
#pragma member_alignment restore
