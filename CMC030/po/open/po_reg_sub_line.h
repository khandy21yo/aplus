/*
 * File Layout for: PO.PO_REG_SUB_LINE on 21-May-01
 *
 * Purcahse Order Segister Sub-Line
 */

#pragma member_alignment save
#pragma nomember_alignment

struct po_reg_sub_line_cdd
{
/* Element = PO
   Description = Purchase order number */
	char po[10];
/* Element = LINE
   Description = Line */
	char po_line[4];
/* Element =
   Description = Action */
	char po_action[2];
/* Element = DATE
   Description = Date (YYYYMMDD) */
	char action_date[8];
/* Element =
   Description = Quantity */
	double qty;
/* Element =
   Description = Price */
	double price;
/* Element = SUBACCT
   Description = Sub account (job number) */
	char subacct[10];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
/* Element = BATCH
   Description = Batch number used for process (post,clos */
	char batch[6];
/* Element = POSTDATE
   Description = Date of posting (YYYYMMDD) */
	char postdate[8];
/* Element = POSTTIME
   Description = Time of posting */
	char posttime[6];
};
#pragma member_alignment restore
