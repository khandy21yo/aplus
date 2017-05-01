/*
 * File Layout for: PO.PO_ARCHIVE_SUB_LINE on 21-May-01
 *
 * Purchase Order Archive Sub Line
 */

#pragma member_alignment save
#pragma nomember_alignment

struct po_archive_sub_line_cdd
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
   Description = Our Quanity */
	double our_qty;
/* Element =
   Description = Vendor Quanity */
	double ven_qty;
/* Element =
   Description = Expected Price */
	double ven_rate;
/* Element = DATE
   Description = Expected Date (YYYYMMDD) */
	char receivedate[8];
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
