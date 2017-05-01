/*
 * File Layout for: PO.PO_CATEGORY on 21-May-01
 *
 * Category Definitions File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct po_category_cdd
{
/* Element = CODE
   Description = Code */
	char code[4];
/* Element = DESCRIPTION
   Description = Description */
	char descr[40];
};
#pragma member_alignment restore
