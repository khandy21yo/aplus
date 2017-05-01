/*
 * File Layout for: PO.PO_NOTES on 21-May-01
 *
 * Notes file
 */

#pragma member_alignment save
#pragma nomember_alignment

struct po_notes_cdd
{
/* Element = CODE
   Description = Note Code */
	char notecode[2];
/* Element = DESCRIPTION
   Description = Description */
	char descr[40];
};
#pragma member_alignment restore
