/*
 * File Layout for: PO.PO_ACKNOWLEDGE on 21-May-01
 *
 * Acknowledgement Codes
 */

#pragma member_alignment save
#pragma nomember_alignment

struct po_acknowledge_cdd
{
/* Element =
   Description = Acknowledgement Code */
	char code[2];
/* Element =
   Description = Description */
	char descr[40];
};
#pragma member_alignment restore
