/*
 * File Layout for: UTL.UTL_DOC_DEST on 21-May-01
 *
 * Printer Destinations
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_doc_dest_cdd
{
/* Element =
   Description = Name for combination */
	char pname[10];
/* Element =
   Description = Output device */
	char dest[20];
/* Element =
   Description = Printer type */
	char ptype[8];
};
#pragma member_alignment restore
