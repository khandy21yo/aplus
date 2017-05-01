/*
 * File Layout for: UTL.UTL_EDI_DATAELEMENT on 21-May-01
 *
 * EDI Data Element Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_edi_dataelement_cdd
{
/* Element = CODE3
   Description = Data element reference number */
	char reference[6];
/* Element = TITLE
   Description = Data element title */
	char title[60];
/* Element =
   Description = Minumum Length */
	int mmin;
/* Element =
   Description = Maximum Length */
	int mmax;
/* Element = TYPE
   Description = Type */
	char ttyp[2];
};
#pragma member_alignment restore
