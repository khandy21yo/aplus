/*
 * File Layout for: UTL.UTL_EDI_CODELIST on 21-May-01
 *
 * EDI Code List
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_edi_codelist_cdd
{
/* Element = CODE3
   Description = Data Element Reference Number */
	char reference[6];
/* Element = CODE3
   Description = Code */
	char code[4];
/* Element = DESCR
   Description = Definition */
	char descr[60];
};
#pragma member_alignment restore
