/*
 * File Layout for: TK.TK_FOREIGN on 21-May-01
 *
 * Foreign Key Definition
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tk_foreign_cdd
{
/* Element =
   Description = Sub-record structure */
	char substruct[50];
/* Element =
   Description = Field names used in the association */
	char fldnames[60];
/* Element =
   Description = Record structure */
	char struct[50];
/* Element =
   Description = Sub-record structure association */
	char subassociate[1];
/* Element =
   Description = Record Structure association */
	char associate[1];
};
#pragma member_alignment restore
