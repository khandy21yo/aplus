/*
 * File Layout for: GL.GL_USERJOUR on 21-May-01
 *
 * General Ledget User Defined Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_userjour_cdd
{
/* Element =
   Description = Journal Code */
	char jcode[4];
/* Element =
   Description = Journal line */
	char jline[4];
/* Element = DESCRIPTION
   Description = Description */
	char description[40];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
/* Element =
   Description = Dollar Amount */
	double dollars;
/* Element =
   Description = Units */
	double units;
/* Element = XREF
   Description = Customer/Vendor number */
	char xref[10];
/* Element = INVOICE
   Description = Invoice number */
	char invnum[8];
};
#pragma member_alignment restore
