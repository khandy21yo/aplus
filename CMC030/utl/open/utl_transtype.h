/*
 * File Layout for: UTL.UTL_TRANSTYPE on 21-May-01
 *
 * Transaction Type Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_transtype_cdd
{
/* Element = TRANSTYPE
   Description = Transaction type code */
	char code[2];
/* Element =
   Description = Transaction type description */
	char description[20];
/* Element =
   Description = Classification */
	char class[2];
/* Element =
   Description = Transaction sign */
	char transsign[1];
};
#pragma member_alignment restore
