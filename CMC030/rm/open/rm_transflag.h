/*
 * File Layout for: RM.RM_TRANSFLAG on 21-May-01
 *
 * Transaction Flag for Worksheet
 */

#pragma member_alignment save
#pragma nomember_alignment

struct rm_transflag_cdd
{
/* Element = TRANSTYPE
   Description = Transaction type code */
	char transtype[2];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
};
#pragma member_alignment restore
