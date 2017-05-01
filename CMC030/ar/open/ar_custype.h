/*
 * File Layout for: AR.AR_CUSTYPE on 21-May-01
 *
 * Customer Type Description File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ar_custype_cdd
{
/* Element =
   Description = Customer Type Code */
	char custype[2];
/* Element =
   Description = Customer Type Description */
	char description[40];
};
#pragma member_alignment restore
