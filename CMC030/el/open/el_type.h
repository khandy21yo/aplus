/*
 * File Layout for: EL.EL_TYPE on 21-May-01
 *
 * Equipment Ledger Type Description File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct el_type_cdd
{
/* Element =
   Description = Type */
	char ttype[2];
/* Element =
   Description = Description */
	char descr[30];
};
#pragma member_alignment restore
