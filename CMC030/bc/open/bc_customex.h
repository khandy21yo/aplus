/*
 * File Layout for: BC.BC_CUSTOMEX on 21-May-01
 *
 * Customer Extra Information
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bc_customex_cdd
{
/* Element = CUSTOMER
   Description = Customer Number */
	char cusnum[10];
/* Element =
   Description = Customer Type */
	char custyp[2];
};
#pragma member_alignment restore
