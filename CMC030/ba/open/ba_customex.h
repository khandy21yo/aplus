/*
 * File Layout for: BA.BA_CUSTOMEX on 21-May-01
 *
 * Customer File Extra Information
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ba_customex_cdd
{
/* Element = CUSTOMER
   Description = Customer Number */
	char cusnum[10];
/* Element =
   Description = Customer type */
	char custyp[2];
};
#pragma member_alignment restore
