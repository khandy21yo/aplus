/*
 * File Layout for: BI.BI_INSUREX on 21-May-01
 *
 * Insurance Carrier Extra File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bi_insurex_cdd
{
/* Element = CUSTOMER
   Description = Customer Number */
	char cusnum[10];
/* Element =
   Description = Reference number */
	char reference[16];
/* Element =
   Description = Group Number */
	char groupnum[16];
};
#pragma member_alignment restore
