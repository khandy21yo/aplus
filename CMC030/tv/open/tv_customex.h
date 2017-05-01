/*
 * File Layout for: TV.TV_CUSTOMEX on 21-May-01
 *
 * TV Customer Extra Info File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_customex_cdd
{
/* Element =
   Description = Customer number */
	char cusnum[10];
/* Element =
   Description = Salesman number */
	char salesman[10];
/* Element =
   Description = Agency number */
	char agency[10];
/* Element =
   Description = Customer Type */
	char custyp[2];
/* Element =
   Description = Bill Type */
	char biltyp[2];
/* Element =
   Description = Representative */
	char rep[10];
};
#pragma member_alignment restore
