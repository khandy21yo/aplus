/*
 * File Layout for: BM.BM_PRODOPER on 21-May-01
 *
 * Product operation table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bm_prodoper_cdd
{
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = ITEMNUM
   Description = Oparation seq. number */
	char itemnum[4];
/* Element = OPERATION
   Description = Operation */
	char operation[8];
/* Element =
   Description = Number of hours */
	double hours;
/* Element = DATE
   Description = Effective Date (YYYYMMDD) */
	char effdate[8];
/* Element =
   Description = Status (A,O..) */
	char stat[1];
/* Element =
   Description = Number of hours on this level */
	double thishours;
};
#pragma member_alignment restore
