/*
 * File Layout for: BI.BI_CATEGORY on 21-May-01
 *
 * CPT Category
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bi_category_cdd
{
/* Element = CATEGORY
   Description = CPT Category */
	char category[4];
/* Element =
   Description = Description */
	char description[40];
};
#pragma member_alignment restore
