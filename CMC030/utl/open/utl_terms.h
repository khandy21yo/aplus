/*
 * File Layout for: UTL.UTL_TERMS on 21-May-01
 *
 * Terms Definition File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_terms_cdd
{
/* Element =
   Description = Terms Code */
	char code[2];
/* Element =
   Description = Description */
	char descr[40];
/* Element =
   Description = Discount percentage */
	double discount;
/* Element =
   Description = Days until become due */
	int duedays;
/* Element =
   Description = Day of month becomes due */
	char duedate[2];
/* Element =
   Description = Days discount is good for */
	int discountdays;
/* Element =
   Description = Day discount becomes due */
	char discountdate[2];
};
#pragma member_alignment restore
