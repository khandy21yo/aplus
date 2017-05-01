/*
 * File Layout for: MO.MO_OPTION on 21-May-01
 *
 * Option Definition Master File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_option_cdd
{
/* Element =
   Description = Option Group */
	char optgroup[2];
/* Element =
   Description = Option Code */
	char optn[4];
/* Element =
   Description = Option Description */
	char descr[40];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
};
#pragma member_alignment restore
