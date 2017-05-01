/*
 * File Layout for: MO.MO_MODEL on 21-May-01
 *
 * Model Base Definition File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct mo_model_cdd
{
/* Element =
   Description = Model Code */
	char modelcode[4];
/* Element =
   Description = Make Size */
	char msize[4];
/* Element =
   Description = Make Class */
	char class[4];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = PRODUCT
   Description = Box Product Number */
	char bproduct[14];
};
#pragma member_alignment restore
