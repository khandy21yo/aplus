/*
 * File Layout for: GL.GL_CATEGORY on 21-May-01
 *
 * Chart of Account Category Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct gl_category_cdd
{
/* Element = CATEGORY
   Description = Category */
	char category[4];
/* Element = DESCRIPTION6
   Description = Description */
	char descr[40];
/* Element =
   Description = Description */
	char titledesc[10];
};
#pragma member_alignment restore
