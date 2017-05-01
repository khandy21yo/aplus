/*
 * File Layout for: TK.TK_ELEMENT on 21-May-01
 *
 * Element Definition
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tk_element_cdd
{
/* Element =
   Description = Element */
	char element[39];
/* Element =
   Description = Element description */
	char descr[60];
/* Element =
   Description = Type */
	char etype[20];
/* Element =
   Description = Size */
	int esize;
/* Element =
   Description = Test file structure name */
	char teststruct[39];
};
#pragma member_alignment restore
