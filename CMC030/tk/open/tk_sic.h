/*
 * File Layout for: TK.TK_SIC on 21-May-01
 *
 * Standard Industrial Classification Codes
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tk_sic_cdd
{
/* Element = SIC
   Description = Standard industrial classification code */
	char sic[6];
/* Element =
   Description = Description */
	char description[40];
/* Element =
   Description = Division */
	char division[2];
/* Element =
   Description = Business type */
	char businesstype[2];
};
#pragma member_alignment restore
