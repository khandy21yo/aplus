/*
 * File Layout for: OE.OE_SALESTAX on 21-May-01
 *
 * Sales Tax Code Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct oe_salestax_cdd
{
/* Element = TAXCODE
   Description = Tax code */
	char taxcode[2];
/* Element = DESCRIPTION
   Description = Description */
	char jurisdiction[20];
/* Element = PERCENTAGE
   Description = State Tax Percentage */
	double statetax;
/* Element = ACCOUNT
   Description = GL Account Number for State */
	char stateacc[18];
/* Element = PERCENTAGE
   Description = City Sales Tax Percentage */
	double citytax;
/* Element = ACCOUNT
   Description = GL Account Number for City Tax */
	char cityacc[18];
/* Element = PERCENTAGE
   Description = County Sales TAx Percentage */
	double countytax;
/* Element = ACCOUNT
   Description = GL Account Number for County Tax */
	char countyacc[18];
};
#pragma member_alignment restore
