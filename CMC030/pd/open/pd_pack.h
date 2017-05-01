/*
 * File Layout for: PD.PD_PACK on 21-May-01
 *
 * Product Pack Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pd_pack_cdd
{
/* Element =
   Description = Pack Code */
	char code[4];
/* Element =
   Description = Packaging form */
	char form[3];
/* Element =
   Description = Packaging material */
	char material[2];
/* Element =
   Description = Volume unit of measure */
	char uom[2];
/* Element =
   Description = Units in one pack */
	double pack_factor;
/* Element =
   Description = Weight of one unit in case */
	double weight_factor;
/* Element =
   Description = Product units in one pack */
	double product_factor;
};
#pragma member_alignment restore
