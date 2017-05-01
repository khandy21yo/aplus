/*
 * File Layout for: UTL.UTL_MEASURE on 21-May-01
 *
 * Units Measure Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_measure_cdd
{
/* Element = UOM
   Description = Units of measure code */
	char code[2];
/* Element =
   Description = Description */
	char description[20];
};
#pragma member_alignment restore
