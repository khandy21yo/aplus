/*
 * File Layout for: AD.AD_DEPCLASS on 21-May-01
 *
 * Depreciation Class Definition Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_depclass_cdd
{
/* Element = DEPCLASS
   Description = Depreciation class code */
	char depclass[4];
/* Element =
   Description = Description */
	char description[40];
/* Element = PROPTYPE
   Description = Property type code */
	char proptype[2];
/* Element = DEPMETHOD
   Description = Depreciation method */
	char depmethod[4];
/* Element =
   Description = Recovery period */
	char years[4];
/* Element = DEPCONV
   Description = First year convention code */
	char fyconv[2];
/* Element = DEPCONV
   Description = Disposition convention code */
	char dyconv[2];
/* Element = OPTTABLE
   Description = Depreciation optional table code */
	char opttable[6];
/* Element = CEILTABLE
   Description = Ceiling table code */
	char ceiltable[6];
/* Element =
   Description = Adjust basis by salvage (Y,N) */
	char salvfactor[1];
/* Element =
   Description = Adjust basis by bonus (Y,N) */
	char bonusfactor[1];
/* Element =
   Description = Adjust basis by ITC (Y,N) */
	char itcfactor[1];
};
#pragma member_alignment restore
