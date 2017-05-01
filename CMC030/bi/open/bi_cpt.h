/*
 * File Layout for: BI.BI_CPT on 21-May-01
 *
 * Current Procedural Terminology Codes
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bi_cpt_cdd
{
/* Element = CPT
   Description = Current Procedural Terminology Code */
	char cpt[5];
/* Element = DESCRIPTION
   Description = Description */
	char description[40];
/* Element = CPTTYPE
   Description = CPT Type */
	char cpttype[2];
/* Element = CATEGORY
   Description = CPT Category */
	char category[4];
/* Element =
   Description = Rate Flag (F,R,T) */
	char rateflag[1];
};
#pragma member_alignment restore
