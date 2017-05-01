/*
 * File Layout for: BI.BI_CPTTYPE on 21-May-01
 *
 * CPT Type and Account Number Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bi_cpttype_cdd
{
/* Element = CPTTYPE
   Description = CPT Type */
	char cpttype[2];
/* Element =
   Description = Description */
	char description[40];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
};
#pragma member_alignment restore
