/*
 * File Layout for: BI.BI_BILLH on 21-May-01
 *
 * Insurance Journal Header
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bi_billh_cdd
{
/* Element = INSURED
   Description = Insured number */
	char insured[10];
/* Element = PATIENT
   Description = Patient Number */
	char patient[10];
/* Element = STATIONMAN
   Description = Station man (operator) */
	char stationman[10];
/* Element = INVOICE
   Description = Invoice number */
	char invnum[8];
/* Element = DATE
   Description = Invoice Date (YYYYMMDD) */
	char invdate[8];
};
#pragma member_alignment restore
