/*
 * File Layout for: BI.BI_BILLL on 21-May-01
 *
 * Insurance Billing Journal Lines
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bi_billl_cdd
{
/* Element = INSURED
   Description = Insured number */
	char insured[10];
/* Element = PATIENT
   Description = Patient Number */
	char patient[10];
/* Element = DATE
   Description = Service Date (YYYYMMDD) */
	char servdate[8];
/* Element = CPT
   Description = Current Procedural Terminology Code */
	char cpt[5];
/* Element =
   Description = CPT Description */
	char description[40];
/* Element = DIAGNOSIS
   Description = Diagnosis Code */
	char diagnosis[6];
/* Element =
   Description = Service time (in hours) */
	double lenth;
/* Element =
   Description = Time Multiplier */
	int multiplier;
/* Element =
   Description = Amount */
	double amount;
};
#pragma member_alignment restore
