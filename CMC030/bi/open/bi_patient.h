/*
 * File Layout for: BI.BI_PATIENT on 21-May-01
 *
 * Patient File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bi_patient_cdd
{
/* Element = INSURED
   Description = Insured */
	char insured[10];
/* Element = PATIENT
   Description = Patient Number */
	char patient[10];
/* Element = FAMRELAT
   Description = Family relation */
	char famrelat[1];
/* Element =
   Description = Insurance carrier */
	char insurance[10];
/* Element =
   Description = Insurance Group Number */
	char groupno[10];
};
#pragma member_alignment restore
