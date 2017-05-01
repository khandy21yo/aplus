/*
 * File Layout for: BI.BI_PATIENTEX on 21-May-01
 *
 * Extra Information for Patients
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bi_patientex_cdd
{
/* Element = CUSTOMER
   Description = Customer Number */
	char cusnum[10];
/* Element = DATE
   Description = Birth Date (YYYYMMDD) */
	char bday[8];
};
#pragma member_alignment restore
