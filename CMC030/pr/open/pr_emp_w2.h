/*
 * File Layout for: PR.PR_EMP_W2 on 21-May-01
 *
 * W2 EMPLOYEE GENERATION
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_emp_w2_cdd
{
/* Element = EMPLOYEE
   Description = Employee number */
	char empnum[10];
/* Element = NAME
   Description = Name of a Person */
	char empname[30];
/* Element = ADDRESS
   Description = Address Line */
	char add1[20];
/* Element = ADDRESS
   Description = Address Line */
	char add2[20];
/* Element = CITY
   Description = City */
	char city[16];
/* Element = STATE
   Description = State */
	char state[2];
/* Element = ZIP
   Description = Zip code */
	char zip[10];
/* Element = COUNTRY
   Description = Country */
	char country[2];
/* Element = SSN
   Description = Social Security Number */
	char ssn[12];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element =
   Description = Array of earnings */
	double earnings[11];
/* Element =
   Description = Array of earnings codes */
	char earnings_code[11][2];
/* Element =
   Description = Array of Taxes witheld */
	double taxes[11];
/* Element =
   Description = Array of taxables */
	double taxable[11];
/* Element =
   Description = Array of tax codes */
	char taxes_code[11][2];
/* Element =
   Description = Array of state codes for taxes */
	char taxes_state[11][2];
/* Element =
   Description = Array of tax id numbers */
	char taxes_id[11][20];
/* Element =
   Description = Array of deductions */
	double deductions[11];
/* Element =
   Description = Array of deduction codes */
	char deductions_code[11][2];
};
#pragma member_alignment restore
