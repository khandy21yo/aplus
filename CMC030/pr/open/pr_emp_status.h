/*
 * File Layout for: PR.PR_EMP_STATUS on 21-May-01
 *
 * Payroll Employee Status File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_emp_status_cdd
{
/* Element =
   Description = Employee number */
	char empnum[10];
/* Element =
   Description = Datatype. FW=Fed WH, SW=State WH, etc. */
	char sttype[2];
/* Element =
   Description = State, city, county, school code */
	char code[2];
/* Element =
   Description = Withholding status */
	char ststatus[1];
/* Element = EXEMPT
   Description = Number of exemptions */
	int exempt;
/* Element = EXEMPT
   Description = Additional Number of exemptions */
	int addexempt;
};
#pragma member_alignment restore
