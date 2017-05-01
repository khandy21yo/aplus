/*
 * File Layout for: PR.PR_EMP_MASTER on 21-May-01
 *
 * Employee Master File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_emp_master_cdd
{
/* Element =
   Description = Employee number */
	char empnum[10];
/* Element =
   Description = Employee name */
	char empname[30];
/* Element =
   Description = Street address */
	char add1[20];
/* Element =
   Description = Post office box */
	char add2[20];
/* Element =
   Description = City */
	char city[16];
/* Element =
   Description = FIPS postal abbreviation */
	char state[2];
/* Element =
   Description = Zip code */
	char zip[10];
/* Element =
   Description = Country */
	char country[6];
/* Element =
   Description = Telephone number */
	char phone[10];
/* Element =
   Description = Social security number */
	char ssn[12];
/* Element =
   Description = Alpha sort key */
	char sort[14];
/* Element =
   Description = Default sub account number for costing */
	char subacc[10];
/* Element =
   Description = Default account number */
	char acct[18];
/* Element =
   Description = Default trade */
	char trade[6];
/* Element =
   Description = Operation */
	char oper[8];
/* Element =
   Description = Default Union Code */
	char union[2];
/* Element = LOCATION
   Description = Location */
	char location[4];
/* Element =
   Description = Default department */
	char dept[6];
/* Element = WORK_CENTER
   Description = Work Center */
	char work_center[4];
/* Element =
   Description = Employee Skill */
	char emp_skill[6];
/* Element =
   Description = Employee grade */
	char emp_grade[2];
/* Element =
   Description = Disabled (Y/N) */
	char disabled[1];
/* Element =
   Description = Number of pay periods in a year */
	int payfreq;
/* Element =
   Description = Default rate type */
	char rate_type[1];
/* Element =
   Description = Default rate code */
	char rate_cde[2];
/* Element =
   Description = State unemployment code */
	char sui_sw[2];
/* Element =
   Description = Tax Package */
	char tax_pkg[2];
/* Element = CODE
   Description = Workmen comp code */
	char wc[6];
/* Element =
   Description = W2 = N, 1099 = Y */
	char w2_1099[1];
/* Element =
   Description = Employee birthday */
	char birth[8];
/* Element =
   Description = Employee hire date */
	char hireday[8];
/* Element =
   Description = Employee termination date */
	char termday[8];
/* Element = FLAG
   Description = Rehire this person flag (Y/N) */
	char rehire_flag[1];
/* Element =
   Description = M = Male, F = Female */
	char sex[1];
/* Element =
   Description = B=Black,H=Hisp,O=Orien,I=Indian,W=White */
	char race[1];
/* Element =
   Description = US Citizen (Y/N) */
	char uscit[1];
/* Element =
   Description = Work permit number */
	char wrkpermit[15];
/* Element =
   Description = Home country */
	char homcntry[6];
/* Element =
   Description = Active (Y/N) */
	char active_flag[1];
};
#pragma member_alignment restore
