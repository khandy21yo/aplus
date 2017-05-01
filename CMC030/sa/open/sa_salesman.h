/*
 * File Layout for: SA.SA_SALESMAN on 21-May-01
 *
 * Salesman Address File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct sa_salesman_cdd
{
/* Element = SUBJECT
   Description = Subject type for salesman "S" */
	char subject[1];
/* Element =
   Description = Salesperson number */
	char salesman[10];
/* Element =
   Description = Salesman or Broker Name */
	char descr[40];
/* Element =
   Description = Job Type */
	char ttype[2];
/* Element =
   Description = Job Class */
	char class[4];
/* Element =
   Description = Onset Date */
	char bdate[8];
/* Element =
   Description = Current Job Status */
	char sstatus[1];
/* Element =
   Description = Termination Date */
	char edate[8];
/* Element = ADDRESS
   Description = Address Line */
	char add1[25];
/* Element = ADDRESS
   Description = Address Line */
	char add2[25];
/* Element = CITY
   Description = City */
	char city[15];
/* Element = STATE
   Description = State */
	char state[2];
/* Element = ZIP
   Description = Zip code */
	char zip[10];
/* Element = COUNTRY
   Description = Country */
	char country[2];
/* Element = PHONE
   Description = Phone number */
	char phone[10];
/* Element = INITIALS
   Description = Inititals */
	char initials[3];
/* Element = REGION
   Description = Region number */
	char region[2];
/* Element =
   Description = Commission Percentage */
	double commper;
};
#pragma member_alignment restore
