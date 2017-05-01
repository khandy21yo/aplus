/*
 * File Layout for: SA.SA_COMMACCT on 21-May-01
 *
 * Commision GL Accounts Table
 */

#pragma member_alignment save
#pragma nomember_alignment

struct sa_commacct_cdd
{
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = SALTYPE
   Description = Salesman Type */
	char saltype[2];
/* Element = ACCOUNT
   Description = Commision Expanse GL Account */
	char expacct[18];
/* Element = ACCOUNT
   Description = Commision Payable GL Account */
	char payacct[18];
};
#pragma member_alignment restore
