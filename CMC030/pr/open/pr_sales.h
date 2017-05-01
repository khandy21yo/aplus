/*
 * File Layout for: PR.PR_SALES on 21-May-01
 *
 * Sales File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct pr_sales_cdd
{
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = DEPARTMENT
   Description = Department number */
	char department[6];
/* Element = DATE
   Description = Date of sale (YYYYMMDD) */
	char saledate[8];
/* Element =
   Description = Sales amount */
	double amount;
};
#pragma member_alignment restore
