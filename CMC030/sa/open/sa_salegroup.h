/*
 * File Layout for: SA.SA_SALEGROUP on 21-May-01
 *
 * Salesman Group File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct sa_salegroup_cdd
{
/* Element = SALESMAN
   Description = Salesperson number */
	char salgroup[10];
/* Element = SALESMAN
   Description = Salesperson number */
	char salesman[10];
};
#pragma member_alignment restore
