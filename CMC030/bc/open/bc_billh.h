/*
 * File Layout for: BC.BC_BILLH on 21-May-01
 *
 * Billing to Customer Billing Journal Header
 */

#pragma member_alignment save
#pragma nomember_alignment

struct bc_billh_cdd
{
/* Element =
   Description = Order number */
	char order[8];
/* Element = CUSTOMER
   Description = Customer Number */
	char cusnum[10];
/* Element = CUSTOMER
   Description = Customer Number */
	char shpnum[10];
/* Element =
   Description = Salesman */
	char saleman[10];
/* Element = DATE
   Description = Date (YYYYMMDD) */
	char orderdate[8];
/* Element =
   Description = Ship Via */
	char shpvia[20];
/* Element = INVOICE
   Description = Invoice number */
	char invnum[8];
/* Element =
   Description = Terms */
	char terms[16];
/* Element = ACCOUNT
   Description = General Ledger Account Number */
	char account[18];
/* Element = REFNUM
   Description = Reference Number */
	char refnum[20];
};
#pragma member_alignment restore
