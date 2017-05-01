/*
 * File Layout for: OS.OS_JOURNAL on 13-Apr-99
 *
 * Main Journal Header
 */

struct os_journal_cdd
{
/* Element = TRANKEY
   Description = Transaction key */
	char trans[6];
/* Element = CUSTOMER
   Description = Customer Number */
	char customer[10];
/* Element = INVOICE
   Description = Invoice number */
	char invoice[8];
/* Element = DATE
   Description = Transaction Date (YYYYMMDD) */
	char trandate[8];
/* Element =
   Description = Sales Tax */
	double salestax;
/* Element =
   Description = Prepayment */
	double payment;
/* Element = DEPOSIT
   Description = Deposit number */
	char deposit[6];
};
