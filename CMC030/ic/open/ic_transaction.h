/*
 * File Layout for: IC.IC_TRANSACTION on 21-May-01
 *
 * Inventory Transaction
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ic_transaction_cdd
{
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = DATE
   Description = Date (MMDDYYYY) */
	char trans_date[8];
/* Element = REFNO
   Description = Reference number */
	char primary_ref[16];
/* Element = XREF
   Description = Cross reference */
	char cross_ref[10];
/* Element =
   Description = Subaccount */
	char subaccount[10];
/* Element =
   Description = Lot number */
	char lot[10];
/* Element =
   Description = Salesman,Operator etc */
	char stationman[10];
/* Element =
   Description = Transaction type */
	char type_a[2];
/* Element = UNIT
   Description = Unit amount */
	double quantity_a;
/* Element =
   Description = Transaction type */
	char type_b[2];
/* Element = UNIT
   Description = Unit amount */
	double quantity_b;
/* Element =
   Description = Inventory cost */
	double cost;
/* Element =
   Description = Price Amount */
	double price;
/* Element = ACCOUNT
   Description = Transaction General Ledger Account Numbe */
	char transacct[18];
/* Element = DATE
   Description = Date (MMDDYYYY) */
	char postdate[8];
/* Element = TIME
   Description = Time (HHMMSS) */
	char posttime[6];
/* Element = BATCH
   Description = Batch number used for posting */
	char batch[6];
};
#pragma member_alignment restore
