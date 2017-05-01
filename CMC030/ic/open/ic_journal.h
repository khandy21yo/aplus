/*
 * File Layout for: IC.IC_JOURNAL on 21-May-01
 *
 * Inventory Journal
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ic_journal_cdd
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
/* Element = REF
   Description = Primary Reference */
	char primary_ref[8];
/* Element = REF
   Description = Secondary Reference. */
	char secondary_ref[8];
/* Element = XREF
   Description = Cross reference */
	char cross_ref[10];
/* Element =
   Description = Subaccount */
	char subaccount[10];
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
   Description = Unit Cost */
	double cost;
/* Element = ACCOUNT
   Description = Trans General ledger account number */
	char expacct[18];
/* Element = LOCATION
   Description = Location number */
	char tolocation[4];
};
#pragma member_alignment restore
