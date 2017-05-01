/*
 * File Layout for: AR.AR_SALTAXLED on 21-May-01
 *
 * Sales Tax Monthly Report File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ar_saltaxled_cdd
{
/* Element =
   Description = Tax Type */
	char taxtyp[1];
/* Element = CUSTOMER
   Description = Customer Number */
	char cusnum[10];
/* Element = INVOICE
   Description = Invoice number */
	char invnum[8];
/* Element =
   Description = Sales Tax Amount */
	double amount;
/* Element = BATCH
   Description = Batch number used for posting */
	char batch[6];
/* Element =
   Description = Transaction date */
	char tradat[8];
};
#pragma member_alignment restore
