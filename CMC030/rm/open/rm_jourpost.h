/*
 * File Layout for: RM.RM_JOURPOST on 21-May-01
 *
 * Restaurant Journal Posting File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct rm_jourpost_cdd
{
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = DATE
   Description = Date */
	char startdate[8];
/* Element = TRANSTYPE
   Description = Transaction type code from entry */
	char transtype[2];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element = DATE
   Description = Actual date (YYYYMMDD) */
	char actdate[8];
/* Element = TRANSTYPE
   Description = Transaction type code */
	char ttype[2];
/* Element =
   Description = Quantity */
	double quantity;
/* Element =
   Description = Price */
	double price;
/* Element =
   Description = Sequential number */
	char seqnum[4];
};
#pragma member_alignment restore
