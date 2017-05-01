/*
 * File Layout for: RM.RM_JOURPROD on 21-May-01
 *
 * Restaurant Journal Product File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct rm_jourprod_cdd
{
/* Element = LOCATION
   Description = Location number */
	char location[4];
/* Element = DATE
   Description = Start date */
	char startdate[8];
/* Element = TRANSTYPE
   Description = Transaction type code */
	char transtype[2];
/* Element =
   Description = Sequential number */
	char seqnum[4];
/* Element = PRODUCT
   Description = Product Number */
	char product[14];
/* Element =
   Description = Product price */
	double price;
/* Element =
   Description = Daily quantity */
	double quantity[7];
};
#pragma member_alignment restore
