/*
 * File Layout for: SS.SS_LICENSE on 21-May-01
 *
 * Support System Customer Licensing File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ss_license_cdd
{
/* Element = CUSTOMER
   Description = Customer Number */
	char cusnum[10];
/* Element =
   Description = System License Number */
	char license_num[10];
/* Element = DATE
   Description = License Expiration Date (YYYYMMDD) */
	char expir_date[8];
/* Element =
   Description = Product (System) Make Number */
	char make[10];
/* Element =
   Description = Product (System) Model Number */
	char model[10];
/* Element = SERIAL
   Description = Product (System) Serial number */
	char serial_num[20];
/* Element =
   Description = Operating System */
	char oper_sys[6];
/* Element =
   Description = Version Number */
	char version[6];
/* Element = PHONE
   Description = Phone number */
	char phone_num[10];
/* Element = PHONE_EXTENSION
   Description = Phone Extension */
	char phone_ext[4];
};
#pragma member_alignment restore
