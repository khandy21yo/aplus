/*
 * File Layout for: AP.AP_1099_YYYY on 21-May-01
 *
 * 1099 Annual Work File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ap_1099_yyyy_cdd
{
/* Element =
   Description = Vendor Number */
	char vennum[10];
/* Element =
   Description = Code */
	char code[2];
/* Element =
   Description = Transaction key */
	char trankey[6];
/* Element =
   Description = Invoice number */
	char invnum[15];
/* Element =
   Description = Invoice Date */
	char invdat[8];
/* Element =
   Description = Check Number */
	char cknum[6];
/* Element =
   Description = Check date */
	char ckdat[8];
/* Element =
   Description = 1099 Amount */
	double amt1099;
/* Element =
   Description = Check Amount */
	double ckamt;
};
#pragma member_alignment restore
