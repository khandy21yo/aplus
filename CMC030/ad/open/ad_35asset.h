/*
 * File Layout for: AD.AD_35ASSET on 21-May-01
 *
 * Asset Description File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ad_35asset_cdd
{
/* Element = ASSET_NUM
   Description = Asset number */
	char asset_num[10];
/* Element =
   Description = Asset description */
	char description[40];
/* Element =
   Description = Asset type */
	char asset_type[2];
/* Element = LOCATION
   Description = Location */
	char location[4];
/* Element = DEPT_NUM
   Description = Department number */
	char dept_num[6];
/* Element =
   Description = Serial number */
	char serial_num[20];
/* Element = DATE
   Description = Service Date */
	char servdate[8];
/* Element =
   Description = Initial cost */
	double cost;
/* Element =
   Description = Salvage */
	double salvage;
/* Element =
   Description = Bonus, Section 179 */
	double bonus;
/* Element =
   Description = Investment tax credit */
	double itc;
/* Element =
   Description = ITC Basis Reduction */
	double itcreduce;
/* Element =
   Description = Life in Units */
	double units;
/* Element = DATE
   Description = Date when asset has been retired */
	char ret_date[8];
/* Element =
   Description = Amount of disposition */
	double proceeds;
/* Element =
   Description = Notes */
	char notes[40];
};
#pragma member_alignment restore
