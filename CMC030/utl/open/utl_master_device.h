/*
 * File Layout for: UTL.UTL_MASTER_DEVICE on 21-May-01
 *
 * Utility Device File from CMC:
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_master_device_cdd
{
/* Element =
   Description = */
	char filenam[39];
/* Element =
   Description = */
	char device[127];
/* Element =
   Description = */
	char procod[32];
/* Element =
   Description = */
	char catag[8];
};
#pragma member_alignment restore
