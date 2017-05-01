/*
 * File Layout for: TV.TV_COPY_INSTR on 21-May-01
 *
 * TV Copy Instructions
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tv_copy_instr_cdd
{
/* Element =
   Description = Form number */
	char frmnum[8];
/* Element =
   Description = Sequence number */
	char seqnum[2];
/* Element =
   Description = From date */
	char from_date[8];
/* Element =
   Description = To date */
	char to_date[8];
/* Element =
   Description = Length */
	char length[6];
/* Element =
   Description = From time */
	char from_time[6];
/* Element =
   Description = To time */
	char to_time[6];
/* Element =
   Description = Spot rotations */
	char spot_rotation[30];
/* Element =
   Description = Current Rotation */
	int current_rotation;
};
#pragma member_alignment restore
