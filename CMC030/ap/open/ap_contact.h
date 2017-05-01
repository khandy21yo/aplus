/*
 * File Layout for: AP.AP_CONTACT on 21-May-01
 *
 * Contact File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct ap_contact_cdd
{
/* Element =
   Description = */
	char cusnum[10];
/* Element =
   Description = */
	char contact_name[30];
/* Element =
   Description = */
	char title[20];
/* Element =
   Description = */
	char phone[10];
/* Element =
   Description = */
	char extension[4];
};
#pragma member_alignment restore
