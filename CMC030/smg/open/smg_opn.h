/*
 * File Layout for: SMG.SMG_OPN on 11-Mar-99
 *
 * PASS OPEN FILE DATA BETWEEN PROGRAMS
 */

struct smg_opn_cdd
{
/* Element =
   Description = File organization */
	char orgnization[30];
/* Element =
   Description = File structure */
	char strcture[30];
/* Element =
   Description = File open name */
	char file_name[60];
/* Element =
   Description = File extension */
	char extension[10];
/* Element =
   Description = Keys */
	char keys[32][255];
/* Element =
   Description = Number of keys */
	int keys_num;
};
