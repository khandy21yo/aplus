/*
 * File Layout for: TK.TK_FILE on 21-May-01
 *
 * File Structure Description
 */

#pragma member_alignment save
#pragma nomember_alignment

struct tk_file_cdd
{
/* Element = FILESTRNAME
   Description = Record structure name */
	char struct[39];
/* Element =
   Description = Sequence number */
	char sequence[3];
/* Element =
   Description = Field name */
	char fldname[39];
/* Element =
   Description = Description */
	char description[60];
/* Element =
   Description = Database */
	char database[2];
/* Element =
   Description = Field classifier */
	char classifier[20];
/* Element =
   Description = Data array (y/n) */
	char dataarray[1];
/* Element =
   Description = Date type */
	char datetype[20];
/* Element =
   Description = Size */
	long datasize;
/* Element = DATE
   Description = Creating date */
	char cdate[8];
/* Element = TIME
   Description = Creating time */
	char ctime[6];
};
#pragma member_alignment restore
