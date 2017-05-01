/*
 * File Layout for: UTL.UTL_MASTER_REPORT on 21-May-01
 *
 * CMC: Version of UTL_REPORT File
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_master_report_cdd
{
/* Element =
   Description = System name */
	char system[6];
/* Element =
   Description = Subsystem name */
	char subsys[6];
/* Element =
   Description = report number */
	char repnum[6];
/* Element =
   Description = Description */
	char repdes[30];
/* Element =
   Description = Program device */
	char prodev[32];
/* Element =
   Description = Program name */
	char pronam[40];
/* Element =
   Description = Can report be spooled */
	char canspool[1];
/* Element =
   Description = Can it be displayed */
	char candisp[1];
/* Element =
   Description = Can it go to a device */
	char candev[1];
/* Element =
   Description = Can it go to a file */
	char canfile[1];
/* Element =
   Description = Can it run detached */
	char candet[1];
/* Element =
   Description = Report Date used (Y/N) */
	char repyn[1];
/* Element =
   Description = Description of options */
	char descr[10][20];
/* Element =
   Description = Type of option */
	char opttype[10][1];
/* Element =
   Description = Length of option */
	int optlen[10];
/* Element =
   Description = Valid items in options */
	char valid[10][20];
/* Element =
   Description = Option data required? */
	char require[10][1];
/* Element =
   Description = Spooler name */
	char spool[32];
/* Element =
   Description = Option data */
	char optdef[10][20];
/* Element =
   Description = Default output file/device name */
	char defout[20];
/* Element =
   Description = Printer type groups */
	char itemgroup[10][2];
/* Element =
   Description = Defaults for printer groups */
	char item[10][6];
/* Element =
   Description = Program to chain to */
	char chainto[20];
/* Element =
   Description = Printer type */
	char printtype[8];
/* Element =
   Description = Last run date */
	char lastrundate[8];
/* Element =
   Description = Last run time */
	char lastruntime[6];
/* Element =
   Description = Base run date */
	char baserundate[8];
/* Element =
   Description = Run frequency */
	char runfreq[2];
/* Element =
   Description = Report Width in Characters */
	int repwid;
/* Element =
   Description = Spooler From Name */
	char spoolform[20];
};
#pragma member_alignment restore
