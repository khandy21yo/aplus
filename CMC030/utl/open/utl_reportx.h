/*
 * File Layout for: UTL.UTL_REPORTX on 21-May-01
 *
 * Report Settings Structure
 */

#pragma member_alignment save
#pragma nomember_alignment

struct utl_reportx_cdd
{
/* Element =
   Description = report number */
	char repnum[6];
/* Element =
   Description = Program device */
	char prodev[32];
/* Element =
   Description = Program name */
	char pronam[40];
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
   Description = Starting page */
	long startp;
/* Element =
   Description = Ending page */
	long endp;
/* Element =
   Description = Copies */
	long copies;
/* Element =
   Description = Page length */
	long pagelen;
/* Element =
   Description = Report width */
	long repwidth;
/* Element =
   Description = Report date */
	char repdate[32];
/* Element =
   Description = Print report date on report (yn) */
	char repyn[1];
/* Element =
   Description = Output flag 1 - Display 2 - Spool 3 - Ou */
	long printto;
/* Element =
   Description = Autoscroll flag */
	long autoscroll;
/* Element =
   Description = Printer type */
	char printtype[8];
/* Element =
   Description = Printer initilization string */
	char printinit[64];
/* Element =
   Description = Printer finilization string */
	char printfinish[64];
/* Element =
   Description = Control string to go to next page */
	char nextpage[64];
/* Element =
   Description = Output to local printer */
	char tolocal[32];
/* Element =
   Description = Output to screen again */
	char toscreen[32];
/* Element =
   Description = Next program to run */
	char nextrun[64];
/* Element =
   Description = Channel for report output */
	long chan;
/* Element =
   Description = Exit status */
	long stat;
/* Element =
   Description = Page number */
	long pageno;
/* Element =
   Description = Line number */
	long lineno;
/* Element =
   Description = Starting date */
	char sdate[10];
/* Element =
   Description = Starting time */
	char stime[10];
/* Element =
   Description = Window for selection menu */
	long window;
/* Element =
   Description = Cannot detach flag */
	long detach;
/* Element =
   Description = Spooler From Name */
	char spoolform[20];
/* Element =
   Description = Spaces to add to left margin */
	long offset;
/* Element = TIME
   Description = Run After Time (HHMMSS) */
	char aftertime[6];
/* Element = YESNO
   Description = Run Background? */
	char background[1];
};
#pragma member_alignment restore
