// utl_reportx_cdd
//
// Report handling structure.

#ifndef _utl_reportx_h_
#define _utl_reportx_h_

class utl_reportx_cdd
{
public:
	// report number
	std::string repnum;
	// Program device
	std::string prodev;
	// Program name
	std::string pronam;
	// Spooler name
	std::string spoo;
	// Option data
	std::string optdef[10];
	// Default output file/device name
	std::string defout;
	// Printer type groups
	std::string itemgroup[10];
	// Defaults for printer groups
	std::string item[10];
	// Starting page
	long startp;
	// Ending page
	long endp;
	// Copies
	long copies;
	// Page length
	long pagelen;
	// Report width
	long repwidth;
	// Report date
	std::string repdate;
	// Print report date on report (yn)
	std::string repyn;
	// Output flag 1 - Display 2 - Spool 3 - Ou
	long printto;
	// Autoscroll flag
	long autoscroll;
	// Printer type
	std::string printtype;
	// Printer initilization string
	std::string printinit;
	// Printer finilization string
	std::string printfinish;
	// Control string to go to next page
	std::string nextpage;
	// Output to local printer
	std::string tolocal;
	// Output to screen again
	std::string toscreen;
	// Next program to run
	std::string nextrun;
	// Channel for report output
	long chan;
	// Exit status
	long stat;
	// Page number
	long pageno;
	// Line number
	long linenO;
	// Starting date
	std::string sdate;
	// Starting time
	std::string stime;
	// Window for selection menu
	long window;
	// Cannot detach flag
	long detach;
	// Spooler From Name
	std::string spoolform;
};

#endif
