/*
 * File Layout for: UTL.UTL_REPORT on 21-May-01
 *
 * Utility Report File
 */
#ifndef _utl_report_h_
#define _utl_report_h_

//!
//! \brief Utility Report File
//!
class utl_report_cdd : public db_rmsindexed_cdd
{
public:
	std::string system; //[6];
		//!< System name
	std::string subsys; //[6];
		//!< Subsystem name
	std::string repnum; //[6];
		//!< report number
	std::string repdes; //[30];
		//!< Description
	std::string prodev; //[32];
		//!< Program device
	std::string pronam; //[40];
		//!< Program name
	std::string canspool; //[1];
		//!< Can report be spooled
	std::string candisp; //[1];
		//!< Can it be displayed
	std::string candev; //[1];
		//!< Can it go to a device
	std::string canfile; //[1];
		//!< Can it go to a file
	std::string candet; //[1];
		//!< Can it run detached
	std::string repyn; //[1];
		//!< Report Date used (Y/N)
	std::string descr[10]; //[20];
		//!< Description of options
	std::string opttype[10]; //[1];
		//!< Type of option
	int optlen[10];
		//!< Length of option
	std::string valid[10]; //[20];
		//!< Valid items in options
	std::string require[10]; //[1];
		//!< Option data required?
	std::string spool; //[32];
		//!< Spooler name
	std::string optdef[10]; //[20];
		//!< Option data
	std::string defout; //[20];
		//!< Default output file/device name
	std::string itemgroup[10]; //[2];
		//!< Printer type groups
	std::string item[10]; //[6];
		//< Defaults for printer groups
	std::string chainto; //[20];
		//< Program to chain to
	std::string printtype; //[8];
		//< Printer type
	std::string lastrundate; //[8];
		//!< Last run date
	std::string lastruntime; //[6];
		//!< Last run time
	std::string baserundate; //[8];
		//!< Base run date
	std::string runfreq; //[2];
		//< Run frequency
	int repwid;
		//!< Report Width in Characters
	std::string spoolform; //[20];
		//!< Description = Spooler From Name

public:
	utl_report_cdd()
	{
		table_name ="utl_report";
	}
	void Get(const std::string &repnum);
};

void outp_initstructure(
	const utl_report_cdd &utl_report,
	utl_reportx_cdd &utl_reportx);

#endif
