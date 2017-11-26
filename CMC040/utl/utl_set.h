/*
 * File Layout for: UTL.UTL_SET on 21-May-01
 *
 * CMC Utility Set File
 */
#ifndef _utl_set_h_
#define _utl_set_h_

class utl_set_cdd
{
public:
/* Program name */
	std::string  programname; //[39];
/* Item number */
	std::string  item; //[6];
/* System name */
	std::string  system; //[2];
/* Yes or No Flag for Undefined Input */
	std::string allowund; //[1];
/* Unused */
	std::string  unused; //[3];
/* Hard/Soft/Field default */
	std::string  hard; //[1];
/* Data */
	std::string sdata; //[30];
/* Data Format */
	std::string fdata; //[30];
};

#endif
