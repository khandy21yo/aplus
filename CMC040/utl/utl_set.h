//! \file
//
// File Layout for: UTL.UTL_SET on 21-May-01
//
// CMC Utility Set File
///


//!
//! \brief utl_set class
//!
class utl_set_cdd
{
public:
//! Program name
	std::string programname; //[39];
//! Item number
	std::string item; //[6];
//! System name
	std::string system; //[2];
//! Yes or No Flag for Undefined Input
	std::string allowund; //[1];
//! Hard/Soft/Field default
	std::string hard; //[1];
//! Data
	std::string sdata; //[30];
//! Data Format
	std::string fdata; //[30];
//! Unused Fiels
	std::string unused2; //[4];
};
