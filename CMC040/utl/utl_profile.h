/*
 * File Layout for: UTL.UTL_PROFILE on 21-May-01
 *
 * Company Profile
 */
#ifndef _utl_profile_h_
#define _utl_profile_h_

//!
//! \brief Company profile information
//!
class utl_profile_cdd : public db_rmsrelative_cdd
{
public:

public:
/* Company name for menu */
	std::string menu_name;
/* Company name for report */
	std::string rep_name;
/* Main Office Location number */
	std::string mainlocation;
/*  Default Location number */
	std::string deflocation;
};

#endif
