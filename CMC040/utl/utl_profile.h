/*
 * File Layout for: UTL.UTL_PROFILE on 21-May-01
 *
 * Company Profile
 */
#ifndef _utl_profile_h_
#define _utl_profile_h_

class utl_profile_cdd : public db_rmsrelative_cdd
{
public:
	virtual void copy_tomap(db_map_cdd &dbmap)
	{
		dbmap["menu_name"] = menu_name;
		dbmap["rep_name"] = rep_name;
		dbmap["mainlocation"] = mainlocation;
		dbmap["deflocation"] = deflocation;
	}
	virtual void copy_frommap(db_map_cdd &dbmap)
	{ 
		menu_name = dbmap["menu_name"];
		rep_name = dbmap["rep_name"];
		mainlocation = dbmap["mainlocation"];
		deflocation = dbmap["deflocation"];
	}

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
