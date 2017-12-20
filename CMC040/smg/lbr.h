//! \file
//! \brief Header file for LBR$ emulation routines for Aplus
//!
//! \author Kevin Handy, Dec 017

#ifndef _lbr_h_
#define _lbr_h_

//
// Constants
//
static long LBR$C_READ = 1;	//!< Reading


//!
//! \Brief lib$ structure
//!
class lbr_index_cdd
{
public:
	long mode;		//!!< Operation mode (LBRC_READ)
	std::string name;	//!< Library name being referenced

};

//!
//! \brief close library
//!
static inline long lbr$close(
	lbr_index_cdd &lr_index)	//!< Control structure
{
	// Don't really need to do anything
}

//!
//! \brief initialize control structure
//!
static inline long lbr$ini_control(
	lbr_index_cdd &lr_index,	//!< Control structure
	long omode)			//!< Mode
{
	lr_index.mode = omode;
	return 1;
}
long lbr$open(
	lbr_index_cdd &lr_index,
	const std::string &lib1_name,
	long a,
	const std::string &extension);

#endif



