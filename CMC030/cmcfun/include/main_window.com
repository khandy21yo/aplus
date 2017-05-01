	!*******************************************************************
	! This include file is used to DECLARE information necessary for
	! maintainence programs using some varient of MAIN_WINDOW.
	!*******************************************************************

	%NOCROSS

	DECLARE LONG CONSTANT OPT_INIT		= 37%	! Init window
	DECLARE LONG CONSTANT OPT_OPTLIST	= 38%	! Modify options
	DECLARE LONG CONSTANT OPT_BACKGROUND	= 1%	! Paint background
	DECLARE LONG CONSTANT OPT_ENTRY		= 2%	! Enter/display/etc.
	DECLARE LONG CONSTANT OPT_TESTENTRY	= 3%	! Test input
	DECLARE LONG CONSTANT OPT_LOG		= 4%	! Log (obsolete)
	DECLARE LONG CONSTANT OPT_SETOLD	= 5%	! Set old value
	DECLARE LONG CONSTANT OPT_RESETOLD	= 6%	! Restore old value
	DECLARE LONG CONSTANT OPT_SETDEFAULT	= 7%	! Set default value
	DECLARE LONG CONSTANT OPT_RESETDEFAULT	= 8%	! Restore default value
	DECLARE LONG CONSTANT OPT_VIEW		= 9%	! View record
	DECLARE LONG CONSTANT OPT_FIND		= 11%	! Find record
	DECLARE LONG CONSTANT OPT_DISPLAY	= 12%	! Display additional info
	DECLARE LONG CONSTANT OPT_TESTOPT	= 15%	! Verify completing option
	DECLARE LONG CONSTANT OPT_AFTEROPT	= 16%	! Routine to do after option
	DECLARE LONG CONSTANT OPT_WINDOW	= 90%	! "Window" option
	DECLARE LONG CONSTANT OPT_MOREMENU	= 91%	! Additional menu options
	DECLARE LONG CONSTANT OPT_ARRAY		= 17%	! Journal array maint
	DECLARE LONG CONSTANT OPT_EXIT		= 18%	! Do before THE exit
	DECLARE LONG CONSTANT OPT_SUBWIND	= 19%	! Subwindow 

	!****************************************************************
	! Here are the options for OPT_ARRAY
	!****************************************************************

	DECLARE LONG CONSTANT OPT_ARRAY_LOAD	= 1%	! Load data into array
	DECLARE LONG CONSTANT OPT_ARRAY_REMOVE	= 2%	! Remove item from array
	DECLARE LONG CONSTANT OPT_ARRAY_SET	= 3%	! Set array item to current
	DECLARE LONG CONSTANT OPT_ARRAY_GETLOCK	= 4%	! Get/Lock element
	DECLARE LONG CONSTANT OPT_ARRAY_GET	= 5%	! Get element w/o lock
	DECLARE LONG CONSTANT OPT_ARRAY_SETKEY	= 6%	! Change key to match header
	DECLARE LONG CONSTANT OPT_ARRAY_PRINT	= 7%	! Display additional info

	%CROSS
