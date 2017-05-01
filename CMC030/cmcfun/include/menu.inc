	!*******************************************************************
	! MENU.INC - Include file to DECLARE record structures and constants
	! for the menu program.
	!*******************************************************************


	!
	! Record structure to hold working menu
	!
	RECORD MENU_CDD
		STRING	OPT	= 6%		! Option Name
		WORD	OPTLEN			! Length to highlight
		STRING	DESCR	= 40%		! Description of option
		WORD	OPTV			! Function to perform
		STRING	SVALUE	= 40%		! String Value
		LONG	OVALUE			! Numeric Value
		WORD	EFLAG			! End of list flag
	END RECORD

	!
	! OPTV values
	!
	DECLARE WORD CONSTANT OPTV_MENU	= 1%	! Go to another menu level
	DECLARE WORD CONSTANT OPTV_RUN	= 2%	! Run a program
	DECLARE WORD CONSTANT OPTV_REPORT= 3%	! Run a report
	DECLARE WORD CONSTANT OPTV_SPAWN	= 4%	! Spawn (run) a program
	DECLARE WORD CONSTANT OPTV_INCLUDE= 5%	! Include another menu

