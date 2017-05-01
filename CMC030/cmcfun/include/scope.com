	!*******************************************************************
	! SCOPE.COM - Defines scope type terminal functions so that
	! they are easily changed.
	!
	! This file is closely related to SCOPEDEF.COM and must be
	! changed at the same time.
	!*******************************************************************

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! External SMG$ functions declarations
	!
 !	%INCLUDE "FUNC_INCLUDE:SMG_EXTERNALS.COM"

	!
	! CMC$ Codes
	!
 !	%INCLUDE"FUNC_INCLUDE:CONSTANTS.INC"

	!
	! Common area
	!
	%NOCROSS

 !	RECORD SCOPE_STRUCT
 !		LONG SCOPE_EXIT		* Exit status of entry routines
 !		LONG SCOPE_TIMEOUT	* Time for input timeout in seconds
 !		LONG SMG_KBID		* Keyboard buffer name
 !		LONG SMG_PBID		* Pastboard buffer name
 !		LONG SMG_OPTION		* Option display
 !		LONG SMG_MESSAGE	* Message display
 !		LONG MACROFLAG		* Macro command flag
 !		STRING PRG_COMPANY=64	* Company name
 !		STRING PRG_IDENT=4	* Message type 
 !		STRING PRG_PROGRAM=40	* Program Name
 !		STRING PRG_ITEM=16	* Item for help
 !		LONG SCREEN_WIDTH	* Width of created screen
 !		LONG IMENU_LEVELS	* Interupt Menu Levels
 !	END RECORD

	MAP (SCOPE) &
		SCOPE_STRUCT SCOPE

	MAP (SCOPE) &
		SCOPE.EXIT%,		! Exit status of entry routines &
		SCOPE.TIMEOUT%,		! Time for input timeout in seconds &
		LONG SMG_KBID,		! Keyboard buffer name &
		LONG SMG_PBID,		! Pastboard buffer name &
		LONG SMG_OPTION,	! Option display &
		LONG SMG_MESSAGE,	! Message display &
		LONG MACROFLAG,		! Macro command flag &
		STRING PRG_COMPANY = 64,! Company name &
		STRING PRG_IDENT = 4,	! Message type &
		STRING PRG_PROGRAM = 40,! Program Name &
		STRING PRG_ITEM = 16,	! Item for help &
		LONG SCREEN.WIDTH,	! Width of created screen &
		LONG IMENU_LEVELS	! Interupt Menu Levels


	%CROSS
