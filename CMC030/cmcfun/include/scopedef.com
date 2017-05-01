	!*******************************************************************
	! SCOPEDEF.COM - Defines scope type terminal functions so that
	! they are easily changed.
	!
	! This file is closely related to SCOPE.COM and must be changed
	! at the same time.
	!*******************************************************************

	!
	! External SMG$ functions declarations
	!
	%INCLUDE "FUNC_INCLUDE:SMG_EXTERNALS.COM"

	!
	! Common area
	!
	%NOCROSS

	RECORD SCOPE_STRUCT
		LONG SCOPE_EXIT		! Exit status of entry routines
		LONG SCOPE_TIMEOUT	! Time for input timeout in seconds
		LONG SMG_KBID		! Keyboard buffer name
		LONG SMG_PBID		! Pastboard buffer name
		LONG SMG_OPTION		! Option display
		LONG SMG_MESSAGE	! Message display
		LONG MACROFLAG		! Macro command flag
		STRING PRG_COMPANY=64	! Company name
		STRING PRG_IDENT=4	! Message type 
		STRING PRG_PROGRAM=40	! Program Name
		STRING PRG_ITEM=16	! Item for help
		LONG SCREEN_WIDTH	! Width of created screen
		LONG IMENU_LEVELS	! Interupt Menu Levels
	END RECORD

	%CROSS
