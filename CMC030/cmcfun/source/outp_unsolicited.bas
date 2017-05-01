1	%TITLE "Handle Unsolicited Input"
	%SBTTL "OUTP_UNSOLICITED"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OUTP_UNSOLICITED(LONG OPT)

	!
	! COPYRIGHT (C) 1986 BY
	! Computer Management Center
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies thereof may not be provided or otherwise made
	! available to any other person.  No title to and ownership of
	! the software is hereby transferred.
	!
	! The information in this software is subject to change without
	! notice and should not be construed as a commitment by
	! Computer Management Center, Inc.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	!
	! Abstract:HELP
	!	.b
	!	REPORT
	!	.b
	!	.lm +5
	!	This function handles unsolicited inputs.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_UNSOLICITED/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OUTP_UNSOLICITED
	!	$ DELETE OUTP_UNSOLICITED.OBJ;*
	!
	! Author:
	!
	!	01/31/89 - Frank Starman
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/08/99 - Kevin Handy
	!		Lose extra parameter to SMG$REPAINT_SCREEN
	!
	!	04/21/99 - Kevin Handy
	!		Fix unsolicited input
	!--
	!

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	DECLARE LONG SMG_STATUS
	DECLARE LONG EXIT_STATUS

	!
	! External functions
	!
	EXTERNAL LONG    OUTP_XUNSOL

	%PAGE

	EXIT_STATUS = CMC$_NORMAL

	!
	! Handle any special junk in RRR_FLAG%
	!
	SELECT RRR_FLAG%

	!
	! Repaint screen
	!
	CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
		SMG_STATUS = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
		SMG_STATUS = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Help
	!
	CASE SMG$K_TRM_HELP
		SMG_STATUS = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
		CALL HELP_3MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, SCOPE::PRG_PROGRAM, SCOPE::PRG_ITEM)
		SMG_STATUS = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Interrupt
	!
	CASE SMG$K_TRM_F6, SMG$K_TRM_F20, SMG$K_TRM_CTRLY
		SMG_STATUS  = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)
		CALL MENU_3INTERRUPT(SCOPE)
		SMG_STATUS  = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

	!
	! Exit keys
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, SMG$K_TRM_F8
		EXIT_STATUS = CMC$_UNTERROR

	END SELECT

	RRR_FLAG% = 0%
	OUTP_UNSOLICITED = EXIT_STATUS

32767	END FUNCTION
