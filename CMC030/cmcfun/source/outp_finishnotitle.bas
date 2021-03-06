1	%TITLE "Close Out Any Open Report (No Title)"
	%SBTTL "OUTP_FINISHNOTITLE"
	%IDENT "V3.6a Calico"

	SUB OUTP_FINISHNOTITLE(UTL_REPORTX_CDD UTL_REPORTX)

	!
	!	COPYRIGHT (C) 1988 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
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
	!	.p
	!	This subroutine finishes up any REPORT currently open
	!
	!	This is the same as OUTP_FINISH, but calls OUTP_LINENOTITLE
	!	instead of OUPT_LINE, which saves us 10 blocks in the
	!	executable.
	!
	! Parameters:
	!
	!	UTL_REPORTX
	!		The created file that closes any other reports.
	!
	!	Returned value
	!		It finishes up any report that may still
	!		be currently open.
	!
	! Example:
	!
	!	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_FINISHNOTITLE
	!	$ LIB FUNC_LIB:CMCFUN/REP OUTP_FINISHNOTITLE
	!	$ DELETE OUTP_FINISHNOTITLE.OBJ;*
	!
	! Author:
	!
	!	08/29/88 - Kevin Handy
	!
	! Modification history:
	!
	!	08/29/88 - Kevin Handy
	!		Taken from OUTP_FINISH
	!
	!	09/06/88 - Kevin Handy
	!		Added code for ZZ print control string.
	!
	!	10/14/88 - Frank Starman
	!		Change REF: to SIC:.
	!
	!	04/06/89 - Kevin Handy
	!		Modified handling of escape sequences
	!
	!	03/29/90 - Frank F. Starman
	!		Assign a different key for windows.
	!
	!	02/25/92 - Kevin Handy
	!		Modified to try to lose excess form feed at
	!		end of document.
	!
	!	03/25/93 - Kevin Handy
	!		Modifications to try to mke forms not lock up
	!		when user presses the exit key while painting.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/31/95 - Kevin Handy
	!		Modified to call ASSG_FREECHANNEL after closing
	!		the channels. This should reduce the occurance
	!		of the "No available channels" error.
	!
	!	06/19/95 - Kevin Handy
	!		Moved ASSG_FREECHANNEL out of the select statement
	!		so that it always gets called.
	!
	!	03/25/96 - Kevin Handy
	!		Reformat source code.
	!
	!	09/16/97 - Kevin Handy
	!		Lose code for clipboard, which isn't usable.
	!		Lose code for File Cabinet, which didn't work.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/22/99 - Kevin Handy
	!		Zero UTL_REPORTX::CHAN after we release the channel
	!
	!	09/14/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!
	!	09/23/2005 - Kevin Handy
	!		Force autoscroll off after every call to OUTP_LINE.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION LIBR_3INSERT

	%PAGE

	!
	! Set up variables we will probibly need
	!
	CALL WRIT_STRING(UTL_REPORTX::PRINTFINISH, PRINTFINISH$)
	CALL WRIT_STRING(UTL_REPORTX::TOLOCAL, TOLOCAL$)
	CALL WRIT_STRING(UTL_REPORTX::TOSCREEN, TOSCREEN$)

	!
	! Finish off report
	!
	SELECT UTL_REPORTX::PRINTTO

	!
	! Display
	!
	CASE OUTP_TODISPLAY
		UTL_REPORTX::AUTOSCROLL = 0%

		IF UTL_REPORTX::STAT = 0%
		THEN
 ReLoop:
			CALL OUTP_LINENOTITLE(LYT_LINE$, &
				UTL_REPORTX, "", -12345%)
			UTL_REPORTX::AUTOSCROLL = 0%

			IF UTL_REPORTX::STAT = 0%
			THEN
				SELECT SCOPE::SCOPE_EXIT
				!
				! An exit type of key
				!
				CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ, &
					3%, SMG$K_TRM_F8, &
					SMG$K_TRM_DO

				!
				! All other keys cause a reloop
				!
				CASE ELSE
					GOTO Reloop

				END SELECT
			END IF
		END IF

		!
		! Erase display screen
		!
		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(UTL_REPORTX::WINDOW)

	!
	! Spool
	!
1000	CASE OUTP_TOSPOOL
		PRINT #UTL_REPORTX::CHAN, PRINTFINISH$;
		CLOSE UTL_REPORTX::CHAN

		CALL OUTP_SPOOL(UTL_REPORTX)

	!
	! File
	!
	CASE OUTP_TOFILE
		PRINT #UTL_REPORTX::CHAN, PRINTFINISH$;
		CLOSE UTL_REPORTX::CHAN

	!
	! Word Processing, S2020, DIF
	!
	CASE OUTP_TOWP, OUTP_TO2020, OUTP_TOPL
		CLOSE UTL_REPORTX::CHAN

	!
	! Device
	!
	CASE OUTP_TODEVICE
		PRINT #UTL_REPORTX::CHAN, PRINTFINISH$;
		CALL OUTP_FORMFF(UTL_REPORTX) &
			IF (UTL_REPORTX::LINENO <> 0%)
		CLOSE UTL_REPORTX::CHAN

	!
	! Local printer
	!
	CASE OUTP_TOLOCAL
		PRINT #UTL_REPORTX::CHAN, TOLOCAL$; PRINTFINISH$;
		CALL OUTP_FORMFF(UTL_REPORTX) &
			IF (UTL_REPORTX::LINENO <> 0%)
		PRINT #UTL_REPORTX::CHAN, TOSCREEN$;
		CLOSE UTL_REPORTX::CHAN

	!
	! Documentation
	!
	CASE OUTP_TODOCUMENT
		CLOSE UTL_REPORTX::CHAN

		PROG_NAME$ = TRM$(UTL_REPORTX::PRONAM)

		!
		! Create key name
		!
		KEY_NAME$ = TRM$(PROG_NAME$) + "$" + "REPORT"

		!
		! Create library name
		!
		TEMP% = INSTR(1%, PROG_NAME$ + "_", "_")
		LIB_NAME$ = "SIC:WINDOWS_" + &
			LEFT(PROG_NAME$, TEMP% - 1%)

		!
		! Plop it into library
		!
		ST% = LIBR_3INSERT(LIB_NAME$, UTL_REPORTX::DEFOUT, KEY_NAME$)

		!
		! Delete the file (hope for only one)
		!
 !		KILL UTL_REPORTX::DEFOUT
		SMG_STATUS% = LIB$DELETE_FILE(TRM$(UTL_REPORTX::DEFOUT) + ";*")
	END SELECT

	CALL ASSG_FREECHANNEL(UTL_REPORTX::CHAN)
	UTL_REPORTX::CHAN = 0%

2000	!
	! Turn off unsolicited input
	!
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

2010	!
	! Erase display (option and message)
	!
	IF SCOPE::SMG_OPTION
	THEN
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	END IF

2020	IF SCOPE::SMG_MESSAGE
	THEN
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)
	END IF

2030	!
	! Change the width
	!
	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, 80%)

	END SUB
