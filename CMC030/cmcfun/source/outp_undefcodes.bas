1	%TITLE "Store and Print Undefined Codes"
	%SBTTL "OUTP_UNDEFCODES"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OUTP_UNDEFCODES( LONG OPT, &
		STRING TITLE(), &
		UTL_REPORTX_CDD UTL_REPORTX, &
		STRING TEXT)
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
	!	.lm +5
	!	Store and print undefined codes, if any.
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
	!	$ BAS FUNC_SOURCE:OUTP_UNDEFCODES
	!	$ LIB FUNC_LIB:CMCFUN/REP OUTP_UNDEFCODES
	!	$ DELETE OUTP_UNDEFCODES.OBJ;*
	!
	! Author:
	!
	!	01/27/89 - Frank F. Starman
	!
	! Modification history:
	!
	!	08/15/91 - Frank F. Starman
	!		Do not display twice message about undefined codes.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Convert to V3.6 coding standards.
	!		Changes LINES.CH from WORD to LONG.
	!
	!	03/25/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	COM (OUTP_UNDEFCODE.COM) LONG LINES.CH, LONG CONF_STATUS

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	DECLARE LONG EXIT_STATUS

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	OUTP_UNSOLICITED

	!
	! Assume success
	!
	EXIT_STATUS = CMC$_NORMAL

100	!
	! Open file
	!
	IF LINES.CH = 0%
	THEN
		CALL ASSG_CHANNEL(LINES.CH, STAT%)
		WHEN ERROR IN
			OPEN "LINES.SEQ" FOR OUTPUT AS FILE LINES.CH, &
				ORGANIZATION SEQUENTIAL, &
				RECORDSIZE 132%, &
				TEMPORARY, &
				ALLOW NONE, &
				ACCESS MODIFY
		USE
			FILENAME$ = "LINES"
			CONTINUE HelpError
		END WHEN

		CONF_STATUS = CMC$_NORMAL

	END IF

	SELECT OPT

	!
	! Create file
	!
	CASE OPT_ADDREC

		!
		! Check unsolicited input
		!
		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitFunction
			END IF
		END IF

		PRINT #LINES.CH, TEXT
		CONF_STATUS = CMC$_UNDEFINED
	!
	! Confirm
	!
	CASE OPT_CONFIRM

		IF CONF_STATUS <> CMC$_NORMAL
		THEN

			CALL HELP_PRINTMESS(SCOPE, "undefined codes", &
				"E", "OUTP_UNDEFCODES", "", "UNDCODES", &
				UTL_REPORTX, TITLE(), 0%)

	!++
	! Error:UNDCODES
	!	^*Undefined Codes\*
	!	.p
	!	^*Explanation\*
	!	.p
	!	This process has been aborted or interrupted because
	!	of undefined codes (such as account number, customer number,
	!	vendor number, product number, location etc.)
	!	.p
	!	^*User Action\*
	!	.p
	!	All undefined codes are assigned by star (_*). Correct them
	!	in the journal maintenance and restart posting process.
	! Index:
	!	.x Undefined Codes
	!--

			EXIT_STATUS = CMC$_WARNING

		END IF
	!
	! Print undefined codes
	!
	CASE OPT_SUMMARY

		I% = 1%
		WHILE TITLE(I%) <> ""
			I% = I% + 1%
		NEXT

		J% = I% + 1%
		WHILE TITLE(J%) <> ""
			TITLE(J%) = ""
			J% = J% + 1%
		NEXT

		TITLE(I% + 1%) = "Check for undefined or incorrect fields " + &
			"assigned by asterisk (*)"
		TITLE(I% + 1%) = STRING$(132% - LEN(TITLE(I% + 1%)), A"="B) + &
			TITLE(I% + 1%)
		TITLE(I% + 2%) = TEXT
		TITLE(I% + 3%) = "."

		LIN% = 999%

400		WHEN ERROR IN
			RESET #LINES.CH
		USE
			CONTINUE ExitFunction IF ERR = 11%
			FILENAME$ = "LINES"
			CONTINUE HelpError
		END WHEN

 NextLine:
		!
		! Check unsolicited input
		!
		IF RRR_FLAG%
		THEN
			IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
			THEN
				EXIT_STATUS = CMC$_UNTERROR
				GOTO ExitFunction
			END IF
		END IF

		WHEN ERROR IN
			LINPUT #LINES.CH, LINETEXT$
		USE
			CONTINUE ExitFunction IF ERR = 11%
			FILENAME$ = "LINES"
			CONTINUE HelpError
		END WHEN

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), LINETEXT$, LIN%)
		LIN% = 0%
		GOTO NextLine

	END SELECT

 ExitFunction:
	OUTP_UNDEFCODES = EXIT_STATUS
	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), &
		UTL_REPORTX, TITLE(), 0%)
	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = "OUTP_UNDEFCODES"
	RESUME HelpError

32767	END FUNCTION
