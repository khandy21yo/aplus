1	%TITLE "Store in File and Then Print Stored Text"
	%SBTTL "OUTP_FILE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OUTP_FILE( LONG OPT, &
		STRING TITLE(), &
		UTL_REPORTX_CDD UTL_REPORTX, &
		STRING TEXT, &
		STRING ADD_TITLE())
	!
	! COPYRIGHT (C) 1989 BY
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
	!	Store TEXT in a temp file and then print stored text when
	!	requested.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	OPT		- is the long option to select inside of this
	!				function.
	!
	!	TITLE()		- is the string array containing the title
	!				info that is printed on the top of
	!				the page.
	!
	!	UTL_REPORTX	- is the structure containing the report info.
	!
	!	TEXT		- is the string containing the text to put in
	!				the temp file if option is OPT_ADDREC
	!				or if option is OPT_CONFIRM it may
	!				contain a message to be printed.
	!
	!	ADD_TITLE()	- is the string array containing additional
	!				title info just for this output.
	!
	!	FUNCTION RETURNS THE FOLLOWING:
	!
	!		CMC$_NORMAL	- normal exit(sucessful).
	!		CMC$_UNTERROR	- program error or unsolicted input
	!					encountered.
	!		CMC$_WARNING	- for the confirm option.
	!
	!
	!	SEEMS TO BE USED ONLY BY SB_TRAN_PURGE
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_FILE
	!	$ LIB FUNC_LIB:CMCFUN/REP OUTP_FILE
	!	$ DELETE OUTP_FILE.OBJ;*
	!
	! AUTHOR:
	!
	!	05/31/89 - B. Craig Larsen
	!
	! MODIFICATION HISTORY:
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update source to V3.6 coding standards.
	!		Changes LINES.CH from WORD to LONG.
	!
	!	03/25/96 - Kevin Handy
	!		Reformat source code.
	!
	!	09/04/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

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

	ON ERROR GOTO 19000

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
	! Add text to temp file
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

		WHEN ERROR IN
			PRINT #LINES.CH, TEXT
		USE
			FILENAME$ = "LINES"
			CONTINUE HelpError
		END WHEN

		CONF_STATUS = CMC$_UNDEFINED

	!
	! Confirm
	!
	CASE OPT_CONFIRM

		IF CONF_STATUS <> CMC$_NORMAL
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT, 0%)
			EXIT_STATUS = CMC$_WARNING
		END IF

	!
	! Print text
	!
	CASE OPT_SUMMARY

		Z% = 1% FOR I% = 1% WHILE TITLE(I%) <> ""
		TITLE(J%) = "" FOR J% = I% + 1% WHILE TITLE(J%) <> ""

		TITLE(I% + X%) = ADD_TITLE(X%) &
			FOR X% = 1% WHILE ADD_TITLE(X%) <> ""
		TITLE(I% + X%) = "."

		LIN% = 999%

400		WHEN ERROR IN
			RESET #LINES.CH
		USE
			CONTINUE 410 IF ERR = 11%
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
			CONTINUE 410 IF ERR = 11%
			FILENAME$ = "LINES"
			CONTINUE HelpError
		END WHEN

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), LINETEXT$, LIN%)
		LIN% = 0%
		GOTO NextLine

410		TITLE(I% + X%) = "" FOR X% = 1% WHILE ADD_TITLE(X%) <> ""

	END SELECT

 ExitFunction:
	OUTP_FILE = EXIT_STATUS
	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = "OUTP_FILE"
	RESUME HelpError

32767	END FUNCTION
