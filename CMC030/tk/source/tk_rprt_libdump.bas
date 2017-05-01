1	%TITLE "Library Dump"
	%SBTTL "TK_RPRT_LIBDUMP"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987 BY
	!
	! Computer Management Center
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
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
	! ID:TK060
	!
	! Abstract:HELP
	!	.p
	!
	! Index:
	!
	! Option:
	!
	! Author:
	!
	!	02/11/87 - Robert Peterson
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_RPRT_LIBDUMP
	!	$ LINK/EXE=TK_EXE:*.EXE TK_RPRT_LIBDUMP, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_RPRT_LIBDUMP.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/05/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION LIBR_NODIGSR

	!
	! Declare
	!
	DECLARE INTEGER CONSTANT NUM_LINES = 6000	! Size of the array
	DECLARE INTEGER CONSTANT LIB_INDEX = 2000	! Size of the array

	!
	! Dimension statements
	!
	DIM LINE_NUM$(NUM_LINES), LIB_INDEX$(LIB_INDEX), LIB_CONNECT%(LIB_INDEX)
	DIM RFA LIB_RFA(LIB_INDEX)

	ON ERROR GOTO 19000

	%PAGE

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	LFILE$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	WLDLINE$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

 ReportTitle:
	TITLE$(1%) = "Library Dump"
	TITLE$(2%) = "Tool Kit System"
	TITLE$(3%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************


	!
	! Look up entries in library
	!
	CALL LIBR_INDEX(LFILE$, "*", LIB_INDEX$(), LIB_RFA())

	!
	! Print library entries
	!
	FOR ENTRY% = 1% TO LIB_INDEX
		GOTO ExitProgram IF LIB_INDEX$(ENTRY%) = ""

		GOTO NextText IF COMP_STRING(LEFT(LIB_INDEX$(ENTRY%), LEN(WLDCRD$)), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

		!
		! Get Next Entry if this is a connected key
		!
		GOTO NextText IF LIB_CONNECT%(ENTRY%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "-->" + &
			LIB_INDEX$(ENTRY%) + "<--", 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		FOR LOOP% = ENTRY% + 1% TO LIB_INDEX
			IF LIB_CONNECT%(LOOP%) = 0% AND LIB_RFA(ENTRY%) = LIB_RFA(LOOP%)
			THEN
				LIB_CONNECT%(LOOP%) = -1%
				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "-->" + &
					LIB_INDEX$(LOOP%) + "<--", 0%)

				GOTO ExitProgram IF UTL_REPORTX::STAT

			END IF
		NEXT LOOP%

		ST% = LIBR_NODIGSR(LFILE$, LIB_INDEX$(ENTRY%), LINE_NUM$())

		IF (ST% AND 1%) = 0%
		THEN
			!
			! If text not found in main help file, check out
			! the default help file.
			!
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
				"********* No Text for this key ***********", &
				0%)

			GOTO ExitProgram IF UTL_REPORTX::STAT

			GOTO NextText
		END IF

		WHEN ERROR IN
			CURR_LINE% = VAL%(LINE_NUM$(0%))
		USE
			CURR_LINE% = 0%
		END WHEN

		FOR LOOP% = 1% TO CURR_LINE%

			IF COMP_STRING(LINE_NUM$(LOOP%), WLDLINE$) OR WLDLINE$ = ""
			THEN
				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
					LINE_NUM$(LOOP%), 0%)
			END IF
			GOTO ExitProgram IF UTL_REPORTX::STAT

		NEXT LOOP%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
 NextText:
	NEXT Entry%

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%Page

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	RESUME HelpError

32767	END
