1	%TITLE "Library Screens"
	%SBTTL "TK_RPRT_LIBREP"
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
	! ID:TK011
	!
	! Abstract:HELP
	!	.p
	!	This report dumps out reports/screens that are stored in
	!	a particular library.
	!	They will be printed out one per page, without any titles.
	!
	! Index:
	!	.X Report>Windows
	!	.x Windows>Report
	!
	! Option:
	!
	! Author:
	!
	!	03/31/88 - Frank Starman
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_RPRT_LIBREP
	!	$ LINK/EXE=TK_EXE:*.EXE TK_RPRT_LIBREP, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_RPRT_LIBREP.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/16/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/20/97 - Kevin Handy
	!		Don't allocate channel for report
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

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	LFILE$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Library Name\*
	!	.p
	!	This field specifies the library that is to be
	!	printed.  Screens and reports exist in files coded as
	!	"^*WINDOWS__\*??", where ?? is the system name.  Help
	!	text is in files coded as "^*HELP__\*??".
	!
	! Index:
	!	.x Library name>Windows>Report
	!	.x Report>Windows>Library name
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) Wildcard\*
	!	.p
	!	This field selects specific items out of the
	!	library using wildcard characters.
	!	i.e. if you want all
	!	reports, you will enter "^*_*$REPORT\*" here.
	!
	! Index:
	!	.x Wildcard>Windows>Report
	!	.x Report>Windows>Wildcard
	!
	!--


	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************


	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR I% = 1% TO 20%

	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, SPACE$(30%) + LFILE$, 0%)
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, SPACE$(30%) + &
		DATE$(0%), 0%)
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, SPACE$(30%) + &
		TIME$(0%), 0%)

	LINE_COUNT% = 23%

	!
	! Look up entries in library
	!
	CALL LIBR_INDEX(LFILE$, WLDCRD$, LIB_INDEX$(), LIB_RFA())


	!
	! Print library entries
	!
	FOR ENTRY% = 1% TO LIB_INDEX
		GOTO ExitProgram IF LIB_INDEX$(ENTRY%) = ""

		GOTO ExitProgram IF UTL_REPORTX::STAT

		CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
			FOR LOOP% = LINE_COUNT% + 1% TO 66%
		LINE_COUNT% = 0%

		ST% = LIBR_NODIGSR(LFILE$, LIB_INDEX$(ENTRY%), LINE_NUM$())

		WHEN ERROR IN
			CURR_LINE% = VAL%(LINE_NUM$(0%))
		USE
			CURR_LINE% = 0%
		END WHEN

		FOR LOOP% = 1% TO CURR_LINE%

			CALL OUTP_LINENOTITLE(LYT_LINE$, &
				UTL_REPORTX, LINE_NUM$(LOOP%), 0%)
			LINE_COUNT% = LINE_COUNT% + 1%

			GOTO ExitProgram IF UTL_REPORTX::STAT

		NEXT LOOP%

 NextText:
	NEXT ENTRY%

 ExitProgram:
	CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
		FOR I% = LINE_COUNT% + 1% TO 66%

	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

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
