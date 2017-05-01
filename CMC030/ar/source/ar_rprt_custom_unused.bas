1	%TITLE "Print Unused Customer Numbers"
	%SBTTL "AR_RPRT_CUSTOM_UNUSED"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
	!
	! Computer Management Center, Inc.
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
	! ID:AR026
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Unused Customer Numbers\* report lists
	!	all customer numbers which involve no activity in the present or past. They
	!	do not appear in the open, closed, or customer balance files.
	!	.lm -5
	!
	! Index:
	!	.x Accounts Receivable Unused Customer Report
	!	.x Report>Unused Customer Numbers
	!	.x Print>Unused Customer Numbers
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_CUSTOM_UNUSED/LINE
	!	$ LINK/EXECUTABLE=AR_EXE: AR_RPRT_CUSTOM_UNUSED, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_CUSTOM_UNUSED.OBJ;*
	!
	! Author:
	!
	!	06/07/89 - Aaron Redd
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/29/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to assign channel for report
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	10/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!******************************************************************
	! External modules needed
	!******************************************************************

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs)
	!******************************************************************

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP	(AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP	(AR_OPEN)	AR_OPEN_CDD	AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP	(AR_CLOSED)	AR_CLOSED_CDD	AR_CLOSED

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP	(AR_CUSBAL)	AR_CUSBAL_CDD	AR_CUSBAL

	%PAGE

	!******************************************************************
	! Take care of anything else before starting the report
	!******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Initialization:
	!******************************************************************
	! Get ready to begin
	!******************************************************************

	!
	! Initialize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the printing
	!	to begin with the selected item.
	!	.b
	!	A blank field will cause the report to begin with the first item in the
	!	file.
	!
	! Index:
	!	.x From Item>Unused Customer Number Listing
	!	.x Unused Customer Number Listing>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the printing
	!	to end with the selected item.
	!	.b
	!	A blank field will cause the report to end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Unused Customer Number Listing
	!	.x Unused Customer Number Listing>To Item
	!
	!--

	%PAGE

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Receivable Customer Balance file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.OPN"
	USE
		FILENAME$ = "AR_CUSBAL"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Receivable Closed file
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.OPN"
	USE
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Receivable Open Item file
	!
330	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Unneeded Customer Number List"
	TITLE$(2%) = "Accounts Receivable System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Customer#  Description"
	TITLE$(5%) = "."

	!
	! Layouts for printed lines
	!
	LYT_LINE$ = "$CUSTOMER:010,$DESCR:061"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_35CUSTOM.CH%
		ELSE
			FIND #AR_35CUSTOM.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17100	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get next record from the AR Customer file
	!
	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO ExitTotal IF (AR_35CUSTOM::CUSNUM > TO_ITEM$) AND (TO_ITEM$ <> "")

17150	!
	! See if this Customer Number exists in the AR Open Item file
	!
	WHEN ERROR IN
		GET #AR_OPEN.CH%, &
			KEY #0% GE (AR_35CUSTOM::CUSNUM + "          "), &
			REGARDLESS
	USE
		CONTINUE 17200 IF (ERR = 155%)
		FILENAME$ = "AR_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO 17350 IF (AR_OPEN::CUSNUM = AR_35CUSTOM::CUSNUM)

17200	!
	! See if this Customer Number exists in the AR Closed file
	!
	WHEN ERROR IN
		GET #AR_CLOSED.CH%, &
			KEY #0% GE (AR_35CUSTOM::CUSNUM + "          "), &
			REGARDLESS
	USE
		CONTINUE 17250 IF (ERR = 155%)
		FILENAME$ = "AR_CLOSED"
		CONTINUE HelpError
	END WHEN

	GOTO 17350 IF (AR_CLOSED::CUSNUM = AR_35CUSTOM::CUSNUM)

17250	!
	! See if this Customer Number exists in the AR Customer Balance file
	!
	WHEN ERROR IN
		GET #AR_CUSBAL.CH%, &
			KEY #0% GE (AR_35CUSTOM::CUSNUM + "                  "), &
			REGARDLESS
	USE
		CONTINUE 17300 IF (ERR = 155%)
		FILENAME$ = "AR_CUSBAL"
		CONTINUE HelpError
	END WHEN

	GOTO 17350 IF (AR_CUSBAL::CUSNUM = AR_35CUSTOM::CUSNUM)

17300	!
	! Since the Customer number is unused, print it out
	!
	TEXT$ = AR_35CUSTOM::CUSNUM + " " + &
		AR_35CUSTOM::CUSNAM

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Print out totals
	!

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

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report AR_RPRT_CUSTOM_UNUSED
	!******************************************************************
	END
