1	%TITLE "Sub Ledger Sub Account Report"
	%SBTTL "SB_RPRT_SUBACCOUNT"
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
	! ID:SB0001
	!
	! Abstract:HELP
	!	.p
	!	This report prints out a list of the Sub Ledger
	!	Sub Account file.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SB_SOURCE:SB_RPRT_SUBACCOUNT/LINE
	!	$ LINK/EXECUTABLE=SB_EXE: SB_RPRT_SUBACCOUNT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SB_RPRT_SUBACCOUNT.OBJ;*
	!
	! Author:
	!
	!	02/24/89 - B. Craig Larsen
	!
	! Modification history:
	!
	!	02/26/92 - Kevin Handy
	!		Modified compile statement so that it would
	!		compile the correct program.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/20/97 - Kevin Handy
	!		Don't allocate channel for report
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/16/98 - Kevin Handy
	!		Lose excess %PAGE
	!
	!	11/16/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP	(SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

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

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!
	!	^*(01) Sort by (S,T,C)\*
	!	.p
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.p
	!	Valid settings are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	S - Subaccount number
	!	.le
	!	T - subaccount Type
	!	.le
	!	C - subaccount Class
	!	.els
	!	.lm -10
	!	.p
	!	A setting is required in this field.  No other settings are
	!	valid.
	!
	! Index:
	!	.x Sort by
	! Datatype:TEXT
	! Size:1
	! Valid Input: C,O,T
	! Required
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!
	!	^*(02)From Item\*
	!	.p
	!	The ^*From Item\* field allows the user to enter the
	!	order name that he/she wishes to begin with.
	!	.p
	!	A blank field will cause the report to begin with the first
	!	order in the file.
	!
	! Index:
	! Datatype:TEXT
	! Size:10
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.p
	!	The ^*To Item\* field allows the user to specify the order
	!	he wishes to end the report with.
	!	.p
	!	A blank field will cause the report to end with the last
	!	order in the file.
	!
	! Index:
	! Datatype:TEXT
	! Size:10
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!
	!	^*(04) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects
	!	designated programs by entering a "wildcard"
	!	for Wildcarding
	!	Technique.)
	!
	! Index:
	! Datatype:TEXT
	! Size:10
	!--

	SUBJECT$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	SELECT SORT_BY$
	CASE "S"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  SUBACCOUNT  NUMBER"
	CASE "T"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  SUBACCOUNT  TYPE"
	CASE "C"
		SORT_KEY% = 2%
		ADD_TITLE$ = "BY  SUBACCOUNT  CLASS"
	END SELECT

300	!
	! Open Subaccount  file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "SUBACCOUNT  DESCRIPTION" + ADD_TITLE$
	TITLE$(2%) = "Subaccount System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Sb Subaccount Description                    " + &
		"Ty Clas OnsetDate  S TermDate"
	TITLE$(5%) = ""

	!
	! Layouts for printed line
	!
	LYT_LINE$ = "$SUBJECT:003,$SUBACCOUNT:014,$DESCR:045,$TTYPE:048," + &
		"$CLASS:053,DBDATE:064,$SSTATUS:066,DEDATE:077"


	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		FIND #SB_SUBACCOUNT.CH%, &
			KEY #SORT_KEY% GE SUBJECT$ + FROM_ITEM$, &
			REGARDLESS
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17100	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SELECT SORT_BY$
	CASE "S"
		GOTO ExitTotal IF (SB_SUBACCOUNT::SUBACCOUNT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND COMP_STRING(EDIT$( &
			SB_SUBACCOUNT::SUBACCOUNT, -1%), WLDCRD$) = 0%

	CASE "T"
		GOTO ExitTotal IF (SB_SUBACCOUNT::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND COMP_STRING(EDIT$( &
			SB_SUBACCOUNT::TTYPE, -1%), WLDCRD$) = 0%

	CASE "C"
		GOTO ExitTotal IF (SB_SUBACCOUNT::CLASS > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND COMP_STRING(EDIT$( &
			SB_SUBACCOUNT::CLASS, -1%), WLDCRD$) = 0%

	END SELECT

	!
	! Print out one line
	!
	TEXT$ = SB_SUBACCOUNT::SUBJECT + "  " + &
		SB_SUBACCOUNT::SUBACCOUNT + " " + &
		LEFT(SB_SUBACCOUNT::DESCR, 30%) + " " + &
		SB_SUBACCOUNT::TTYPE + " " + &
		SB_SUBACCOUNT::CLASS + " " + &
		PRNT_DATE(SB_SUBACCOUNT::BDATE, 8%) + " " + &
		SB_SUBACCOUNT::SSTATUS + " " + &
		PRNT_DATE(SB_SUBACCOUNT::EDATE, 8%)

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
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
	! End of report SB_RPRT_SUBACCOUNT
	!******************************************************************
	END
