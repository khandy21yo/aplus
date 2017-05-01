1	%TITLE "Equipment Ledger Close/Purge"
	%SBTTL "EL_SPEC_PURGE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	! ID:EL0012
	!
	! Abstract:HELP
	!	.p
	!	The ^*Equipment Ledger Close/Purge\* Report will contain
	!	the following information about the equipment that is
	!	closed and or purged. Equipment will only be removed if it is
	!	closed and is older than the purge date.
	!	.p
	!	Please note: If there is no purge date entered only completed
	!	equipment will be reported.
	!	Each report is separate.
	!	.b
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	Equipment Number
	!	.le
	!	Description
	!	.le
	!	Type
	!	.le
	!	Class
	!	.le
	!	Open Date
	!	.le
	!	Close Date
	!	.els
	!	.lm -10
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS EL_SOURCE:EL_SPEC_PURGE/LINE
	!	$ LINK/EXE=EL_EXE: EL_SPEC_PURGE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE EL_SPEC_PURGE.OBJ;*
	!
	! AUTHOR:
	!
	!	10/14/92 - Dan Perkins
	!
	! MODIFICATION HISTORY:
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/23/97 - Kevin Handy
	!		Use integer for #key
	!
	!	09/16/97 - Kevin Handy
	!		Lose commented out code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include scope.com
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.HB"
	MAP (SB_CONTROL)	SB_CONTROL_CDD		SB_CONTROL

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*E\* - Equipment Number
	!	.te
	!	^*C\* - Equipment Class
	!	.te
	!	^*T\* - Equipment Type
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the report to
	!	begin printing with a selected item.  The value entered
	!	must be in agreement with field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	An ^*To Item\* field causes the report to end printing
	!	with a selected item.  The value entered must be in agreement
	!	with field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selectd
	!	designated items to be printed by entering a "wildcard"
	!	using the Wildcarding
	!	Technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	PURGE_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(4%), 132%))

	!++
	! Abstract:FLD05
	!	^*(05) Purge Date\*
	!	.b
	!	.lm +5
	!	The ^*Purge Date\* entered in this field controls the
	!	removal of completed equipment. Only equipment older than this
	!	date will be removed and only if it is complete.
	!	An entry in the Purge Date field is required in order for
	!	the program to run.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm +5
	!
	! Index:
	!
	!--

	OPTION$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Report/Purge\*
	!	.b
	!	.lm +5
	!	The ^*Report/Purge\* field determines the
	!	program process.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*R\* - Report Closed Equipment to Purge
	!	.te
	!	^*P\* - Purge Closed Equipment
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Option
	!
	!--

300	!
	! Open and post into control file the purge date and any control here
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_CONTROL.UPD"
	USE
		FILENAME$ = "SB_CONTROL"
		CONTINUE HelpError
	END WHEN

310	!
	! Get the SB_CONTROL record
	!
	WHEN ERROR IN
		GET #SB_CONTROL.CH%, KEY #0% EQ "EL"
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "SB_CONTROL"
		CONTINUE HelpError
	END WHEN

320	!
	! Set status flag = 1
	! then when normal exit takes place reset flag to "0"
	!
	SB_CONTROL::CONTROLFLAG = "1"
	WHEN ERROR IN
		UPDATE #SB_CONTROL.CH%
	USE
		FILENAME$ = "SB_CONTROL"
		CONTINUE HelpError
	END WHEN

400	!
	! Open SB_SUBACCOUNT file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.UPD"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "E"
		K_NUM% = 0%

		IF OPTION$ = "R"
		THEN
			TITLE$(1%) = "CLOSED EQUIPMENT BY EQUIPMENT" + &
				" NUMBER AS OF " + &
				PRNT_DATE(PURGE_DATE$, 8%)
		ELSE
			TITLE$(1%) = " PURGED EQUIPMENT OLDER THAN " + &
				PRNT_DATE(PURGE_DATE$, 8%) + &
				" BY EQUIPMENT NUMBER"
		END IF

	CASE "T"
		K_NUM% = 1%

		IF OPTION$ = "R"
		THEN
			TITLE$(1%) = " CLOSED EQUIPMENT BY EQUIPMENT" + &
				" TYPE AS OF " + &
				PRNT_DATE(PURGE_DATE$, 8%)
		ELSE
			TITLE$(1%) = " PURGED EQUIPMENT OLDER THAN " + &
				PRNT_DATE(PURGE_DATE$, 8%) + &
				" BY EQUIPMENT TYPE"
		END IF

	CASE "C"
		K_NUM% = 2%

		IF OPTION$ = "R"
		THEN
			TITLE$(1%) = " CLOSED EQUIPMENT BY EQUIPMENT" + &
				" CLASS AS OF " + &
				PRNT_DATE(PURGE_DATE$, 8%)
		ELSE
			TITLE$(1%) = " PURGED EQUIPMENT OLDER THAN " + &
				PRNT_DATE(PURGE_DATE$, 8%) + &
				" BY EQUIPMENT CLASS"
		END IF

	END SELECT

	TITLE$(2%) = "Equipment Ledger System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "EqNumber   Description                    " + &
		"JobType JobClass OpenDate   Status CloseDate"

	TITLE$(5%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! If from item is blank then reset Register file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #SB_SUBACCOUNT.CH%, KEY #K_NUM%
		ELSE
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #K_NUM% GE "E" + FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitTotal IF UTL_REPORTX::STAT

	!
	! Get next Order Register record
	!
	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be closed and printed
	!
	SELECT SORTBY$

	CASE "N"
		GOTO ExitTotal &
			IF (SB_SUBACCOUNT::SUBACCOUNT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(SB_SUBACCOUNT::SUBACCOUNT, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal &
			IF (SB_SUBACCOUNT::TTYPE > TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(SB_SUBACCOUNT::TTYPE, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "C"
		GOTO ExitTotal IF (SB_SUBACCOUNT::CLASS > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(SB_SUBACCOUNT::CLASS, -1%), &
			WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	!
	! Evaluate job and see if  status flag = C
	! for indication of closed status and forward to purge process
	!
	GOTO GetNextRec &
		IF SB_SUBACCOUNT::SSTATUS <> "C" OR &
		SB_SUBACCOUNT::EDATE > PURGE_DATE$

	GOTO GetNextRec IF SB_SUBACCOUNT::EDATE = "" AND OPTION$ <> "R"

17200	TEXT$ = SB_SUBACCOUNT::SUBACCOUNT + " " + &
		LEFT(SB_SUBACCOUNT::DESCR, 30%) + " " + &
		SB_SUBACCOUNT::TTYPE + "      " + &
		SB_SUBACCOUNT::CLASS + "     " + &
		PRNT_DATE(SB_SUBACCOUNT::BDATE, 8%) + " " + &
		SB_SUBACCOUNT::SSTATUS + "      " + &
		PRNT_DATE(SB_SUBACCOUNT::EDATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

17350	!
	! Delete record after it was printed
	!
	WHEN ERROR IN
		DELETE #SB_SUBACCOUNT.CH% IF OPTION$ <> "R"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec

 ExitTotal:
17410	WHEN ERROR IN
		GET #SB_CONTROL.CH%, KEY #0% EQ "E"
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "SB_CONTROL"
		CONTINUE HelpError
	END WHEN

	!
	! Set activity status flag back to "0"
	!
	SB_CONTROL::CONTROLFLAG = "0"

	WHEN ERROR IN
		UPDATE #SB_CONTROL.CH%
	USE
		FILENAME$ = "SB_CONTROL"
		CONTINUE HelpError
	END WHEN

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
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Resume to display untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
