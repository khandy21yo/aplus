1	%TITLE "Job Register Close/Purge"
	%SBTTL "JC_SPEC_PURGE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! ID:JC0012
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Job Register Purge\* program purges closed
	!	Jobs which are older than the required Purge Date.
	!	It prints a report which contains the following
	!	information about the jobs that are purged.
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Job Number
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
	!	.lm -5
	!	.b
	!	^*NOTE:\* The program will check for and purge Work in Process
	!	register files if these files are found, and if the register
	!	records are connected to jobs which are closed.
	!
	! Index:
	!	.x Report>Job Entry Purge
	!	.x Job Entry Purge>Report
	!
	! Compile:
	!
	!	$ BAS JC_SOURCE:JC_SPEC_PURGE/LINE
	!	$ LINK/EXE=JC_EXE: JC_SPEC_PURGE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE JC_SPEC_PURGE.OBJ;*
	!
	! Author:
	!
	!	12/05/91 - Dan Perkins
	!
	! Modification History:
	!
	!	02/04/92 - Kevin Handy
	!		Removed junk (check)
	!
	!	12/04/92 - Dan Perkins
	!		Check WP Registers before purging JC record.
	!		Use function COMP_ARRAY instead of COMP_STRING.
	!
	!	12/09/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/14/93 - Frank F. Starman
	!		Remove From Item, To.Item, Wildcard and set status
	!		to A if there is still balance on a job.
	!
	!	03/25/93 - Dan Perkins
	!		Modified program to purge only.  Status flag must
	!		be set to "C" in order to purge records.
	!		Set SB_CONTROL::CONTROLFLAG to 4 (purge) instead of
	!		1 (closing).  Update SB_CONTROL::CDATE to today's date
	!		and SB_CONTROL::CTIME to current time.  This will
	!		indicate the last time the Control file was updated
	!		for some reason.  Set SB_CONTROL::BATCH to "PURGE" to
	!		indicate that the purge was run.
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/26/93 - Dan Perkins
	!		Must have a date in SB_SUBACCOUNT::EDATE in order
	!		to purge records.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	01/30/97 - Kevin Handy
	!		Lose lot's of commented out code.
	!
	!	06/09/97 - Kevin Handy
	!		Lose TOTAL_AMOUNT variable.
	!		Lose references to SB_ACCOUNT file.
	!
	!	09/25/97 - Kevin Handy
	!		Comment out null loops through SB_BALANCE,
	!		WP_REGLINE, WP_REQLINE.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/01/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE
	DECLARE			WP_REGLINE_CDD		WP_REGLINE_READ

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER
	DECLARE			WP_REQREGISTER_CDD	WP_REQREGISTER_READ

	%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.HB"
	MAP (SB_BALANCE)	SB_BALANCE_CDD		SB_BALANCE

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
	!	^*(01) Sort by (J,C,T)\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	J - Job Number
	!	.le
	!	C - Job Class
	!	.le
	!	T - Job Type
	!	.els
	!	.lm -5
	!	A setting is required in this field.  No other settings are
	!	valid.
	!	.lm -5
	!
	! Index:
	!	.x Sort by
	!
	!--

	PURGE_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(1%), 132%))

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters the
	!	item with which the report is to begin.  The value
	!	entered must be in agreement with the value in
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item
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
		GET #SB_CONTROL.CH%, KEY #0% EQ "JC"
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
	! Set status flag = 4, for purge, then when normal exit takes place,
	! reset flag to "0"
	!
	SB_CONTROL::CONTROLFLAG = "4"

	!
	! post new purge date into control record
	!
	SB_CONTROL::CDATE = DATE_TODAY
	SB_CONTROL::CTIME = TIME_NOW
	SB_CONTROL::BATCH = "PURGE"

	WHEN ERROR IN
		UPDATE #SB_CONTROL.CH%
	USE
		FILENAME$ = "SB_CONTROL"
		CONTINUE HelpError
	END WHEN

350	!
	! Open SB_SUBACCOUNT file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.PST"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

360	!
	! Open WP_REGLINE file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.PST"
	USE
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

370	!
	! Open WP_REQREGISTER file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.PST"
	USE
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

380	!
	! Open SB_BALANCE file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_BALANCE.PST"
	USE
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "J"
		K_NUM% = 0%

		TITLE$(1%) = " PURGED JOBS AS OF " + &
			PRNT_DATE(PURGE_DATE$, 8%) + &
			" BY JOB NUMBER"

	CASE "T"
		K_NUM% = 1%

		TITLE$(1%) = " PURGED JOBS AS OF " + &
			PRNT_DATE(PURGE_DATE$, 8%) + &
			" BY JOB TYPE"

	CASE "C"
		K_NUM% = 2%

		TITLE$(1%) = " PURGED JOBS AS OF " + &
			PRNT_DATE(PURGE_DATE$, 8%) + &
			" BY JOB CLASS"

	END SELECT

	TITLE$(2%) = " Job Costing System"
	TITLE$(3%) = ""

	TITLE$(4%) = "JobNumber  Description                    " + &
		"JobType JobClass OpenDate   CloseDate  Reglin ReqLin Balanc"

	TITLE$(5%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		FIND #SB_SUBACCOUNT.CH%, KEY #K_NUM% GE "J"
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	!
	! Main loop starts here
	!
	GOTO ExitTotal IF UTL_REPORTX::STAT

	!
	! Get next Order Register record
	!
17020	WHEN ERROR IN
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
	GOTO ExitTotal IF SB_SUBACCOUNT::SUBJECT <> "J"

	!
	! Make sure the JOB STATUS is "C" for closed
	!
	GOTO GetNextRec IF SB_SUBACCOUNT::SSTATUS <> "C"

	!
	! Make sure an end date is in the record
	!
	GOTO GetNextRec IF SB_SUBACCOUNT::EDATE <= "00000000"

	!
	! Make sure the end date is in the purge date range.
	!
	GOTO GetNextRec IF SB_SUBACCOUNT::EDATE > PURGE_DATE$

	!
	! Test for WP_REGLINE and balances
	!
 !	TESTLINE$ = SPACE$(LEN(WP_REGLINE_READ::LLINE))

 ReadRegLine:
 !	GOTO TestBalance IF WP_READ_REGLINE(SB_SUBACCOUNT::SUBACCOUNT, &
 !		TESTLINE$, "GT", WP_REGLINE_READ, QTY()) <> CMC$_NORMAL
 !
 !	TESTLINE$, LASTLINE$ = WP_REGLINE_READ::LLINE
 !
 !	REQLINE$ = SPACE$(LEN(WP_REQREGISTER_READ::REQNUM + &
 !		WP_REQREGISTER_READ::REQLIN))

	!
	! Test for WP_REREQREGISTER and balances
	!
 ReadReqLine:
 !	GOTO ReadRegLine IF LASTLINE$ <> TESTLINE$ OR &
 !		WP_READ_REQREGISTER (SB_SUBACCOUNT::SUBACCOUNT, &
 !		LASTLINE$, REQLINE$, &
 !		"GT", WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL
 !
 !	LASTLINE$ = WP_REQREGISTER_READ::LLINE
 !	REQLINE$  = WP_REQREGISTER_READ::REQNUM + WP_REQREGISTER_READ::REQLIN
 !
 !	GOTO ReadReqLine

 TestBalance:

17200 !	FIND #SB_BALANCE.CH%, KEY #1% EQ SB_CONTROL::PERIOD + &
 !		SB_CONTROL::SYSTEM + SB_SUBACCOUNT::SUBACCOUNT, REGARDLESS

 NextBalance:
17220 !	GET #SB_BALANCE.CH%, REGARDLESS
 !
 !	GOTO PrintText &
 !		IF SB_BALANCE::PERIOD <> SB_CONTROL::PERIOD OR &
 !		SB_BALANCE::SYSTEM <> SB_CONTROL::SYSTEM OR &
 !		SB_BALANCE::SUBACCOUNT <> SB_SUBACCOUNT::SUBACCOUNT
 !
 !	GOTO NextBalance

 PrintText:
	WP_COUNT% = 0%
	WP_REQREGISTER% = 0%
	SB_BALANCE% = 0%

	!
	! Delete records
	!
	! Delete WP_REGLINE record
	!
17300	WHEN ERROR IN
		FIND #WP_REGLINE.CH%, KEY #0% EQ SB_SUBACCOUNT::SUBACCOUNT
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 17400 IF ERR = 155%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

17320	WHEN ERROR IN
		GET #WP_REGLINE.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 17400 IF ERR = 11%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO 17400 IF WP_REGLINE::JOB <> SB_SUBACCOUNT::SUBACCOUNT

17330	WHEN ERROR IN
		DELETE #WP_REGLINE.CH%
	USE
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	WP_COUNT% = WP_COUNT% + 1%

	GOTO 17320

	!
	! Delete WP_REQREGISTER record
	!
17400	WHEN ERROR IN
		FIND #WP_REQREGISTER.CH%, KEY #0% EQ SB_SUBACCOUNT::SUBACCOUNT
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 17500 IF ERR = 155%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

17420	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 17500 IF ERR = 11%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	GOTO 17500 IF WP_REQREGISTER::JOB <> SB_SUBACCOUNT::SUBACCOUNT

17430	WHEN ERROR IN
		DELETE #WP_REQREGISTER.CH%
	USE
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	WP_REQREGISTER% = WP_REQREGISTER% + 1%

	GOTO 17420

	!
	! Delete SB_BALANCE record
	!
17500	WHEN ERROR IN
		FIND #SB_BALANCE.CH%, KEY #0% EQ SB_CONTROL::SYSTEM + &
			SB_SUBACCOUNT::SUBACCOUNT
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 17600 IF ERR = 155%
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

17520	WHEN ERROR IN
		GET #SB_BALANCE.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 17600 IF ERR = 11%
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO 17600 IF SB_BALANCE::SYSTEM <> SB_CONTROL::SYSTEM OR &
		SB_BALANCE::SUBACCOUNT <> SB_SUBACCOUNT::SUBACCOUNT

17530	WHEN ERROR IN
		DELETE #SB_BALANCE.CH%
	USE
		FILENAME$ = "SB_BALANCE"
		CONTINUE HelpError
	END WHEN

	SB_BALANCE% = SB_BALANCE% + 1%

	GOTO 17520

	!
	! Delete the JOB record
	!
17600	WHEN ERROR IN
		DELETE #SB_SUBACCOUNT.CH%
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	TEXT$ = SB_SUBACCOUNT::SUBACCOUNT + " " + &
		LEFT(SB_SUBACCOUNT::DESCR, 30%) + " " + &
		SB_SUBACCOUNT::TTYPE + "      " + &
		SB_SUBACCOUNT::CLASS + "     " + &
		PRNT_DATE(SB_SUBACCOUNT::BDATE, 8%) + " " + &
		PRNT_DATE(SB_SUBACCOUNT::EDATE, 8%) + " " + &
		FORMAT$(WP_COUNT%, "###### ") + &
		FORMAT$(WP_REQREGISTER%, "###### ") + &
		FORMAT$(SB_BALANCE%, "######")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO GetNextRec

 ExitTotal:
17700	WHEN ERROR IN
		GET #SB_CONTROL.CH%, KEY #0% EQ "J"
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
	!+-+-+
	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters the item with
	!	which the report is to end.  The value entered must be in
	!	agreement with the value in field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	using the Wildcarding Technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD05
	!	^*(05) Purge Date\*
	!	.b
	!	.lm +5
	!	The ^*Purge Date\* field controls the
	!	removal of completed jobs. Only jobs older than this
	!	date will be removed and only if they are complete.
	!	An entry in the Purge Date field is required in order for
	!	the program to run.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Purge Date>Job Entry Purge/Report
	!	.x Job Entry Purge/report>Purge Date
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD06
	!	^*(06) Report/Purge\*
	!	.b
	!	.lm +5
	!	The ^*Report/Purge\* field will determine the program
	!	process.
	!	.b
	!	Valid settings are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	^*R\* - Report Closed Jobs to Purge
	!	.le
	!	^*P\* - Purge Closed Jobs
	!	.els
	!	.lm -5
	!	A setting is required in this field.  No other settings are
	!	valid.
	!	.lm -5
	!
	! Index:
	!	.x Option
	!
	!--
