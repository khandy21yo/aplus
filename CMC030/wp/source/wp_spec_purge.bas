1	%TITLE "Order Register Close/Purge"
	%SBTTL "WP_SPEC_PURGE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	! ID:WP051
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Order Register Close/Purge\* contains
	!	the following information about the orders that are
	!	closed and/or purged. Orders will only be removed if they are
	!	closed and are older than the purge date.
	!	.b
	!	Please note: If there is no purge date entered only completed
	!	orders (all items shipped or canceled) will be reported.
	!	Each report is separate.
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Order Number
	!	.le
	!	Order Type
	!	.le
	!	Order Category
	!	.le
	!	Customer Number
	!	.le
	!	Customer Name
	!	.le
	!	Customer PO Number
	!	.le
	!	Order Date
	!	.le
	!	Location
	!	.le
	!	Ship Via
	!	.le
	!	Line
	!	.le
	!	Product
	!	.le
	!	Description
	!	.le
	!	Quantity Ordered
	!	.le
	!	Quantity Shipped
	!	.le
	!	Quantity Cancelled
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Report>Purge
	!	.x Purge>Report
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_SPEC_PURGE/LINE
	!	$ LINK/EXE=WP_EXE: WP_SPEC_PURGE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_SPEC_PURGE.OBJ;*
	!
	! AUTHOR:
	!
	!	08/05/91 - JEFF BEARD
	!
	! MODIFICATION HISTORY:
	!
	!	12/10/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/21/93 - Kevin Handy
	!		Modified to eliminate the eternal loop created
	!		at "ReadRegLine:". Wouldn't leave when
	!		WP_READ_REQREGISTER went to a new job or job line.
	!
	!	09/21/93 - Kevin Handy
	!		Changed references to "JC_JOB" to "SB_SUBACCOUNT"
	!		so that it would be possible to figure out what
	!		was really happening. I didn't like the way
	!		JC_JOB magically got filled in.
	!
	!	09/21/93 - Kevin Handy
	!		Added a check into "ReadRegline:" for the job
	!		number changing to eliminate yet another
	!		eternal loop. (Is this the last one?  Did this
	!		thing EVER get tested?)
	!
	!	09/21/93 - Kevin Handy
	!		Added message "scanning job" so that I could see
	!		if it was running or had locked up again.
	!
	!	09/21/93 - Kevin Handy
	!		Added several comments to help figure out
	!		what is going on, and why it set 'p's in the
	!		file, but didn't purge them.
	!
	!	09/22/93 - Kevin Handy
	!		Modified so that a "end item" will not cause it
	!		to bypass "DoThePurge".
	!
	!	09/22/93 - Kevin Handy
	!		Made input screen in report settings ask for a date
	!		in field 5 instead of a string for purge date.
	!
	!	09/22/93 - Kevin Handy
	!		Modified to make PURGE_DATE$ do something. Was
	!		ignored except for being read into that variable.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	03/21/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/21/96 - Kevin Handy
	!		Added message "Purging..." like had been done
	!		in the scanning section.
	!
	!	04/03/96 - Kevin Handy
	!		Modified to delete SB_SUBACCOUNT record.
	!		Reformat source code.
	!
	!	04/19/96 - Kevin Handy
	!		Modified to only purge J (Jobs) and E (Equipment),
	!		but not S (Salesmen) or P (Program).
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/30/99 - Kevin Handy
	!		Add rounding around balance calculations
	!
	!	03/31/99 - Kevin Handy
	!		Use WHEN ERROR IN so that I could trace the flow
	!		easier.
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
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

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_READ

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	!
	! Keep this stupid include because it is necessary to
	! have this additional include in order to give the correct
	! record length to the file, since Frank set the file up with
	! a variable record layout.
	!
	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE
	DECLARE			WP_REGLINE_CDD		WP_REGLINE_READ

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER
	DECLARE			WP_REQREGISTER_CDD	WP_REQREGISTER_READ

	!
	! Declare external functions
	!
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG	FUNCTION WP_READ_REGLINE
	EXTERNAL LONG	FUNCTION WP_READ_REQREGISTER

	%PAGE

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

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
	!	^*J\* - Job Number
	!	.le
	!	^*C\* - Order Category
	!	.le
	!	^*T\* - Order Type
	!	.els
	!	.lm -5
	!	.b
	!	A setting is required in this field.  No other settings are
	!	valid.
	!
	! Index:
	!	.x Sort by>Purge
	!	.x Purge>Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02)From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field determines the item with which the
	!	report will begin printing.
	!	The value entered must be
	!	in agreement with the value in field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Purge
	!	.x Purge>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field determines the item the report
	!	will end printing on.
	!	The value entered must be
	!	in agreement with the value in field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Purge
	!	.x Purge>To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

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
	!	.x Wildcard>Purge
	!	.x Purge>Wildcard
	!
	!--

	PURGE_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(4%), 132%))

	!++
	! Abstract:FLD05
	!	^*(05) Purge Date\*
	!	.b
	!	.lm +5
	!	The ^*Purge Date\* entered in this field controls the
	!	removal of completed orders. Only orders older than this
	!	date will be removed and only if they are complete.
	!	.b
	!	Note:  If no date is entered (blank) then no purge process or
	!	report will be generated.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Purge Date>Purge
	!	.x Purge>Purge Date
	!
	!--

300	!
	! Open Order Register file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.PST"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Order Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.PST"
	USE
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

400	!
	! Open and post into control file the purge date and any control here
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.PST"
	USE
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "J"
		K_NUM% = 0%
		TITLE$(1%) = " Purge Jobs By Job Number "

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = " Purge Jobs By Class "

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = " Purge Jobs By Type "

	END SELECT


 ReportTitle:
	IF PURGE_DATE$ = ""
	THEN
		TEXT$ = "."
	ELSE
		TEXT$ = "Purge date " + PRNT_DATE(PURGE_DATE$, 8%)
	END IF

	!
	! Title
	!
	TITLE$(2%) = "Work In Process System"
	TITLE$(3%) = TEXT$
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "JobNumber  Line Description            " + &
		"                         Qty StartDate " + &
		" CompDate   Batch"

	TITLE$(6%) = "                ReqNum     Reqline Product"  + &
		"        Description                 " + &
		"                    Qty TranDate   Operator" + &
		"   Period"

	TITLE$(7%) = "."

17000	!***************************************************************
	! OUTPUT REPORT
	!
	! Scans through file, setting status of records to be purged
	! to a 'P'.
	!***************************************************************

	!
	! If from item is blank then reset Register file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			FIND #SB_SUBACCOUNT.CH%, KEY #K_NUM% GE "J"
		ELSE
			FIND #SB_SUBACCOUNT.CH%, KEY #K_NUM% GE "J" + FROM_ITEM$
		END IF
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitTotal IF UTL_REPORTX::STAT

17030	!
	! Get next Order Register record
	!
	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE DoThePurge IF ERR = 11%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be closed and printed
	!
	SELECT SORTBY$

	CASE "J"
		GOTO DoThePurge IF (SB_SUBACCOUNT::SUBACCOUNT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
				SB_SUBACCOUNT::SUBACCOUNT, -1%), WLDCRD$) = 0%
		END IF

	CASE "C"
		GOTO DoThePurge IF (SB_SUBACCOUNT::CLASS > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
				SB_SUBACCOUNT::CLASS, -1%), WLDCRD$) = 0%
		END IF

	CASE "T"
		GOTO DoThePurge IF (SB_SUBACCOUNT::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		IF WLDCRD$ <> ""
		THEN
			GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
				SB_SUBACCOUNT::TTYPE, -1%), WLDCRD$) = 0%
		END IF
	END SELECT

	!
	! See if the beginning date is not early enough to purge
	!
	IF (PURGE_DATE$ <> "")
	THEN
		GOTO GetNextRec IF PURGE_DATE$ < SB_SUBACCOUNT::BDATE
	END IF

	!
	! Tell them what we are looking at
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Scanning Job: " + SB_SUBACCOUNT::SUBACCOUNT, 1%)

	NEXT_LINE$ = "    "

 ReadRegline:
17040	IF WP_READ_REGLINE(SB_SUBACCOUNT::SUBACCOUNT, NEXT_LINE$, &
		"GT", WP_REGLINE_READ, QTY()) <> &
		CMC$_NORMAL
	THEN
		!
		! If there are no more lines in the WP register, then it
		! should be purgable
		!
		SB_SUBACCOUNT::SSTATUS = "P"

		WHEN ERROR IN
			UPDATE #SB_SUBACCOUNT.CH%
		USE
			FILENAME$ = "SB_SUBACCOUNT"
			CONTINUE HelpError
		END WHEN
		GOTO GetNextRec
	END IF

	IF (SB_SUBACCOUNT::SUBACCOUNT <> WP_REGLINE_READ::JOB)
	THEN
		!
		! Read all the WP Register lines
		!
		SB_SUBACCOUNT::SSTATUS = "P"

		WHEN ERROR IN
			UPDATE #SB_SUBACCOUNT.CH%
		USE
			FILENAME$ = "SB_SUBACCOUNT"
			CONTINUE HelpError
		END WHEN

		GOTO GetNextRec
	END IF

	!
	! See if the dates are not early enough to purge
	!
	IF (PURGE_DATE$ <> "")
	THEN
		!
		! Skip out if we have activity out of the date range
		!
		GOTO GetNextRec IF PURGE_DATE$ < WP_REGLINE_READ::START_DATE
		GOTO GetNextRec IF PURGE_DATE$ < WP_REGLINE_READ::COMP_DATE
	END IF

	NEXT_LINE$ = WP_REGLINE_READ::LLINE

	!
	! Skip out if it doesn't balance to less than zero
	!
	BALANCE = FUNC_ROUND(QTY(1%) - QTY(2%) - QTY(3%), 4%)
	GOTO GetNextRec IF BALANCE > 0.0

	REQNUMBER$ = "          "
	REQLINE$   = "    "

 ReadReqRegister:
	GOTO ReadRegline &
		IF WP_READ_REQREGISTER(SB_SUBACCOUNT::SUBACCOUNT, NEXT_LINE$, &
		REQNUMBER$ + REQLINE$, "GT", &
		WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL

	GOTO ReadRegline IF (SB_SUBACCOUNT::SUBACCOUNT <> &
		WP_REQREGISTER_READ::JOB) OR &
		(NEXT_LINE$ <> WP_REQREGISTER::LLINE)

	!
	! See if the dates are not early enough to purge
	!
	IF (PURGE_DATE$ <> "")
	THEN
		GOTO GetNextRec IF PURGE_DATE$ < WP_REQREGISTER_READ::TRANDATE
	END IF

	REQNUMBER$ = WP_REQREGISTER_READ::REQNUM
	REQLINE$   = WP_REQREGISTER_READ::REQLIN

	!
	! Validate a less than zero balance
	!
	BALANCE = FUNC_ROUND(QTY(1%) - QTY(2%) - QTY(3%), 4%)
	GOTO GetNextRec IF BALANCE > 0.0

	GOTO ReadReqRegister

	%PAGE

	!*******************************************************************
	! Actually purge out records
	!*******************************************************************

  DoThePurge:
17050	!
	! If from item is blank then reset Register file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #SB_SUBACCOUNT.CH%, KEY #K_NUM%
		ELSE
			FIND #SB_SUBACCOUNT.CH%, KEY #K_NUM% GE FROM_ITEM$
		END IF
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	TEXT$ = ""

17100	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 5%
			RETRY
		END IF
		CONTINUE ExitTotal IF ERR = 11% OR ERR = 155%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	GOTO 17100 IF SB_SUBACCOUNT::SSTATUS <> "P"

	CALL ENTR_3MESSAGE(SCOPE, "Purging Job: " + &
		SB_SUBACCOUNT::SUBACCOUNT, 1%)

17150	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%) IF TEXT$ <> ""

	TEXT$ = SB_SUBACCOUNT::SUBACCOUNT + " " + &
		SB_SUBACCOUNT::DESCR + " " + &
		PRNT_DATE(SB_SUBACCOUNT::BDATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = ""

17200	WHEN ERROR IN
		GET #WP_REGLINE.CH%, KEY #0% EQ SB_SUBACCOUNT::SUBACCOUNT
	USE
		IF ERR = 154%
		THEN
			SLEEP 5%
			RETRY
		END IF
		CONTINUE 17400 IF ERR = 11% OR ERR = 155%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%) IF TEXT$ <> ""

	TEXT$ = WP_REGLINE::JOB + " " + &
		WP_REGLINE::LLINE + " " + &
		WP_REGLINE::DESCR + " " + &
		FORMAT$(WP_REGLINE::QTY, "###,###.##") + " " + &
		PRNT_DATE(WP_REGLINE::START_DATE, 8%) + " " + &
		PRNT_DATE(WP_REGLINE::COMP_DATE, 8%) + " " + &
		WP_REGLINE::BATCH

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	REGLINE$ = WP_REGLINE::LLINE

17250	WHEN ERROR IN
		DELETE #WP_REGLINE.CH%
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

17275	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, &
			KEY #0% EQ SB_SUBACCOUNT::SUBACCOUNT + REGLINE$
	USE
		IF ERR = 154%
		THEN
			SLEEP 5%
			RETRY
		END IF
		CONTINUE 17200 IF ERR = 11% OR ERR = 155%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	V% = PD_EXAM_PRODUCT(WP_REQREGISTER::PRODUCT, PD_PRODUCT_READ)

	PD_PRODUCT_READ::DESCRIPTION = &
		STRING$(LEN(PD_PRODUCT_READ::DESCRIPTION), &
		A"?"B) IF V% <> CMC$_NORMAL

	TEXT$ = WP_REGLINE::JOB + " " + &
		WP_REGLINE::LLINE + " " + &
		WP_REQREGISTER::REQNUM + " " + &
		WP_REQREGISTER::REQLIN + "    " + &
		WP_REQREGISTER::PRODUCT + " " + &
		PD_PRODUCT_READ::DESCRIPTION + " " + &
		FORMAT$(WP_REQREGISTER::QTY, "###,###.##") + " " + &
		PRNT_DATE(WP_REQREGISTER::TRANDATE, 8%) + " " + &
		WP_REQREGISTER::OPERATOR + " " + &
		WP_REQREGISTER::PERIOD

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

17300	WHEN ERROR IN
		DELETE #WP_REQREGISTER.CH%
	USE
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	GOTO 17275

17400	!
	! Remove this subaccount now
	!
	IF (SB_SUBACCOUNT::SUBJECT = "J") OR (SB_SUBACCOUNT::SUBJECT = "E")
	THEN
		WHEN ERROR IN
			DELETE #SB_SUBACCOUNT.CH%
		USE
			FILENAME$ = "SB_SUBACCOUNT"
			CONTINUE HelpError
		END WHEN
	END IF

	GOTO 17100

 ExitTotal:
	!
	! Handle end of report
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
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

32767	END
