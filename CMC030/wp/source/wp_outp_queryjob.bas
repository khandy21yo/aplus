1	%TITLE "Job Query Report"
	%SBTTL "WP_OUTP_QUERYJOB"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_OUTP_QUERYJOB(JC_JOB_CDD JC_JOB)

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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print\* program prints the Job Query report according to the
	!	specifications.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_OUTP_QUERYJOB/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP WP_OUTP_QUERYJOB
	!	$ DELETE WP_OUTP_QUERYJOB.OBJ;*
	!
	! Author:
	!
	!	07/31/91 - Craig Tanner
	!
	! Modification History:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/17/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/05/97 - Kevin Handy
	!		Use OUTP_INITFORM function
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add several REGARDLESS clauses
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD	WP_REGLINE
	DECLARE			WP_REGLINE_CDD	WP_REGLINE_READ

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER
	DECLARE			WP_REQREGISTER_CDD	WP_REQREGISTER_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION WP_READ_REGLINE
	EXTERNAL LONG	FUNCTION WP_READ_REQREGISTER
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	TEMP_PROGRAM$ = SCOPE::PRG_PROGRAM
	TEMP_ITEM$ = SCOPE::PRG_ITEM
	TEMP_IDENT$ = SCOPE::PRG_IDENT

	SCOPE::PRG_PROGRAM = "OUTP_QUERYJOB"
	SCOPE::PRG_ITEM = "HELP"
	SCOPE::PRG_IDENT = "H"

	WP_OUTP_QUERYJOB = 0%
	REPORT$ = "WP0040"

400	!******************************************************************
	! Set up the report settings screen
	!******************************************************************
	!
	! Ask user to change settings
	!
	GOTO ExitFunction &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	!
	! Set up from user input
	!
	WILDJOBLINE$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Job Line Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Job Line Wildcard\* prints
	!	selected job lines using the "Wildcarding" technique.
	!	.b
	!	The field allows for twenty (20) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Job Line
	!
	!--

	PRINT.JDETAIL$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) Job Detail\*
	!	.b
	!	.lm +5
	!	The ^*Job Detail\* selects whether
	!	all register lines are to be printed along with a line summary,
	!	or if the line summary is to be printed alone.
	!	.lm -5
	!
	! Index:
	!	.x Detail>Job
	!
	!--

	WILDREQ$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) Requisition Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Requisition Wildcard\* prints
	!	selected requisition numbers using the "Wildcarding" technique.
	!	.b
	!	The field allows for twenty (20) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Requisition number
	!
	!--
	WILDREQLINE$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Requisition Line Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Requisition Line Wildcard\* prints
	!	selected requisition line numbers using the "Wildcarding" technique.
	!	.b
	!	The field allows for twenty (20) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Requisition line number
	!
	!--

	PRINT.RDETAIL$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(05) Requisition Detail\*
	!	.b
	!	.lm +5
	!	The ^*Requisition Detail\* selects whether
	!	all requisition register lines are to be printed along with a summary
	!	or if the summary is to be printed alone.
	!	.lm -5
	!
	! Index:
	!	.x Detail>Requistion
	!
	!--

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "QUERY OF JOB# " + JC_JOB::JOB + " " + JC_JOB::DESCR
	TITLE$(2%) = "Work in Process System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Line       LType ItemCode       Description                              QtyOrdere" + &
		"d QtyComplete QtyCancel   Balance"
	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

 PrintItNow:
	!
	! Build and print the Header crap out now
	!
	TEXT$ = "Job Type: " + JC_JOB::TTYPE + "   " + &
		"Job Class: " + JC_JOB::CLASS + "      " + &
		"Job Status: " + JC_JOB::SSTATUS + " " + &
		"BeginDate: " + PRNT_DATE(JC_JOB::BDATE, 8%) + " " + &
		"EndDate: " + PRNT_DATE(JC_JOB::EDATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Location: " + JC_JOB::LOCATION + " " + &
		"Operator: " + JC_JOB::OPERATOR + " " + &
		"Ref: " + JC_JOB::REFNO

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	NEXT_LINE$ = "   "

 ReadRegLine:

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%) IF BLANKL% = 0%

	BLANKL% = 1%

	!
	! Get the job line and check agaist wild card.
	!
	GOTO NewOrder IF WP_READ_REGLINE(JC_JOB::JOB, NEXT_LINE$, "GT", &
		WP_REGLINE_READ, QTY()) <> CMC$_NORMAL

	NEXT_LINE$ = WP_REGLINE_READ::LLINE

	GOTO ReadRegLine &
		IF COMP_STRING(EDIT$(WP_REGLINE_READ::LLINE, -1%), &
		WILDJOBLINE$) = 0% &
		AND WILDJOBLINE$ <> ""

	BALANCE = QTY(1%) - QTY(2%) - QTY(3%)

	BALANCE = 0.0 IF BALANCE <= 0.0

	TEXT$ = WP_REGLINE_READ::LLINE + "       " + &
		WP_REGLINE_READ::TTYPE + "     " + &
		WP_REGLINE_READ::ITEMCODE + " " + &
		WP_REGLINE_READ::DESCR + "  " + &
		FORMAT$(QTY(1%), "######.##") + "   " + &
		FORMAT$(QTY(2%), "######.##") + " " + &
		FORMAT$(QTY(3%), "######.##") + " " + &
		FORMAT$(BALANCE, "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	BLANKL% = 0%

	GOSUB PrintDetail IF PRINT.JDETAIL$ = "Y"

	!
	! Set up a number of variables to be used later on.
	!
	REQNUMBER$ = "          " ! Necessary for using WP_READ_REQREGISTER
	REQLINE$ = "    "
	PRINT_TITLE% = 1%	! Both used to keep from repeating titles
	NEW_TITLE% = 1%

 NextRequisition:
	GOTO ReadRegline IF &
		WP_READ_REQREGISTER(JC_JOB::JOB, WP_REGLINE_READ::LLINE, &
		REQNUMBER$ + REQLINE$, "GT", &
		WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL

	REQNUMBER$ = WP_REQREGISTER_READ::REQNUM
	REQLINE$   = WP_REQREGISTER_READ::REQLIN

	GOTO NextRequisition &
		IF COMP_STRING(EDIT$(WP_REQREGISTER_READ::REQNUM, -1%), &
		WILDREQ$) = 0% &
		AND WILDREQ$ <> ""

	!
	! Check if time to print blank line between requisition line blocks,
	! else time for line between requisition number blocks.
	!
	IF REQ_NUMB$ = WP_REQREGISTER_READ::REQNUM
	THEN
		TEXT$ = WP_REQREGISTER_READ::LLINE + "     " + &
			WP_REQREGISTER_READ::REQNUM
	ELSE
		TEXT$ = WP_REQREGISTER_READ::LLINE
	END IF

	IF NOT PRINT_TITLE%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! Try and print title for entire set of requisitions.
	!
	TEXT$ = WP_REQREGISTER_READ::LLINE + &
		"     ReqNum     ReqLin      Product        D" + &
		"escription                            Q" + &
		"tyOrdered  QtyComplete  QtyCancel     Balance"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%) IF PRINT_TITLE%

	PRINT_TITLE% = 0%

	!
	! Get product description
	!
	PD_PRODUCT::DESCRIPTION = STRING$(40%, A"?"B)

17100	IF PD_PRODUCT.CH% = 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
		USE
			CONTINUE 17120 IF ERR = 155% OR ERR = 5%
			FILENAME$ = "PD_PRODUCT"
			CONTINUE HelpError
		END WHEN
	END IF

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ WP_REQREGISTER_READ::PRODUCT, &
			REGARDLESS
	USE
		CONTINUE 17120 IF ERR = 155% OR ERR = 5%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

17120	BALANCE = QTY(1%) - QTY(2%) - QTY(3%)

	BALANCE = 0.0 IF BALANCE <= 0.0

	TEXT$ = WP_REQREGISTER_READ::LLINE + "     " + &
		WP_REQREGISTER_READ::REQNUM + " " + &
		WP_REQREGISTER_READ::REQLIN + "        " + &
		WP_REQREGISTER_READ::PRODUCT + " " + &
		LEFT$(PD_PRODUCT::DESCRIPTION, 39%) + " " + &
		FORMAT$(QTY(1%), "######.##") + "    " + &
		FORMAT$(QTY(2%), "######.##") + "  " + &
		FORMAT$(QTY(3%), "######.##") + "  " + &
		FORMAT$(BALANCE, "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOSUB PrintReqDetail IF PRINT.RDETAIL$ = "Y"

	!
	! Go for next line
	!
	GOTO NextRequisition

 NewOrder:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

 ExitFunction:
	CALL OUTP_FINISH(UTL_REPORTX)

17400	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$
	SCOPE::PRG_ITEM = TEMP_ITEM$
	SCOPE::PRG_IDENT = TEMP_IDENT$

	EXIT FUNCTION

	%PAGE

 PrintDetail:
17510	!
	! Open file
	!
	IF WP_REGLINE.CH% = 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.OPN"
		USE
			CONTINUE 17550 IF ERR = 5%
			FILENAME$ = "WP_REGLINE"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Try to find any line for the header
	!
	WHEN ERROR IN
		FIND #WP_REGLINE.CH%, &
			KEY #0% GE JC_JOB::JOB + WP_REGLINE_READ::LLINE, &
			REGARDLESS
	USE
		CONTINUE 17550 IF ERR = 155%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	NEW_TITLE% = 1%

	!
	! Read line for header
	!
17520	WHEN ERROR IN
		GET #WP_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE 17550 IF ERR = 11%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO 17550 IF WP_REGLINE::JOB <> JC_JOB::JOB OR &
		WP_REGLINE::LLINE <> WP_REGLINE_READ::LLINE

	GOTO 17520 IF WP_REGLINE::REC_TYPE <> "01" AND &
		WP_REGLINE::REC_TYPE <> "02" AND &
		WP_REGLINE::REC_TYPE <> "03"

	!
	! Print detail line title.
	!
	TEXT$ = WP_REGLINE::LLINE + &
		" RType . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .It" + &
		"emCost       Qty StartDate  CompDate   Batch"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%) &
		IF NEW_TITLE%

	NEW_TITLE% = 0%

	!
	! Take only 1's, 2's, and 3's
	!
	RECTYPE$ = "ORD" IF WP_REGLINE::REC_TYPE = "01"
	RECTYPE$ = "COM" IF WP_REGLINE::REC_TYPE = "02"
	RECTYPE$ = "CAN" IF WP_REGLINE::REC_TYPE = "03"

	GOTO CheckNext IF WP_REGLINE::REC_TYPE <> "01"

	TEXT$ = WP_REGLINE::LLINE + " " + &
		RECTYPE$ + &
		SPACE$(63%) + &
		FORMAT$(WP_REGLINE::COST, "######.##") + " " + &
		FORMAT$(WP_REGLINE::QTY, "######.##") + " " + &
		PRNT_DATE(WP_REGLINE::START_DATE, 8%) + " " + &
		PRNT_DATE(WP_REGLINE::COMP_DATE, 8%) + " " + &
		WP_REGLINE::BATCH

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO 17520

 CheckNext:

	GOTO FinishTheDamnLine IF WP_REGLINE::REC_TYPE <> "02"

	TEXT$ = WP_REGLINE::LLINE + " " + &
		RECTYPE$ + "   " + &
		"      " + &
		"            " + &
		"                                          " + &
		FORMAT$(WP_REGLINE::COST, "######.##") + " " + &
		FORMAT$(WP_REGLINE::QTY, "######.##") + " " + &
		"           " + &
		PRNT_DATE(WP_REGLINE::COMP_DATE, 8%) + " " + &
		WP_REGLINE::BATCH

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO 17520

 FinishTheDamnLine:

	TEXT$ = WP_REGLINE::LLINE + " " + &
		RECTYPE$ + "   " + &
		"      " + &
		"            " + &
		"                                         " + &
		"           " + &
		FORMAT$(WP_REGLINE::QTY, "######.##") + " " + &
		"           " + &
		PRNT_DATE(WP_REGLINE::COMP_DATE, 8%) + " " + &
		WP_REGLINE::BATCH

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO 17520

17550	!
	! Try for next Order Register record
	!
	RETURN

 PrintReqDetail:

17610	IF WP_REQREGISTER.CH% = 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.OPN"
		USE
			CONTINUE 17650 IF ERR = 5%
			FILENAME$ = "WP_REQREGISTER"
			CONTINUE HelpError
		END WHEN
	END IF

	WHEN ERROR IN
		FIND #WP_REQREGISTER.CH%, KEY #0% GE JC_JOB::JOB + &
			WP_REGLINE_READ::LLINE + WP_REQREGISTER_READ::REQNUM + &
			WP_REQREGISTER_READ::REQLIN, REGARDLESS
	USE
			CONTINUE 17650 IF ERR = 155%
			FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	NEW_TITLE% = 1%

17620	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, REGARDLESS
	USE
		CONTINUE 17650 IF ERR = 11%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	GOTO 17650 IF WP_REQREGISTER::JOB <> JC_JOB::JOB OR &
		WP_REQREGISTER::LLINE <> WP_REGLINE_READ::LLINE OR &
		WP_REQREGISTER::REQNUM <> WP_REQREGISTER_READ::REQNUM OR &
		WP_REQREGISTER::REQLIN <> WP_REQREGISTER_READ::REQLIN

	GOTO 17620 IF COMP_STRING(EDIT$(WP_REQREGISTER_READ::REQLIN, -1%), &
		WILDREQLINE$) = 0% &
		AND WILDREQLINE$ <> ""

	REQTYPE$ = ""
	REQTYPE$ = "REQ" IF WP_REQREGISTER::RECTYP = "01"
	REQTYPE$ = "ISS" IF WP_REQREGISTER::RECTYP = "02"
	REQTYPE$ = "CAN" IF WP_REQREGISTER::RECTYP = "03"

	GOTO 17620 IF REQTYPE$ = ""

17640	TEXT$ = WP_REQREGISTER::LLINE + "     " + &
		WP_REQREGISTER::REQNUM + " " + &
		WP_REQREGISTER::REQLIN + "   Type" + &
		" . . . . . . . . . . . . . . . . . . . . . . . ." + &
		" . . . . . Amount        Qty Trandate   Period Batch "

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%) IF NEW_TITLE%

	NEW_TITLE% = 0%

	TEXT$ = WP_REQREGISTER::LLINE + "     " + &
		WP_REQREGISTER::REQNUM + " " + &
		WP_REQREGISTER::REQLIN + "   " + &
		REQTYPE$ + "  " + &
		WP_REQREGISTER::PRODUCT + " " + &
		SPACE$(38%) + " " + &
		FORMAT$(WP_REQREGISTER::AMT, "###,###.##") + " " + &
		FORMAT$(WP_REQREGISTER::QTY, "###,###.##") + " " + &
		PRNT_DATE(WP_REQREGISTER::TRANDATE, 8%) + " " + &
		WP_REQREGISTER::PERIOD + " " + &
		WP_REQREGISTER::BATCH

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	REQ_NUMB$ = WP_REQREGISTER_READ::REQNUM

	GOTO 17620

17650	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitFunction

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END FUNCTION
