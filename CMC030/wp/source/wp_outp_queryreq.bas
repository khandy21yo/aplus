1	%TITLE "Requistion Query Report"
	%SBTTL "WP_OUTP_QUERYREQ"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_OUTP_QUERYREQ(JC_JOB_CDD JC_JOB)

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
	!	This program prints the Job Query report.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_OUTP_QUERYREQ/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP WP_OUTP_QUERYREQ
	!	$ DELETE WP_OUTP_QUERYREQ.OBJ;*
	!
	! Author:
	!
	!	07/31/91 - Jeff Beard
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
	!	05/01/97 - Kevin Handy
	!		Use OUTP_INITFORM function.
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Lose definition for FUNC_FILEXISTS, whish isn't used
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP	(UTL_REPORT)	UTL_REPORT_CDD		UTL_REPORT

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD	WP_REGLINE
	DECLARE			WP_REGLINE_CDD	WP_REGLINE_READ

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REGLINE)	WP_REQREGISTER_CDD  WP_REQREGISTER
	DECLARE			WP_REQREGISTER_CDD  WP_REQREGISTER_READ

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"

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

	GOTO Exit1 &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	!
	! Set up from user input
	!
	PRINT.DETAIL$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Print Detail\*
	!	.b
	!	.lm +5
	!	The ^*Print Detail\* selects whether
	!	all register lines are to be printed along with a summary,
	!	or if the summary is to be printed alone.
	!	.lm -5
	!
	! Index:
	!	.x Print Detail
	!
	!--

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "QUERY OF JOB# " + JC_JOB::JOB
	TITLE$(2%) = "Work in Process System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	IF PRINT.DETAIL$ = "Y"
	THEN
		!			 1         2         3         4
		!		1234567890123456789012345678901234567890
		TITLE$(4%) = "JobNumber  JobDescription               " + &
			"            Type Class   JobDate  Status" + &
			" CloseDate  Location Operator   Referenc" + &
			"eNo"
		TITLE$(5%) = "           Line   TType ItemCode       D" + &
			"escription                             Q" + &
			"tyOrdered       QtyComplete  QtyCancel   Balance"
		TITLE$(6%) = "                ReqNum      Reqlin Type" + &
			" Product         Descr                 " + &
			"       Amount        Qty Trandate   Period Batch "
		TITLE$(7%) = "."
	ELSE
		!			 1         2         3         4
		!		1234567890123456789012345678901234567890
		TITLE$(4%) = "JobNumber  JobDescription               " + &
			"            Type Class   JobDate  Status" + &
			" CloseDate  Location Operator   Referenc" + &
			"eNo"
		TITLE$(5%) = "           Line   TType ItemCode       D" + &
			"escription                             Q" + &
			"tyOrdered  QtyComplete  QtyCancel   Balance"
		TITLE$(6%) = "                ReqNum      Reqlin Produc" + &
			"t                                    " + &
			"      QtyOrdered  QtyComplete  QtyCancel" + &
			"    Balance"
		TITLE$(7%) = "."
	END IF

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

 PrintItNow:
	!
	! Build and print the Header crap out now
	!
	TEXT1$ = JC_JOB::JOB + " " + &
		JC_JOB::DESCR + " " + &
		JC_JOB::TTYPE + "   " + &
		JC_JOB::CLASS + "  " + &
		PRNT_DATE(JC_JOB::BDATE, 8%) + " " + &
		JC_JOB::SSTATUS + "      " + &
		PRNT_DATE(JC_JOB::EDATE, 8%) + " " + &
		JC_JOB::LOCATION + "     " + &
		JC_JOB::OPERATOR + " " + &
		JC_JOB::REFNO

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT1$, 0%)

	NEXT_LINE$ = "   "

 ReadRegLine:
	GOTO NewOrder IF WP_READ_REGLINE(JC_JOB::JOB, NEXT_LINE$, "GT", &
		WP_REGLINE_READ, QTY()) <> CMC$_NORMAL

	NEXT_LINE$ = WP_REGLINE_READ::LLINE

	BALANCE = QTY(1%) - QTY(2%) - QTY(3%)

	BALANCE = 0.0 IF BALANCE <= 0.0

	TEXT$ = JC_JOB::JOB + " " + &
		WP_REGLINE_READ::LLINE + "   " + &
		WP_REGLINE_READ::TTYPE + "     " + &
		WP_REGLINE_READ::ITEMCODE + " " + &
		WP_REGLINE_READ::DESCR + " " + &
		FORMAT$(QTY(1%), "######.##") + "    " + &
		FORMAT$(QTY(2%), "######.##") + "  " + &
		FORMAT$(QTY(3%), "######.##") + " " + &
		FORMAT$(BALANCE, "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	REQNUMBER$ = "          "
	REQLINE$ = "    "

	GOTO PrintDetail IF PRINT.DETAIL$ = "Y"

 NextRequistion:

	GOTO ReadRegline &
		IF WP_READ_REQREGISTER(JC_JOB::JOB, WP_REGLINE_READ::LLINE, &
		REQNUMBER$ + REQLINE$, "GT", &
		WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL

	REQNUMBER$ = WP_REQREGISTER_READ::REQNUM
	REQLINE$   = WP_REQREGISTER_READ::REQLIN

	BALANCE = QTY(1%) - QTY(2%) - QTY(3%)

	BALANCE = 0.0 IF BALANCE <= 0.0

	TEXT$ =  WP_REQREGISTER_READ::JOB + " " + &
		WP_REQREGISTER_READ::LLINE + " " + &
		WP_REQREGISTER_READ::REQNUM + "  " + &
		WP_REQREGISTER_READ::REQLIN + "   " + &
		WP_REQREGISTER_READ::PRODUCT + "                  " + &
		"                  " + &
		FORMAT$(QTY(1%), "######.##") + "    " + &
		FORMAT$(QTY(2%), "######.##") + "  " + &
		FORMAT$(QTY(3%), "######.##") + "  " + &
		FORMAT$(BALANCE, "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO NextRequistion


 PrintDetail:

17200	WHEN ERROR IN
		FIND #WP_REQREGISTER.CH%, KEY #0% GT JC_JOB::JOB + &
			WP_REGLINE_READ::LLINE + "              "
	USE
		FILENAME$ = "WP+REQREGISTER"
		CONTINUE HelpError
	END WHEN

17250	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, REGARDLESS
	USE
		FILENAME$ = "WP+REQREGISTER"
		CONTINUE HelpError
	END WHEN

	GOTO ReadRegline IF WP_REQREGISTER::JOB <> JC_JOB::JOB OR &
		WP_REQREGISTER::LLINE <> WP_REGLINE_READ::LLINE

	REQTYPE$ = ""
	REQTYPE$ = "REQ" IF WP_REQREGISTER::RECTYP = "01"
	REQTYPE$ = "ISS" IF WP_REQREGISTER::RECTYP = "02"
	REQTYPE$ = "CAN" IF WP_REQREGISTER::RECTYP = "03"

	TEXT$ =  JC_JOB::JOB + " " + &
		WP_REQREGISTER::LLINE + " " + &
		WP_REQREGISTER::REQNUM + "  " + &
		WP_REQREGISTER::REQLIN + "   " + &
		REQTYPE$ + "  " + &
		WP_REQREGISTER::PRODUCT + &
		"  DESCRIPTION WILL GO HEAR " + &
		FORMAT$(WP_REQREGISTER::AMT, "###,###.##") + " " + &
		FORMAT$(WP_REQREGISTER::QTY, "###,###.##") + " " + &
		PRNT_DATE(WP_REQREGISTER::TRANDATE, 8%) + " " + &
		WP_REQREGISTER::PERIOD + " " + &
		WP_REQREGISTER::BATCH

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO 17250

 NewOrder:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

 ExitFunction:

	CALL OUTP_FINISH(UTL_REPORTX)

 Exit1:
	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$
	SCOPE::PRG_ITEM = TEMP_ITEM$
	SCOPE::PRG_IDENT = TEMP_IDENT$

	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitFunction

	%Page

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END FUNCTION
