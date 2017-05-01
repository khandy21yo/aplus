1	%TITLE "Requistion Detail"
	%SBTTL "WP_RPRT_REQDETAIL"
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
	! ID:WP0070
	!
	! Abstract:HELP
	!	.lm +5
	!	.b
	!	The ^*Requisition Detail\* report contains the following information:
	!	.table 3,25
	!	.te
	!	Job Number	Job Description
	!	.te
	!	Type	Class
	!	.te
	!	Order Date	Location
	!	.te
	!	Operator	Reference No
	!	.te
	!	Status Flag	Closed Date
	!	.te
	!	Line	Transaction Type
	!	.te
	!	Item code	Description
	!	.te
	!	Quantity Ordered	Quantity Completed
	!	.te
	!	Quantity Cancelled	Balance
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Report>Requisition Detail
	!	.x Requisition Detail>Report
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_REQDETAIL/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_REQDETAIL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_REQDETAIL.OBJ;*
	!
	! Author:
	!
	!	07/31/91 - JEFF BEARD
	!
	! Modification History:
	!
	!	10/02/91 - Dan Perkins
	!		Cleaned program code.
	!		Checked error trapping.
	!
	!	09/22/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/29/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in
	!		several places.
	!
	!	05/19/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/29/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include codes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	DECLARE			WP_REGLINE_CDD		WP_REGLINE_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_READ

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	!
	! Declare external functions
	!
	EXTERNAL LONG    FUNCTION WP_READ_REGLINE
	EXTERNAL LONG    FUNCTION PD_EXAM_PRODUCT

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
	!	^*(01) Sort by (J,T,C)\*
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
	!	T - Job Type
	!	.le
	!	C - Class
	!	.els
	!	.lm -5
	!	.b
	!	A setting is required in this field. No other settings are valid.
	!
	! Index:
	!	.x Sort>Job Status Summary Report
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02)From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field enters the item from which be report
	!	is to begin.  The "item" must be in agreement with the setting in field (01)
	!	Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Job Status Summary Report
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field specifies the item to which the
	!	report will print.  The "item" must be in agreement with the setting in
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Job Status Summary Report
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items to be
	!	printed by entering a wildcard through the Wildcarding Techniques.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Job Status Summary Report
	!
	!--

300	!
	! Open  Subaccount Register file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	!
	! Open WP_REQREGISTER file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.OPN"
	USE
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	! Select which title heading to print
	!
	SELECT SORTBY$

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = " REGISTER DETAIL BY JOB TYPE"

	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = " REGISTER DETAIL BY JOB CLASS"

	CASE "J"
		K_NUM% = 0%
		TITLE$(1%) = " REGISTER DETAIL BY JOB NUMBER"

	END SELECT

	TITLE$(2%) = "Work In Process System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "JobNumber  JobDescription               " + &
		"            Type Class   JobDate  Status" + &
		" CloseDate  Location Operator   Referenc" + &
		"eNo"

	TITLE$(5%) = "              Line   TType ItemCode      D" + &
		"escription                             Q" + &
		"tyOrdered       QtyComplete  QtyCancel   Balance"

	TITLE$(6%) = "                ReqNum      Reqlin Type" + &
		" Product         Descr                 " + &
		"       Amount        Qty Trandate   Period Batch "

	TITLE$(7%) =  "."

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
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #K_NUM% GE "J", &
				REGARDLESS
		ELSE
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #K_NUM% GE "J" + FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	GOTO ExitProgram IF UTL_REPORTX::STAT

	PRINT_HEADER_FLAG% = 0%

	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	GOTO ExitProgram IF JC_JOB::SUBJECT <> "J"

	!
	! Check current record if should be printed
	!
	SELECT SORTBY$

	CASE "C"
		GOTO ExitProgram IF (JC_JOB::CLASS > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY &
			(EDIT$(JC_JOB::CLASS, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "J"
		GOTO ExitProgram IF (JC_JOB::JOB > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY &
			(EDIT$(JC_JOB::JOB, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitProgram IF (JC_JOB::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY &
			(EDIT$(JC_JOB::TTYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

	NEXT_LINE$ = "   "

 ReadRegLine:
	IF WP_READ_REGLINE(JC_JOB::JOB, NEXT_LINE$, "GT", &
		WP_REGLINE_READ, QTY()) <> CMC$_NORMAL
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) &
			IF PRINT_HEADER_FLAG%

		GOTO GetNextRec
	END IF

	NEXT_LINE$ = WP_REGLINE_READ::LLINE
	PRINT_REGLINE_FLAG% = 0%

 NextRequistion:
17200	WHEN ERROR IN
		FIND #WP_REQREGISTER.CH%, &
			KEY #0% GT JC_JOB::JOB + &
			WP_REGLINE_READ::LLINE + "              ", &
			REGARDLESS
	USE
		CONTINUE ReadRegline IF ERR = 155% OR ERR = 11%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	!
	! LAST_PRODUCT$ is used so that the description is not printed twice
	!
	LAST_PRODUCT$ = ""

 GetRequisition:
	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, REGARDLESS
	USE
		CONTINUE ReadRegline IF ERR = 155% OR ERR = 11%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	GOTO ReadRegline &
		IF WP_REQREGISTER::JOB <> JC_JOB::JOB OR &
		WP_REQREGISTER::LLINE <> WP_REGLINE_READ::LLINE

	IF PRINT_HEADER_FLAG% = 0%
	THEN
		!
		! Build and print Header
		!
		TEXT$ = JC_JOB::JOB + " " + &
			JC_JOB::DESCR + " " + &
			JC_JOB::TTYPE + "   " + &
			JC_JOB::CLASS + "  " + &
			PRNT_DATE(JC_JOB::BDATE, 8%) + " " + &
			JC_JOB::SSTATUS	+ "      " + &
			PRNT_DATE(JC_JOB::EDATE, 8%) + " " + &
			JC_JOB::LOCATION + "     " + &
			JC_JOB::OPERATOR + " " + &
			JC_JOB::REFNO

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		PRINT_HEADER_FLAG% = 1%
	END IF

	IF PRINT_REGLINE_FLAG% = 0%
	THEN
		!
		! Build and print Regline
		!
		TEXT$ = JC_JOB::JOB + "    " + &
			WP_REGLINE_READ::LLINE + "   " + &
			WP_REGLINE_READ::TTYPE + "     " + &
			WP_REGLINE_READ::ITEMCODE + &
			WP_REGLINE_READ::DESCR + " " + &
			FORMAT$(QTY(1%), "######.##") + "         " + &
			FORMAT$(QTY(2%), "######.##") + "  " + &
			FORMAT$(QTY(3%), "######.##") + " " + &
			FORMAT$(QTY(0%), "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		PRINT_REGLINE_FLAG% = 1%
	END IF

	SELECT WP_REQREGISTER::RECTYP

	CASE "01"
		REQTYPE$ = "REQ"

	CASE "02"
		REQTYPE$ = "ISS"

	CASE "03"
		REQTYPE$ = "CAN"

	CASE ELSE	! Don't print temporary records
		GOTO GetRequisition

	END SELECT

	IF WP_REQREGISTER::PRODUCT = LAST_PRODUCT$
	THEN
		PD_PRODUCT_READ::DESCRIPTION = ""
	ELSE
		V% = PD_EXAM_PRODUCT( WP_REQREGISTER::PRODUCT, PD_PRODUCT_READ)
		LAST_PRODUCT$ = WP_REQREGISTER::PRODUCT
	END IF

	TEXT$ = JC_JOB::JOB + "      " + &
		CONV_STRING(WP_REQREGISTER::REQNUM, CMC$_LEFT) + "  " + &
		WP_REQREGISTER::REQLIN + "   " + &
		REQTYPE$ + "  " + &
		WP_REQREGISTER::PRODUCT + " " + &
		LEFT(PD_PRODUCT_READ::DESCRIPTION, 26%) + " " + &
		FORMAT$(WP_REQREGISTER::AMT, "###,###.##") + " " + &
		FORMAT$(WP_REQREGISTER::QTY, "###,###.##") + " " + &
		PRNT_DATE(WP_REQREGISTER::TRANDATE, 8%) + " " + &
		WP_REQREGISTER::PERIOD + " " + &
		WP_REQREGISTER::BATCH

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO GetRequisition

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
	! Abstract:FLD05
	!	^*(05) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field selects the batch number which
	!	will be printed.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Job Status Summary Report
	!
	!--
