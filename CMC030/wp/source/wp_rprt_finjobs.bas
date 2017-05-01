1	%TITLE "Print Finished Jobs"
	%SBTTL "WP_RPRT_FINJOBS"
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
	! ID:WP0080
	!
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	The ^*Print Finished Jobs\* option prints a report
	!	relative to finished jobs.  The following fields are included in a detailed
	!	report:
	!	.TABLE 3,25
	!	.TE
	!	Job Number	Job Type
	!	.Te
	!	Line Number	Job Class
	!	.Te
	!	Status	Line Type
	!	.Te
	!	Begin Date	Item Code
	!	.TE
	!	End Date	Description
	!	.TE
	!	Location	Operator
	!	.TE
	!	Reference Number	Quantity Ordered
	!	.TE
	!	Quantity Complete	Quantity Cancelled
	!	.eND TABLE
	!	A summary report may be printed in which all line item detail is eliminated.
	!	.LM -5
	!
	! Index:
	!	.x Print>Finished Jobs
	!	.x Finished Jobs>Report
	!	.x Report>Finished Jobs
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_FINJOBS/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_FINJOBS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_FINJOBS.OBJ;*
	!
	! Author:
	!
	!	08/07/91 - Craig Tanner
	!
	! Modification History:
	!
	!	09/26/91 - Dan Perkins
	!		Modified program structure.
	!		Checked error trapping.
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/18/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	DECLARE			WP_REGLINE_CDD		WP_REGLINE_READ

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	DECLARE			WP_REQREGISTER_CDD	WP_REQREGISTER_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION WP_READ_REGLINE
	EXTERNAL LONG	FUNCTION WP_READ_REQREGISTER
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Set up from user input
	!
	PRINT.JDETAIL$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Job Detail\*
	!	.b
	!	.lm +5
	!	The ^*Job Detail\* field
	!	prints a register which includes or excludes line item information.
	!	.B
	!	If this field contains a ^*Y\*, line item detail will be printed. If this
	!	field contains an ^*N\*, line items will not be printed.
	!	.lm -5
	!
	! Index:
	!	.x Detail>Job>Finished Goods
	!	.x Finished Goods>Job>Detail
	!	.x Summary>Job>Finished Goods
	!	.x Summary>Finished Goods
	!
	!--

	!
	! Open Job file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "FINISHED JOBS REPORT"
	TITLE$(2%) = "Work in Process System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Job        Type Class Stat BDate      EDate      " + &
		"Location Operator   RefNo"

	IF PRINT.JDETAIL$ <> "Y"
	THEN
		TITLE$(5%) = "."
	ELSE
		TITLE$(5%) = "           Line          LT ItemCode       " + &
			"Description" + SPACE$(30%) + "QtyOrdered QtyComplete" + &
			" QtyCancel"

		TITLE$(6%) = "."
	END IF

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		FIND #SB_SUBACCOUNT.CH%, KEY #0% GE "J", REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

 GetRec:
17020	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	GOTO ExitProgram IF SB_SUBACCOUNT::SUBJECT <> "J"

	NEXT_LINE$ = "    "
	REQNUMBER$ = "          "
	REQLINE$   = "    "

	!
	! Test Lines to see if balance is zero
	!
 ReadRegLine:
	GOTO PrintHeader IF WP_READ_REGLINE(JC_JOB::JOB, NEXT_LINE$, &
		"GT", WP_REGLINE_READ, QTY()) <> CMC$_NORMAL

	GOTO GetRec IF QTY(0%) <> 0.0

	NEXT_LINE$ = WP_REGLINE_READ::LLINE

 ReadReqRegister:
	GOTO ReadRegLine IF WP_READ_REQREGISTER (JC_JOB::JOB, &
		WP_REGLINE_READ::LLINE, REQNUMBER$ + REQLINE$, &
		"GT", WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL

	GOTO GetRec IF QTY(0%) <> 0.0

	REQNUMBER$ = WP_REQREGISTER_READ::REQNUM
	REQLINE$   = WP_REQREGISTER_READ::REQLIN

	GOTO ReadReqRegister

 PrintHeader:
	BLANKL% = 0%
	!
	! Build and print the Header out now
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%) IF BLANK_LINE%

	BLANK_LINE% = 1%

	TEXT$ = JC_JOB::JOB + " " + &
		JC_JOB::TTYPE + "   " + &
		JC_JOB::CLASS + "  " + &
		JC_JOB::SSTATUS + "    " + &
		PRNT_DATE(JC_JOB::BDATE, 8%) + " " + &
		PRNT_DATE(JC_JOB::EDATE, 8%) + " " + &
		JC_JOB::LOCATION + "     " + &
		JC_JOB::OPERATOR + " " + &
		JC_JOB::REFNO

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO GetRec IF PRINT.JDETAIL$ <> "Y"

	NEXT_LINE$ = "    "

  PrintRegLine:
	GOTO GetRec IF WP_READ_REGLINE(JC_JOB::JOB, NEXT_LINE$, &
		"GT", WP_REGLINE_READ, QTY()) <> CMC$_NORMAL

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), JC_JOB::JOB, -1%) &
		IF BLANKL% = 0%

	BLANKL% = 1%

	NEXT_LINE$ = WP_REGLINE_READ::LLINE

	TEXT$ = JC_JOB::JOB + " " + &
		WP_REGLINE_READ::LLINE + "          " + &
		WP_REGLINE_READ::TTYPE + "  " + &
		WP_REGLINE_READ::ITEMCODE + " " + &
		WP_REGLINE_READ::DESCR + "  " + &
		FORMAT$(QTY(1%), "######.##") + "   " + &
		FORMAT$(QTY(2%), "######.##") + " " + &
		FORMAT$(QTY(3%), "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	BLANKL% = 0%

	!
	! Set up a number of variables to be used later on.
	!
	REQNUMBER$ = "          "	! Necessary for using WP_READ_REQREGISTER
	REQLINE$ = "    "
	PRINT_TITLE% = 1%

 PrintReqRegister:
	GOTO PrintRegLine IF WP_READ_REQREGISTER (JC_JOB::JOB, &
		WP_REGLINE_READ::LLINE, REQNUMBER$ + REQLINE$, "GT", &
		WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL

	REQNUMBER$ = WP_REQREGISTER_READ::REQNUM
	REQLINE$   = WP_REQREGISTER_READ::REQLIN

	!
	! Check if time to print blank line between requisition line blocks,
	! else time for line between requisition number blocks.
	!
	IF REQ_NUMB$ = WP_REQREGISTER_READ::REQNUM
	THEN
		TEXT$ = JC_JOB::JOB + " " + &
			WP_REQREGISTER_READ::LLINE + "     " + &
			WP_REQREGISTER_READ::REQNUM
	ELSE
		TEXT$ = JC_JOB::JOB + " " + &
			WP_REQREGISTER_READ::LLINE
	END IF

	IF NOT PRINT_TITLE%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! Try and print title for entire set of requisitions.
	!
	TEXT$ = JC_JOB::JOB + " " + &
		WP_REQREGISTER_READ::LLINE + &
		"     ReqNum     ReqLin      Product        " + &
		"Description                             " + &
		"QtyOrdered  QtyComplete  QtyCancel"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%) IF PRINT_TITLE%

	PRINT_TITLE% = 0%

	!
	! Get product description
	!
	V% = PD_EXAM_PRODUCT(WP_REQREGISTER_READ::PRODUCT, PD_PRODUCT_EXAM)

	TEXT$ = JC_JOB::JOB + " " + &
		WP_REQREGISTER_READ::LLINE + "     " + &
		WP_REQREGISTER_READ::REQNUM + " " + &
		WP_REQREGISTER_READ::REQLIN + "        " + &
		WP_REQREGISTER_READ::PRODUCT + " " + &
		PD_PRODUCT_EXAM::DESCRIPTION + " " + &
		FORMAT$(QTY(1%), "######.##") + "    " + &
		FORMAT$(QTY(2%), "######.##") + "  " + &
		FORMAT$(QTY(3%), "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO PrintReqRegister

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
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
