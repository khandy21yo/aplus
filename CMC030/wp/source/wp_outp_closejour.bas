1	%TITLE "WIP Closing Journal Report"
	%SBTTL "WP_OUTP_CLOSEJOUR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_OUTP_CLOSEJOUR(STRING JOB)

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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints the WIP Closing Journal
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_OUTP_CLOSEJOUR/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP WP_OUTP_CLOSEJOUR
	!	$ DELETE WP_OUTP_CLOSEJOUR.OBJ;*
	!
	! Author:
	!
	!	07/23/92 - Dan Perkins
	!
	! Modification History:
	!
	!	09/01/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/23/93 - Danster and Frankster
	!		Added 3 decimal places to the REQREGISTER balance
	!		column.
	!
	!	05/27/93 - Dan Perkins
	!		Print issue lines first, then buyoff, then accounts,
	!		then standard, and actual.  Added variances.
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/27/93 - Frank F. Starman
	!		Try to use the same channel for WP_CLOSELINE like for
	!		maintenance.
	!
	!	04/05/94 - Kevin Handy
	!		Format to 80 columns.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/13/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/96 - Kevin Handy
	!		Lose map for 'FORM_GROUP', which was never used.
	!		Lost call to get PS_FORM.DEV$.
	!
	!	05/07/97 - Kevin Handy
	!		Use OUTP_INITFORM function
	!
	!	05/21/98 - Kevin Handy
	!		Increase size of batch number from 2 to 8 characters
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/15/2000 - Kevin Handy
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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSEJOUR.HB"
	MAP (WP_CLOSEJOUR)	WP_CLOSEJOUR_CDD	WP_CLOSEJOUR

	%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSELINE.HB"
	MAP (WP_CLOSELINE)	WP_CLOSELINE_CDD	WP_CLOSELINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	DECLARE			WP_REGLINE_CDD		WP_REGLINE_READ

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	DECLARE			WP_REQREGISTER_CDD	WP_REQREGISTER_READ

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.HB"
	MAP (JC_TYPE)		JC_TYPE_CDD		JC_TYPE

	%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.HB"
	MAP (JC_CLASS)		JC_CLASS_CDD		JC_CLASS

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	DECLARE			GL_CHART_CDD		GL_CHART_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	DECLARE			UTL_LOCATION_CDD	UTL_LOCATION_EXAM

	!
	! Common statements
	!
	COM (CH_WP_CLOSEJOUR)        WP_CLOSEJOUR.CH%
	COM (CH_JC_TYPE_READ)        JC_TYPE.CH%
	COM (CH_JC_CLASS_READ)       JC_CLASS.CH%

	COM (CH_WP_CLOSELINE) &
		WP_CLOSELINE.CH%, &
		WP_CLOSELINE.READONLY%

	COM (BATCH_WP_CLOSEJOUR) &
		BATCH_NO$ = 8%

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION WP_READ_REGLINE
	EXTERNAL LONG FUNCTION WP_READ_REQREGISTER
	EXTERNAL LONG FUNCTION WP_WRIT_VARIANCE
	EXTERNAL LONG FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG FUNCTION UTL_EXAM_LOCATION
	EXTERNAL LONG FUNCTION GL_EXAM_CHART
	EXTERNAL LONG FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************
	REPORT$ = "WP0030"

	!***************************************************************
	! Open Report files
	!***************************************************************

	!
	! store original values for the help message
	!
	TEMP_PROGRAM$ = SCOPE::PRG_PROGRAM
	TEMP_ITEM$ = SCOPE::PRG_ITEM
	TEMP_IDENT$ = SCOPE::PRG_IDENT

	SCOPE::PRG_PROGRAM = "WP_OUTP_CLOSEJOUR"
	SCOPE::PRG_ITEM = "HELP"
	SCOPE::PRG_IDENT = "H"


	!
	! Plug the passed job number into the report file
	!
	SETVALUE$ = "00" + JOB + ",01" + JOB

	!
	! Ask user to change settings
	!
	GOTO ExitFunction &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL


	!
	! Open other necessary files
	!
600	IF WP_CLOSEJOUR.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSEJOUR.CRE"
		USE
			FILENAME$ = "WP_CLOSEJOUR"
			CONTINUE HelpError
		END WHEN
	END IF

610	IF WP_CLOSELINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_CLOSELINE.CRE"
		USE
			CONTINUE 620 IF ERR = 5%
			FILENAME$ = "WP_CLOSELINE"
			CONTINUE HelpError
		END WHEN
	END IF

620	IF SB_SUBACCOUNT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
		USE
			FILENAME$ = "JC_JOB"
			CONTINUE HelpError
		END WHEN
	END IF

650	IF JC_TYPE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.OPN"
		USE
			CONTINUE 660 IF ERR = 5%
			FILENAME$ = "JC_TYPE"
			CONTINUE HelpError
		END WHEN
	END IF

660	IF JC_CLASS.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.OPN"
		USE
			CONTINUE Init IF ERR = 5%
			FILENAME$ = "JC_CLASS"
			CONTINUE HelpError
		END WHEN
	END IF

 Init:	!
	! Set up from user input
	!

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Job\*
	!	.b
	!	.lm +5
	!	The ^*From Job\* field enters the period with which
	!	the report will begin printing.  A blank field causes the report to begin
	!	with the first period in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Job\*
	!	.b
	!	.lm +5
	!	The ^*To Job\* field enters the period with which
	!	the report will end. A blank field causes the report to end with
	!	the last period in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	VARFLAG$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) Calc Var (Y,N)\*
	!	.lm +5
	!	.b
	!	The ^*Calc[ulate] Var[iances] (Y,N)\* field
	!	prints a journal report before and after
	!	variances have been calculated.
	!	.lm -5
	!
	! Index:
	!	.x Variances>Calculate
	!	.x Calculate>Variances
	!
	!--

	BALANCEDFLAG$ = LEFT$(UTL_REPORTX::OPTDEF(3%), 1%)

	!++
	! Abstract:FLD04
	!	^*(04) Show Balanced Entries Only (Y,N)\*
	!	.lm +5
	!	.b
	!	Should entries where the Requested and the Issue Quantity
	!	are equal be printed?
	!	.b
	!	Defaults to No.
	!	.lm -5
	!
	! Index:
	!	.x Balanced>Calculate
	!	.x Calculate>Balanced
	!
	!--

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "CLOSING JOURNAL REPORT"
	TITLE$(2%) = "BATCH No. " + BATCH_NO$
	TITLE$(4%) = "Work In Process System"
	TITLE$(5%) = ""
	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	PAGE% = 0%

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #WP_CLOSEJOUR.CH%, KEY #0%
		ELSE
			FIND #WP_CLOSEJOUR.CH%, &
				KEY #0% GE FROM_ITEM$
		END IF
	USE
		IF ERR = 154%	! Locked record
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE ExitFunction IF ERR = 155%
		FILENAME$ = "WP_CLOSEJOUR"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	WHEN ERROR IN
		GET #WP_CLOSEJOUR.CH%
	USE
		IF ERR = 154%	! Locked record
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE ExitFunction IF ERR = 11%
		FILENAME$ = "WP_CLOSEJOUR"
		CONTINUE HelpError
	END WHEN

	GOTO ExitFunction &
		IF (WP_CLOSEJOUR::JOB > TO_ITEM$) AND TO_ITEM$ <> ""

	!
	! Get other job information
	!
17100	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, &
			KEY #0% EQ "J" + WP_CLOSEJOUR::JOB, &
			REGARDLESS
	USE
		FILENAME$ = "JC_JOB"
		CONTINUE HelpError
	END WHEN

17110	JC_TYPE::DESCR = SPACE$(LEN(JC_TYPE::DESCR))

	WHEN ERROR IN
		GET #JC_TYPE.CH%, &
			KEY #0% EQ JC_JOB::TTYPE, &
			REGARDLESS
	USE
		CONTINUE 17120 IF ERR = 9% OR ERR = 155%
		FILENAME$ = "JC_TYPE"
		CONTINUE HelpError
	END WHEN

17120	JC_CLASS::DESCR = SPACE$(LEN(JC_CLASS::DESCR))

	WHEN ERROR IN
		GET #JC_CLASS.CH%, &
			KEY #0% EQ JC_JOB::CLASS, &
			REGARDLESS
	USE
		CONTINUE GetLocation IF ERR = 9% OR ERR = 155%
		FILENAME$ = "JC_CLASS"
		CONTINUE HelpError
	END WHEN

 GetLocation:
	!
	! Get Location information
	!
	V% = UTL_EXAM_LOCATION(JC_JOB::LOCATION, UTL_LOCATION_EXAM)

	TITLE$(3%) = "Job No. " + WP_CLOSEJOUR::JOB

	TEXT$ = SPACE$(20%) + "Job#:             " + WP_CLOSEJOUR::JOB

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)

	TEXT$ = SPACE$(20%) + "Job Description:  " + JC_JOB::DESCR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(20%) + "Type:             " + JC_JOB::TTYPE + &
		"       " + JC_TYPE::DESCR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(20%) + "Class:            " + JC_JOB::CLASS + &
		"     " + JC_CLASS::DESCR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(20%) + "Location:         " + JC_JOB::LOCATION + &
		"     " + UTL_LOCATION_EXAM::LOCNAME

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(20%) + "Reference Number: " + JC_JOB::REFNO

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(20%) + "Begin Date:       " + &
		PRNT_DATE(JC_JOB::BDATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(20%) + "Close Date:       " + &
		PRNT_DATE(WP_CLOSEJOUR::CLOSEDATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(20%) + "Operator:         " + WP_CLOSEJOUR::OPERATOR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	GOTO ExitFunction &
		IF UTL_REPORTX::STAT

17200	IF VARFLAG$ = "Y"
	THEN
		IF WP_WRIT_VARIANCE(WP_CLOSEJOUR, TEXT$) = CMC$_NORMAL
		THEN
			UPDATE #WP_CLOSEJOUR.CH%
		ELSE
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
				"!!! " + TEXT$, 0%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		END IF
	END IF

	!
	! Get the requisition info from the WP_REQREGISTER
	!
	ISSUETOTAL = 0.0
	LLINE$ = ""
	REQNUMBER$ = "          "
	REQLINE$ = "    "
	TITLE_FLAG% = -1%

 ReadReqRegister:
	GOTO OutaReadReqRegister &
		IF WP_READ_REQREGISTER(WP_CLOSEJOUR::JOB, &
		LLINE$, REQNUMBER$ + REQLINE$, "GT", &
		WP_REQREGISTER_READ, QTY()) <> CMC$_NORMAL

	ISSUETOTAL = ISSUETOTAL + QTY(9%)
	LLINE$ = WP_REQREGISTER_READ::LLINE
	REQNUMBER$ = WP_REQREGISTER_READ::REQNUM
	REQLINE$ = WP_REQREGISTER_READ::REQLIN

	IF TITLE_FLAG%
	THEN
		TEXT$ = "Line     ReqNum      RecLine  Product         " + &
			"Description                     " + &
			" ReqQty   IssQty   CanQty      Amount   BalQty"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 2%)

		TITLE_FLAG% = 0%
	END IF

	!
	! Get product description
	!
	V% = PD_EXAM_PRODUCT(WP_REQREGISTER_READ::PRODUCT, PD_PRODUCT_EXAM)

	!
	! If we are displaying all entries, or the req <> issue, then
	! print the line.
	!
	IF (BALANCEDFLAG$ <> "Y") OR (QTY(1%) <> QTY(2%))
	THEN
		TEXT$ = WP_REQREGISTER_READ::LLINE + "     "  + &
			CONV_STRING(WP_REQREGISTER_READ::REQNUM, CMC$_LEFT) + "  " + &
			WP_REQREGISTER_READ::REQLIN + "     " + &
			WP_REQREGISTER_READ::PRODUCT + "  " + &
			LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 30%) + "  " + &
			FORMAT$(QTY(1%), "###,###") + "  " + &
			FORMAT$(QTY(2%), "###,###") + "  " + &
			FORMAT$(QTY(3%), "###,###") + "  " + &
			FORMAT$(QTY(9%), "<%>##,###.##") + "  " + &
			FORMAT$(QTY(0%), "<%>##,###.###")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 2%)

		GOTO ExitFunction &
			IF UTL_REPORTX::STAT
	END IF

	GOTO ReadReqRegister

 OutaReadReqRegister:
	IF TITLE_FLAG% = 0%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		TEXT$ = "Total Material Issued" + SPACE$(82%) + &
			FORMAT$(ISSUETOTAL, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

	!
	! Get the buyoff from WP_REGLINE
	!
	BUYOFFTOTAL = 0.0
	LLINE$ = "    "
	TITLE_FLAG% = -1%

 ReadRegline:
	GOTO OutaRegline &
		IF WP_READ_REGLINE(WP_CLOSEJOUR::JOB, &
		LLINE$, "GT", WP_REGLINE_READ, QTY()) <> CMC$_NORMAL

	BUYOFFTOTAL = FUNC_ROUND(BUYOFFTOTAL + QTY(9%), 2%)
	LLINE$ = WP_REGLINE_READ::LLINE

	IF TITLE_FLAG%
	THEN
		TEXT$ = "Line  TranType  Itemcode        " + &
			"Description                " + &
			" OrdQty  CompQty   CanQty        Cost   BalQty  " + &
			"ExpectStart  ExpectComp"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 2%)

		TITLE_FLAG% = 0%

	END IF

	SELECT WP_REGLINE_READ::TTYPE

	CASE "L"
		TTYPE$ = "LABOR   "

	CASE "M"
		TTYPE$ = "MATERIAL"

	END SELECT

	TEXT$ = WP_REGLINE_READ::LLINE + "  "  + &
		TTYPE$ + "  "  + &
		WP_REGLINE_READ::ITEMCODE + "  "  + &
		LEFT(WP_REGLINE_READ::DESCR, 25%) + "  "  + &
		FORMAT$(QTY(1%), "###,###") + "  "  + &
		FORMAT$(QTY(2%), "###,###") + "  "  + &
		FORMAT$(QTY(3%), "###,###") + "  "  + &
		FORMAT$(QTY(9%), "###,###.##") + "  "  + &
		FORMAT$(QTY(0%), "<%>##,###") + "  "  + &
		PRNT_DATE(WP_REGLINE_READ::START_DATE, 8%) + "   " + &
		PRNT_DATE(WP_REGLINE_READ::COMP_DATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitFunction &
		IF UTL_REPORTX::STAT

	GOTO ReadRegline

 OutaRegline:
	IF TITLE_FLAG% = 0%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		TEXT$ = "Job Buyoff Total" + SPACE$(68%) + &
			FORMAT$(BUYOFFTOTAL, "#,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

	!
	! Gonna do the CLOSE lines now
	!
17500	TOTAL, VARTOT, BURDTOT, LABETOT, LABRTOT, MATTOT = 0.0

	WHEN ERROR IN
		FIND #WP_CLOSELINE.CH%, KEY #0% EQ WP_CLOSEJOUR::JOB, REGARDLESS
	USE
		CONTINUE ExitCloseLine IF ERR = 9% OR ERR = 155%
		FILENAME$ = "WP_CLOSELINE"
		CONTINUE HelpError
	END WHEN

	TEXT$ = "Variances"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 5%)

 ReadCloseLine:
17520	WHEN ERROR IN
		GET #WP_CLOSELINE.CH%, REGARDLESS
	USE
		CONTINUE ExitCloseLine IF ERR = 11%
		FILENAME$ = "WP_CLOSELINE"
		CONTINUE HelpError
	END WHEN

	GOTO ExitCloseLine &
		IF WP_CLOSELINE::JOB <> WP_CLOSEJOUR::JOB

	TOTAL = TOTAL + WP_CLOSELINE::VAMOUNT

	IF WP_CLOSELINE::LFLAG = "V"
	THEN
		SELECT WP_CLOSELINE::VCLASS

		CASE "BURD"
			BURDTOT = BURDTOT + WP_CLOSELINE::VAMOUNT

		CASE "LABE"
			LABETOT = LABETOT + WP_CLOSELINE::VAMOUNT

		CASE "LABR"
			LABRTOT = LABRTOT + WP_CLOSELINE::VAMOUNT

		CASE "MAT"
			MATTOT = MATTOT + WP_CLOSELINE::VAMOUNT

		END SELECT
	END IF

	!
	! Get GL_CHART description
	!
	V% = GL_EXAM_CHART(WP_CLOSELINE::VACCT, GL_CHART_EXAM)

	TEXT$ = WP_CLOSELINE::VCLASS + "  " + &
		WP_CLOSELINE::VACCT + "  " + &
		GL_CHART_EXAM::DESCR + "  " + &
		FORMAT$(WP_CLOSELINE::VAMOUNT, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitFunction &
		IF UTL_REPORTX::STAT

	GOTO ReadCloseLine

 ExitCloseLine:
	TEXT$ = "Total" + SPACE$(LEN(WP_CLOSELINE::VCLASS + "  " + &
		WP_CLOSELINE::VACCT + "  " + &
		GL_CHART_EXAM::DESCR + "  ") - 5%) + &
		FORMAT$(TOTAL, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	GOTO ExitFunction &
		IF UTL_REPORTX::STAT

	!
	! Print Out the Statistics
	!
	TEXT$ = "Cost                           Burden               " + &
		"Labor               Parts              RawMat             " + &
		"  Total"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	STD_TOTAL = WP_CLOSEJOUR::STDBURDEN + WP_CLOSEJOUR::STDLABOR + &
		WP_CLOSEJOUR::STDPARTS + WP_CLOSEJOUR::STDRAWMAT

	TEXT$ = "Standard         " + SPACE$(10%) + &
		FORMAT$(WP_CLOSEJOUR::STDBURDEN, "###,###.##") + SPACE$(10%) + &
		FORMAT$(WP_CLOSEJOUR::STDLABOR, "###,###.##") + SPACE$(10%) + &
		FORMAT$(WP_CLOSEJOUR::STDPARTS, "###,###.##") + SPACE$(10%) + &
		FORMAT$(WP_CLOSEJOUR::STDRAWMAT, "###,###.##") + SPACE$(10%) + &
		FORMAT$(STD_TOTAL, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	ACT_TOTAL = WP_CLOSEJOUR::ACTBURDEN + WP_CLOSEJOUR::ACTLABOR + &
		WP_CLOSEJOUR::ACTPARTS + WP_CLOSEJOUR::ACTRAWMAT

	TEXT$ = "Actual           " + SPACE$(10%) + &
		FORMAT$(WP_CLOSEJOUR::ACTBURDEN, "###,###.##") + SPACE$(10%) + &
		FORMAT$(WP_CLOSEJOUR::ACTLABOR, "###,###.##") + SPACE$(10%) + &
		FORMAT$(WP_CLOSEJOUR::ACTPARTS, "###,###.##") + SPACE$(10%) + &
		FORMAT$(WP_CLOSEJOUR::ACTRAWMAT, "###,###.##") + SPACE$(10%) + &
		FORMAT$(ACT_TOTAL, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	VARTOT = BURDTOT + LABETOT + LABRTOT + MATTOT

	TEXT$ = "Variance             " + &
		"Burd  " + FORMAT$(BURDTOT, "###,###.##") + "  " + &
		"LabEff  " + FORMAT$(LABETOT, "###,###.##") + SPACE$(20%) + &
		"Material  " + FORMAT$(MATTOT, "###,###.##") + "   " + &
		"Total  " + FORMAT$(VARTOT, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(39%) + "LabRat  " + FORMAT$(LABRTOT, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	GOTO ExitFunction &
		IF UTL_REPORTX::STAT

	PAGE% = 999%

	GOTO GetNextRec

 ExitFunction:
	CALL OUTP_FINISH(UTL_REPORTX)

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

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END FUNCTION
