1	%TITLE "Product Yield Report"
	%SBTTL "WP_RPRT_ANDREA"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1994 BY
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
	! ID:WP0023
	!
	! Abstract:HELP
	!	.lm +5
	!	.b
	!	The ^*Product Yield Report\*
	!
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_RPRT_ANDREA/LINE
	!	$ LINK/EXE=WP_EXE: WP_RPRT_ANDREA, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_RPRT_ANDREA.OBJ;*
	!
	! Author:
	!
	!	09/16/94 - Kevin Handy
	!
	! Modification History:
	!
	!	11/04/94 - Kevin Handy
	!		Rearranged output to display by job instead of
	!		product. Fixed Totals. Added wildcard for the
	!		category.
	!
	!	01/18/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/18/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/25/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add several REGARDLESS clauses
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include codes.inc
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	DECLARE			JC_JOB_CDD		SB_SUBACCOUNT_EXAM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD	BM_RELATION

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER

	DIM RFA RFA_LEVEL(500%)
	DIM REAL QTY_LEVEL(500%)
	DIM STRING TEST_PRODUCT(500%)

	!
	! Local map's
	!
	RECORD REQSUM_CDD
		STRING	JOB = 10
		STRING	LLINE = 4
		STRING	PRODUCT = 14
		REAL	QTY
	END RECORD

	MAP (REQSUM) REQSUM_CDD REQSUM

	RECORD REGSUM_CDD
		STRING	FINPRODUCT = 14
		STRING	RLINE = 4
		STRING	PRODUCT = 14
		STRING	JOB = 10
		STRING	LLINE = 4
		STRING	COMPDATE = 8
		REAL	QTY
	END RECORD

	MAP (REGSUM) REGSUM_CDD REGSUM

	RECORD SUMMARY_BY_JOB_CDD
		STRING	JOB = 10
		REAL	QTY
	END RECORD

	DIM SUMMARY_BY_JOB_CDD SUMMARY_BY_JOB(100%)

	RECORD PRODUCT_SUMMARY_CDD
		STRING	PRODUCT = 14%
		STRING	DESCRIPTION = 40%
		REAL	QTY
	END RECORD

	DIM PRODUCT_SUMMARY_CDD PRODUCT_SUMMARY(200%)
	DECLARE PRODUCT_SUMMARY_CDD PRODUCT_SUMMARY_TEMP

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(5%)), -1%)

	!++
	! Abstract:FLD06
	!	.ts 55
	!	^*(06) From Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*From Date\* field enters the date to
	!	begin printing.
	!	.b
	!	A blank field will cause the report to begin with the earliest
	!	dated item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	CATWILD$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 128%)

	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) Category Wildcard
	!
	! Index:
	!
	!--

	TO_DATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(6%)), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) To Date\*
	!	.b
	!	.lm +5
	!	The ^*To Date\* field enters the date with which to
	!	end printing.
	!	.b
	!	A blank field will cause the report to end with the most recent
	!	date in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	!
	! Open Location File
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	!
	! Open Product File
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Open REGLINE file
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.OPN"
	USE
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Open BM RELATION file
	!
330	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"
	USE
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

340	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.OPN"
	USE
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "PRODUCT YIELD REPORT"
	TITLE$(2%) = "Work In Process System"

	TITLE$(3%) = "From " + PRNT_DATE(FROM_DATE$, 8%) + " To " + &
		PRNT_DATE(TO_DATE$, 8%)

	TITLE$(4%) = "Before " + PRNT_DATE(TO_DATE$, 8%) IF FROM_DATE$ = ""
	TITLE$(4%) = "After " + PRNT_DATE(FROM_DATE$, 8%) IF TO_DATE$ = ""
	TITLE$(4%) = "For All Dates" IF FROM_DATE$ + TO_DATE$ = ""

	TITLE$(5%) = ""

	!
	! Heading
	!
	TITLE$(6%) = "Product        Description                  "  + &
		"                                       " + &
		"FinQty        RawQty  ActYield  StdYield"

	TITLE$(7%) = "."

	!
	! Set some date ranges if FROM_DATE$
	! and TO_DATE$ are blank
	!
	FROM_DATE$ = "00010101" IF FROM_DATE$ = ""
	TO_DATE$   = "99993112" IF TO_DATE$   = ""

	%PAGE

600	!*******************************************************************
	! Create a version of the REQREGISTER sorted how we want it
	!*******************************************************************

	CALL ASSG_CHANNEL(REQSUM.CH%, STAT%)

	OPEN "Temp_REQSUM.TMP" FOR OUTPUT AS FILE REQSUM.CH%, &
		ORGANIZATION INDEXED FIXED, &
		TEMPORARY, &
		BUFFER 32%, &
		MAP REQSUM, &
		PRIMARY KEY (REQSUM::PRODUCT, REQSUM::LLINE, REQSUM::JOB) DUPLICATES, &
		ALTERNATE KEY (REQSUM::JOB, REQSUM::LLINE) DUPLICATES, &
		ACCESS MODIFY

610	RESET #WP_REQREGISTER.CH%

620	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, REGARDLESS
	USE
		CONTINUE 700 IF ERR = 11%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	GOTO 620 UNLESS WP_REQREGISTER::RECTYP = "02"

	GOTO 620 UNLESS WP_REQREGISTER::TRANDATE >= FROM_DATE$ AND &
		WP_REQREGISTER::TRANDATE <= TO_DATE$

	GOTO 620 IF WP_REQREGISTER::REQLIN > "0003"

625	IF PD_PRODUCT::PRODUCT_NUM <> WP_REQREGISTER::PRODUCT
	THEN
		WHEN ERROR IN
			GET #PD_PRODUCT.CH%, &
				KEY #0% EQ WP_REQREGISTER::PRODUCT, &
				REGARDLESS
		USE
			CONTINUE 630
		END WHEN
	END IF

	IF CATWILD$ <> ""
	THEN
		GOTO 620 IF COMP_STRING(TRM$(PD_PRODUCT::CATEGORY), CATWILD$) = 0%
	END IF

630	WHEN ERROR IN
		GET #REQSUM.CH%, &
			KEY #0% EQ WP_REQREGISTER::PRODUCT + &
				WP_REQREGISTER::REQLIN + &
				WP_REQREGISTER::JOB, &
			REGARDLESS
	USE
		CONTINUE 650 IF ERR = 155%
		FILENAME$ = "REQSUM"
		CONTINUE HelpError
	END WHEN

	REQSUM::QTY = REQSUM::QTY + WP_REQREGISTER::QTY

640	UPDATE #REQSUM.CH%

	GOTO 620

650	REQSUM::JOB	= WP_REQREGISTER::JOB
	REQSUM::LLINE	= WP_REQREGISTER::REQLIN
	REQSUM::PRODUCT	= WP_REQREGISTER::PRODUCT
	REQSUM::QTY	= WP_REQREGISTER::QTY

	PUT #REQSUM.CH%

	GOTO 620


700	!*******************************************************************
	! Create a version of the REGLINE sorted how we want it
	!*******************************************************************

	CALL ASSG_CHANNEL(REGSUM.CH%, STAT%)

	OPEN "Temp_REGSUM.TMP" FOR OUTPUT AS FILE REGSUM.CH%, &
		ORGANIZATION INDEXED FIXED, &
		TEMPORARY, &
		BUFFER 32%, &
		MAP REGSUM, &
		PRIMARY KEY (REGSUM::FINPRODUCT, &
			REGSUM::RLINE, &
			REGSUM::JOB, &
			REGSUM::PRODUCT) DUPLICATES, &
		ACCESS MODIFY

710	FIND #WP_REGLINE.CH%, KEY #1% GE "M"

720	WHEN ERROR IN
		GET #WP_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE 800 IF ERR = 11%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	GOTO 800 IF WP_REGLINE::TTYPE <> "M"

	GOTO 720 UNLESS WP_REGLINE::REC_TYPE = "02"

	GOTO 720 UNLESS WP_REGLINE::COMP_DATE >= FROM_DATE$ AND &
		WP_REGLINE::COMP_DATE <= TO_DATE$

730	WHEN ERROR IN
		GET #REQSUM.CH%, &
			KEY #1% EQ WP_REGLINE::JOB + "0001"
	USE
		CONTINUE 740 IF ERR = 155%
		FILENAME$ = "REQSUM"
		CONTINUE HelpError
	END WHEN

	GOSUB AddRegSum

740	WHEN ERROR IN
		GET #REQSUM.CH%, &
			KEY #1% EQ WP_REGLINE::JOB + "0002"
	USE
		CONTINUE 750 IF ERR = 155%
		FILENAME$ = "REQSUM"
		CONTINUE HelpError
	END WHEN

	GOSUB AddRegSum

750	WHEN ERROR IN
		GET #REQSUM.CH%, &
			KEY #1% EQ WP_REGLINE::JOB + "0003"
	USE
		CONTINUE 760 IF ERR = 155%
		FILENAME$ = "REQSUM"
		CONTINUE HelpError
	END WHEN

	GOSUB AddRegSum

760	GOTO 720

 AddRegSum:
780	REGSUM::FINPRODUCT = REQSUM::PRODUCT
	REGSUM::RLINE	= REQSUM::LLINE
	REGSUM::PRODUCT	= WP_REGLINE::ITEMCODE
	REGSUM::JOB	= WP_REGLINE::JOB
	REGSUM::LLINE	= WP_REGLINE::LLINE
	REGSUM::COMPDATE = WP_REGLINE::COMP_DATE
	REGSUM::QTY	= WP_REGLINE::QTY

	PUT #REGSUM.CH%

	RETURN

800	!

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		RESET #REQSUM.CH%, KEY #0%
		GET #REQSUM.CH%
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	EOF_FLAG% = 0%
	PRODUCT_SUMMARY% = 0%

 NewReqSum:
	!
	! Pull in new product to handle
	!
	THIS_PRODUCT$ = REQSUM::PRODUCT
	THIS_LINE$ = REQSUM::LLINE
	THIS_QTY = REQSUM::QTY

	SUMMARY_BY_JOB% = 1%
	SUMMARY_BY_JOB(SUMMARY_BY_JOB%)::JOB = REQSUM::JOB
	SUMMARY_BY_JOB(SUMMARY_BY_JOB%)::QTY = REQSUM::QTY

17010	WHEN ERROR IN
		GET #REQSUM.CH%
	USE
		IF ERR = 11%
		THEN
			EOF_FLAG% = -1%
			CONTINUE 17020
		END IF
		FILENAME$ = "Temp_REQSUM"
		CONTINUE HelpError
	END WHEN

17015	GOTO 17020 IF REQSUM::PRODUCT <> THIS_PRODUCT$ OR &
		REQSUM::LLINE <> THIS_LINE$

	SUMMARY_BY_JOB% = SUMMARY_BY_JOB% + 1%
	SUMMARY_BY_JOB(SUMMARY_BY_JOB%)::JOB = REQSUM::JOB
	SUMMARY_BY_JOB(SUMMARY_BY_JOB%)::QTY = REQSUM::QTY

	!
	! Summarize for totals
	!
	THIS_QTY = THIS_QTY + REQSUM::QTY
	GOTO 17010

17020	!
	! Output Header line
	!
	TOTAL_QTY = 0.0
	TOTAL_QTY = TOTAL_QTY + &
		SUMMARY_BY_JOB(I%)::QTY &
		FOR I% = 1% TO SUMMARY_BY_JOB%

	GOSUB PrintHeaderItem
	GOSUB DoRegSum
	GOSUB TotalSum

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 4%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO NewReqSum IF EOF_FLAG% = 0%

	GOTO ExitTotal

 DoRegSum:
	!*******************************************************************
	! Print lines out
	!*******************************************************************

17100	QTY = 0.0

	WHEN ERROR IN
		FIND #REGSUM.CH%, KEY #0% EQ THIS_PRODUCT$ + THIS_LINE$
	USE
		CONTINUE 17190
	END WHEN

	ONE_REG_PROD$ = ""
	ONE_REG_QTY = 0.0
	ONE_REG_RQTY = 0.0
	ONE_REG_COUNT% = 0%
	ALL_REG_QTY = 0.0

17110	WHEN ERROR IN
		GET #REGSUM.CH%, REGARDLESS
	USE
		CONTINUE 17190 IF ERR=11%
		FILENAME$ = "TEMP_REGSUM"
		CONTINUE HelpError
	END WHEN

	GOTO 17190 IF REGSUM::FINPRODUCT <> THIS_PRODUCT$ OR &
		REGSUM::RLINE <> THIS_LINE$

	GOSUB DoRegTotal IF ONE_REG_JOB$ <> REGSUM::JOB

17130	PD_PRODUCT::DESCRIPTION = ""

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ REGSUM::PRODUCT, &
			REGARDLESS
	USE
		CONTINUE 17180
	END WHEN

17140	QTY = SUMMARY_BY_JOB(I%)::QTY &
		IF SUMMARY_BY_JOB(I%)::JOB = REGSUM::JOB &
		FOR I% = 1% TO SUMMARY_BY_JOB%

	ONE_REG_RQTY = QTY

17180	IF QTY = 0.0
	THEN
		PCT = 0%
	ELSE
		PCT = REGSUM::QTY / QTY * 100.0
	END IF

	!
	! Calculate standard yield
	!
	GOSUB ReadBM
	REQQTY = 1.0 IF REQQTY = 0.0
	STDYIELD = FUNC_ROUND(1.0 / REQQTY * 100.0, 2%)

	TEXT$ = "   " + &
		REGSUM::PRODUCT + " " + &
		LEFT(PD_PRODUCT::DESCRIPTION, 30%) + " " + &
		REGSUM::JOB + " " + &
		REGSUM::LLINE + " " + &
		PRNT_DATE(REGSUM::COMPDATE, 8%) + " " + &
		FORMAT$(REGSUM::QTY, "###,###,###.##") + &
		FORMAT$(QTY, "###,###,###.##") + &
		FORMAT$(PCT, "######.##%") + &
		FORMAT$(STDYIELD, "######.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	ONE_REG_QTY = ONE_REG_QTY + REGSUM::QTY
	ALL_REG_QTY = ALL_REG_QTY + REGSUM::QTY
	ONE_REG_COUNT% = ONE_REG_COUNT% + 1%

	MOD_LINE% = 0%
	MOD_LINE% = I% &
		IF PRODUCT_SUMMARY(I%)::PRODUCT = REGSUM::PRODUCT &
		FOR I% = 1% TO PRODUCT_SUMMARY%

	IF (MOD_LINE% = 0%)
	THEN
		PRODUCT_SUMMARY% = PRODUCT_SUMMARY% + 1%
		PRODUCT_SUMMARY(PRODUCT_SUMMARY%)::PRODUCT = REGSUM::PRODUCT
		PRODUCT_SUMMARY(PRODUCT_SUMMARY%)::DESCRIPTION = &
			PD_PRODUCT::DESCRIPTION
		PRODUCT_SUMMARY(PRODUCT_SUMMARY%)::QTY = REGSUM::QTY
	ELSE
		PRODUCT_SUMMARY(MOD_LINE%)::QTY = &
			PRODUCT_SUMMARY(MOD_LINE%)::QTY + REGSUM::QTY
	END IF

	GOTO 17110

17190	GOSUB DoRegTotal

	RETURN

	%PAGE

	!*******************************************************************

 DoRegTotal:
	IF ONE_REG_COUNT% <> 0%
	THEN
		IF ONE_REG_RQTY = 0.0
		THEN
			PCT = 0.0
		ELSE
			PCT = ONE_REG_QTY / ONE_REG_RQTY * 100.0
		END IF

		TEXT$ = "   " + &
			"              " + " " + &
			"Total                         " + " " + &
			"          " + " " + &
			"    " + " " + &
			"          " + " " + &
			FORMAT$(ONE_REG_QTY, "###,###,###.##") + &
			FORMAT$(ONE_REG_RQTY, "###,###,###.##") + &
			FORMAT$(PCT, "######.##%")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	ONE_REG_JOB$ = REGSUM::JOB
	ONE_REG_QTY = 0.0
	ONE_REG_RQTY = 0.0
	ONE_REG_COUNT% = 0%

	RETURN

	!*******************************************************************

 TotalSum:
	IF TOTAL_QTY = 0.0
	THEN
		PCT = 0.0
	ELSE
		PCT = ALL_REG_QTY / TOTAL_QTY * 100.0
	END IF

	TEXT$ = "   " + &
		"              " + " " + &
		"Grand Total                   " + " " + &
		"          " + " " + &
		"    " + " " + &
		"          " + " " + &
		FORMAT$(ALL_REG_QTY, "###,###,###.##") + &
		FORMAT$(TOTAL_QTY, "###,###,###.##") + &
		FORMAT$(PCT, "######.##%")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	ALL_REG_QTY = 0.0

	RETURN

	%PAGE

	!*******************************************************************
	! Print the header line
	!*******************************************************************

 PrintHeaderItem:
17300	PD_PRODUCT::DESCRIPTION = ""

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, KEY #0% EQ THIS_PRODUCT$, REGARDLESS
	USE
		CONTINUE 17380
	END WHEN

17380	TEXT$ = THIS_PRODUCT$ + " " + &
		PD_PRODUCT::DESCRIPTION + " " + &
		THIS_LINE$ + " " + &
		FORMAT$(THIS_QTY, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 4%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17390	RETURN

	%PAGE

	!*******************************************************************
	! Read information from bill of material for standard %
	!*******************************************************************

 ReadBM:
17500	REQQTY = 0.0

	WHEN ERROR IN
		GET #BM_RELATION.CH%, KEY #0% EQ REGSUM::PRODUCT, &
			REGARDLESS
	USE
		CONTINUE EndBOM IF ERR = 155%
		FILENAME$  = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	QTY_LEVEL(0%) = 1.0
	LEVEL% = 1%

 GoDownTree:

	TEST_PRODUCT(LEVEL%) = BM_RELATION::PRODUCT
	QTY_LEVEL(LEVEL%) = QTY_LEVEL(LEVEL% - 1%) * BM_RELATION::QUANTITY
	RFA_LEVEL(LEVEL%) = GETRFA(BM_RELATION.CH%)

	IF BM_RELATION::COMPONENT = THIS_PRODUCT$
	THEN
		REQQTY = REQQTY + QTY_LEVEL(LEVEL%)
		GOTO GoUpTree
	END IF

17520	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #0% EQ BM_RELATION::COMPONENT, &
			REGARDLESS
	USE
		CONTINUE 17530 IF ERR = 155%
		FILENAME$  = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	LEVEL% = LEVEL% + 1%
	GOTO GoDownTree

 GoUpTree:
	LEVEL% = LEVEL% - 1%
	GOTO 17550 IF LEVEL% = 0%

17530	WHEN ERROR IN
		GET #BM_RELATION.CH%, RFA RFA_LEVEL(LEVEL%), REGARDLESS
		GET #BM_RELATION.CH%, REGARDLESS
	USE
		CONTINUE GoUpTree IF ERR = 155% OR ERR = 11%
		FILENAME$  = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	IF BM_RELATION::PRODUCT <> TEST_PRODUCT(LEVEL%)
	THEN
		GOTO GoUpTree
	ELSE
		GOTO GoDownTree
	END IF

 EndBOM:
17550	RETURN

	%PAGE

	!*******************************************************************

 ExitTotal:

	!
	! Sort Product Summary (Bubble)
	!
	FOR I% = 1% TO PRODUCT_SUMMARY%
		FOR J% = 1% TO PRODUCT_SUMMARY% - I%
			IF PRODUCT_SUMMARY(J% + 1%)::PRODUCT < &
				PRODUCT_SUMMARY(J%)::PRODUCT
			THEN
				PRODUCT_SUMMARY_TEMP = PRODUCT_SUMMARY(J%)
				PRODUCT_SUMMARY(J%) = PRODUCT_SUMMARY(J% + 1%)
				PRODUCT_SUMMARY(J% + 1%) = PRODUCT_SUMMARY_TEMP
			END IF
		NEXT J%
	NEXT I%

	!
	! Print Product Summary
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
		"Summary by Product", 4%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FOR I% = 1% TO PRODUCT_SUMMARY%
		TEXT$ = PRODUCT_SUMMARY(I%)::PRODUCT + " " + &
			PRODUCT_SUMMARY(I%)::DESCRIPTION + &
			FORMAT$(PRODUCT_SUMMARY(I%)::QTY, &
			"#,###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	NEXT I%


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
