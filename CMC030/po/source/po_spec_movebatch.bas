1	%TITLE "Move Records from One PO Period to Another"
	%SBTTL "PO_SPEC_MOVEBATCH"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
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
	! ID:PO011
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program requires a Batch Number and a From PO Period.
	!	If a To PO Period is blank, the program will open the
	!	PO_REG_LINE file and the PO_REG_SUB_LINE file and delete the
	!	batch records from these files.
	!	.b
	!	If the To PO Period field is not blank, the program will only
	!	open the PO_REG_LINE file and update the particular batch
	!	records to the new TO Period.
	!	.b
	!	1) This program will not affect any system but
	!	the Purchase Order System.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_SPEC_MOVEBATCH/LINE
	!	$ LINK/EXECUTABLE=PO_EXE: PO_SPEC_MOVEBATCH, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_SPEC_MOVEBATCH.OBJ;*
	!
	! Author:
	!
	!	03/22/93 - Dan Perkins
	!
	! Modification history:
	!
	!	04/13/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/22/94 - Kevin Handy
	!		Fixed MAJOR bug. Before fix it would look through
	!		the LINE file, and delete the line and all sub
	!		lines if the batch number matched, EVEN IF THE
	!		SUB LINES WERE NOT FROM THAT BATCH. Now will
	!		remove the sub-lines, then will remove the lines
	!		ONLY IF THERE ARE NO SUB LINES UNDER IT.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/04/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include special CMC information
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	MAP (PO_REG_SUB_LINE)	PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	DECLARE			AP_VENDOR_CDD		AP_VENDOR_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! External functions
	!
	EXTERNAL LONG     FUNCTION AP_EXAM_VENDOR
	EXTERNAL LONG     FUNCTION PD_EXAM_PRODUCT

	%PAGE

	!**************************************************************
	! Get some stuff done before we start
	!**************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!**************************************************************
	! Process `from user' input
	!**************************************************************

	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Batch\*
	!	.b
	!	.lm +5
	!	The ^*Batch\* field enters the batch number of the records
	!	which will be transferred.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) From Period\*
	!	.b
	!	.lm +5
	!	The ^*From Period\* field enters the period from which
	!	the records will be taken for replacement in a different ledger.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) To Period\*
	!	.b
	!	.lm +5
	!	The ^*To Period\* field enters the period to which the
	!	records will be transferred.
	!	.lm -5
	!
	! Index:
	!
	!--

	!
	! Open the REG_LINE file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.PST"
	USE
		IF ERR = 138%	! Locked File
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

	!
	! Open the REG_SUB_LINE file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.PST"
	USE
		IF ERR = 138%	! Locked File
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	TITLE$(1%) = "PROCESS TO MOVE BATCH " + BATCH_NO$ + &
		" FROM " + FROM_PERIOD$ + " TO " + TO_PERIOD$

	TITLE$(2%) = "Purchase Order System"

	TITLE$(3%) = ""

	TITLE$(4%) = "PO#         Line  Type  Vendor      Name               " + &
		"             OrderDate   Location"

	TITLE$(5%) = "                     Product         Description" + &
		"                     UOM  Batch"

	TITLE$(6%) = "                        Action  ActionDate           Qty" + &
		"         Price  Subacct     Account             Batch"

	TITLE$(7%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	REGRECORDS%, SUBRECORDS% = 0%

17100	!
	! Check sub-line file first
	!
	WHEN ERROR IN
		FIND #PO_REG_SUB_LINE.CH%, &
			KEY #1% EQ BATCH_NO$, &
			REGARDLESS
	USE
		CONTINUE DoHeader IF ERR = 155%
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

	TEXT$ = "Processing Sub-lines:"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

 GetLine:
17120	WHEN ERROR IN
		GET #PO_REG_SUB_LINE.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE DoHeader IF ERR = 11%
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

	GOTO DoHeader IF PO_REG_SUB_LINE::BATCH <> BATCH_NO$

	SELECT PO_REG_SUB_LINE::PO_ACTION

	CASE "01"
		TTYPE$ = "O"

	CASE "02"
		TTYPE$ = "R"

	CASE "03"
		TTYPE$ = "C"

	CASE "09"
		TTYPE$ = "I"

	END SELECT

	!
	! Print out one line
	!
	TEXT$ = CONV_STRING(PO_REG_SUB_LINE::PO, CMC$_LEFT) + "  " + &
		PO_REG_SUB_LINE::PO_LINE + "        " + &
		TTYPE$ + "       " + &
		PRNT_DATE(PO_REG_SUB_LINE::ACTION_DATE, 8%) + "  " + &
		FORMAT$(PO_REG_SUB_LINE::QTY, "#,###,###.##") + "  " + &
		FORMAT$(PO_REG_SUB_LINE::PRICE, "#,###,###.##") + "  " + &
		PO_REG_SUB_LINE::SUBACCT + "  " + &
		PO_REG_SUB_LINE::ACCOUNT + "  " + &
		PO_REG_SUB_LINE::BATCH

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Delete the record if TO_PERIOD is blank
	!
17150	IF TO_PERIOD$ = ""
	THEN
		WHEN ERROR IN
			DELETE #PO_REG_SUB_LINE.CH%
		USE
			FILENAME$ = "PO_REG_SUB_LINE"
			CONTINUE HelpError
		END WHEN

		SUBRECORDS% = SUBRECORDS% + 1%
	END IF

	GOTO GetLine

 DoHeader:
	!
	! FIND the REGLINE record with this batch number
	!
17180	WHEN ERROR IN
		FIND #PO_REG_LINE.CH%, KEY #3% EQ BATCH_NO$
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 10%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "Processing Lines:"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

 GetNextRec:
17190	WHEN ERROR IN
		GET #PO_REG_LINE.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

	!
	! Make sure it has the same Batch number
	!
	GOTO ExitTotal IF PO_REG_LINE::BATCH <> BATCH_NO$

	GOTO GetNextRec IF PO_REG_LINE::PERIOD <> FROM_PERIOD$

	!
	! Get the vendor name
	!
	V% = AP_EXAM_VENDOR(PO_REG_LINE::VENDOR, AP_VENDOR_EXAM)

	!
	! Get the product description
	!
	V% = PD_EXAM_PRODUCT(PO_REG_LINE::PRODUCT, PD_PRODUCT_EXAM)

	!
	! Print out one line
	!
	TEXT$ = CONV_STRING(PO_REG_LINE::PO, CMC$_LEFT) + "  " + &
		PO_REG_LINE::PO_LINE + "  " + &
		PO_REG_LINE::PO_TYPE + "    " + &
		PO_REG_LINE::VENDOR + "  " + &
		LEFT(AP_VENDOR_EXAM::VENNAM, 30%) + "  " + &
		PRNT_DATE(PO_REG_LINE::ORDDATE, 8%) + "  " + &
		PO_REG_LINE::FROMLOCATION

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = CONV_STRING(PO_REG_LINE::PO, CMC$_LEFT) + "  " + &
		PO_REG_LINE::PO_LINE + "     " + &
		PO_REG_LINE::PRODUCT + "  " + &
		LEFT(PD_PRODUCT_EXAM::DESCRIPTION, 30%) + "  " + &
		PO_REG_LINE::UOM + "   " + &
		PO_REG_LINE::BATCH

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17200	!
	! Try to delete the header
	!
	IF TO_PERIOD$ = ""
	THEN
		!
		! Only delete the header if it is not needed
		!
		WHEN ERROR IN
			GET #PO_REG_SUB_LINE.CH%, &
				KEY #0% EQ PO_REG_LINE::PO + PO_REG_LINE::PO_LINE, &
				REGARDLESS

			DELETE #PO_REG_LINE.CH%
		USE
			CONTINUE 17250 IF ERR = 155%
			FILENAME$ = "PO_REG_LINE"
			CONTINUE HelpError
		END WHEN

	ELSE
		PO_REG_LINE::PERIOD = TO_PERIOD$

		WHEN ERROR IN
			UPDATE #PO_REG_LINE.CH%
		USE
			CONTINUE 17250 IF ERR = 155%
			FILENAME$ = "PO_REG_LINE"
			CONTINUE HelpError
		END WHEN
	END IF

	REGRECORDS% = REGRECORDS% + 1%

17250	GOTO GetNextRec

 ExitTotal:
	IF REGRECORDS% > 0%
	THEN
		IF TO_PERIOD$ = ""
		THEN
			ADD.TEXT$ = " Deleted PO_REG_LINE Records."
		ELSE
			ADD.TEXT$ = " Transfered PO_REG_LINE Records."
		END IF

		TEXT$ = SPACE$(9%) + FORMAT$(REGRECORDS%, "########") + &
			ADD.TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	IF SUBRECORDS% > 0%
	THEN
		ADD.TEXT$ = " Deleted PO_REG_SUB_LINE Records."

		TEXT$ = SPACE$(9%) + FORMAT$(SUBRECORDS%, "########") + &
			ADD.TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

 ExitProgram:
	!
	! Exit to next program or menu
	!
	CALL OUTP_FINISH(UTL_REPORTX)

	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN" + UTL_REPORTX::NEXTRUN, "")
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

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Trap untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
