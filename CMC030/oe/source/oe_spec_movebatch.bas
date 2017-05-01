1	%TITLE "Move Records from One OE Period to Another"
	%SBTTL "OE_SPEC_MOVEBATCH"
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
	! ID:OE039
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program requires a Batch Number and a From OE Period.
	!	If a To OE Period is blank, the program will open the
	!	OE__REGLINE file and the OE__REGHEADER file and delete the
	!	batch records from these files.
	!	.b
	!	If the To OE Period field is not blank, the program will only
	!	open the OE__REGLINE file and update the particular batch
	!	records to the new TO Period.
	!	.note
	!	This program will not affect any system but
	!	the Order Entry System. You will need to move Inventory
	!	and  General Legder seperately.
	!	.lm -5
	!	.end note
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_SPEC_MOVEBATCH/LINE
	!	$ LINK/EXECUTABLE=OE_EXE: OE_SPEC_MOVEBATCH, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_SPEC_MOVEBATCH.OBJ;*
	!
	! Author:
	!
	!	03/22/93 - Dan Perkins
	!
	! Modification history:
	!
	!	04/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/02/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE)	OE_REGLINE_CDD		OE_REGLINE
	DECLARE			OE_REGLINE_CDD		OE_REGLINE_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_CUSTOM_EXAM

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION AR_EXAM_CUSTOM
	EXTERNAL LONG	FUNCTION OE_READ_REGLINE
	EXTERNAL LONG   FUNCTION PD_EXAM_PRODUCT

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
	! Open the REGLINE file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.PST"
	USE
		IF ERR = 138%	! Locked File
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Open the REGHEADER file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.PST"
	USE
		IF ERR = 138%	! Locked File
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	TITLE$(1%) = "PROCESS TO MOVE BATCH " + BATCH_NO$ + &
		" FROM " + FROM_PERIOD$ + " TO " + TO_PERIOD$

	TITLE$(2%) = "Order-Invoice System"

	TITLE$(3%) = ""

	TITLE$(4%) = "Doc#        ST SalCat   CusNumber   CusName" + &
		"                        CustPo#    OrdDate"  + &
		"     Location       Shipvia"

	TITLE$(5%) = "                 Line  TType  Product        Descr" + &
		"                        Qty   Ty   Re#   TranDate"  + &
		"    PostDate    PostTime    Batch"

	TITLE$(6%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! FIND the REGLINE record with this batch number
	!
	WHEN ERROR IN
		FIND #OE_REGLINE.CH%, KEY #2% EQ BATCH_NO$
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	TEST_ORDNUM$ = ""
	REGRECORDS%, HDRRECORDS% = 0%

 GetNextRec:
17020	WHEN ERROR IN
		GET #OE_REGLINE.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "OE_REGLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Make sure it has the same Batch number
	!
	GOTO ExitTotal IF OE_REGLINE::BATCH <> BATCH_NO$

	GOTO GetNextRec IF OE_REGLINE::PERIOD <> FROM_PERIOD$

	IF (OE_REGLINE::ORDNUM <> TEST_ORDNUM$) AND (TEST_ORDNUM$ <> "")
	THEN
		GOSUB DoHeader IF TO_PERIOD$ = ""
		TEST_ORDNUM$ = ""
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

17100	IF TEST_ORDNUM$ = ""
	THEN
		!
		! Get the HEADER record
		!
		WHEN ERROR IN
			GET #OE_REGHEADER.CH%, KEY #0% EQ OE_REGLINE::ORDNUM
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE GetNextRec IF ERR = 155%
			FILENAME$ = "OE_REGHEADER"
			CONTINUE HelpError
		END WHEN

		!
		! Get the Customer name
		!
		V% = AR_EXAM_CUSTOM(OE_REGHEADER::CUSNUM, AR_CUSTOM_EXAM)

		!
		! Print out one line
		!
		TEXT$ = CONV_STRING(OE_REGHEADER::ORDNUM, CMC$_LEFT) + "  " + &
			OE_REGHEADER::ORDTYPE + " " + &
			OE_REGHEADER::ORDCAT + "     " + &
			OE_REGHEADER::CUSNUM + "  " + &
			LEFT$(AR_CUSTOM_EXAM::CUSNAM, 30%) + " " + &
			OE_REGHEADER::CUSTPO + " " + &
			PRNT_DATE(OE_REGHEADER::ORDDATE, 8%) + "  " + &
			OE_REGHEADER::LOCATION + "           " + &
			OE_REGHEADER::SHIPVIA

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEST_ORDNUM$ = OE_REGLINE::ORDNUM

	END IF

	!
	! Get the Product Description
	!
	V% = PD_EXAM_PRODUCT(OE_REGLINE::PRODUCT, PD_PRODUCT_EXAM)

	SELECT OE_REGLINE::TRANTYPE

	CASE "01"
		TTYPE$ = "O"

	CASE "02"
		TTYPE$ = "S"

	CASE "03"
		TTYPE$ = "C"

	END SELECT

	!
	! Print out one line
	!
	TEXT$ = CONV_STRING(OE_REGLINE::ORDNUM, CMC$_LEFT) + "       " + &
		OE_REGLINE::LIN + "  " + &
		OE_REGLINE::TRANTYPE + "     " + &
		OE_REGLINE::PRODUCT + " " + &
		LEFT$(PD_PRODUCT_EXAM::DESCRIPTION, 20%) + &
		FORMAT$(OE_REGLINE::QTY, "#,###,###.##") + "   " + &
		TTYPE$ + "    " + &
		OE_REGLINE::SHIPNO + "    " + &
		PRNT_DATE(OE_REGLINE::TDATE, 8%) + "  " + &
		PRNT_DATE(OE_REGLINE::POSTDATE, 8%) + "  " + &
		PRNT_TIME(OE_REGLINE::POSTTIME, 0%) + "   " + &
		OE_REGLINE::BATCH

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

17200	!
	! Take care of the REGLINE record
	!
	IF TO_PERIOD$ <> ""
	THEN
		OE_REGLINE::PERIOD = TO_PERIOD$

		WHEN ERROR IN
			UPDATE #OE_REGLINE.CH%
		USE
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN
	ELSE
		WHEN ERROR IN
			DELETE #OE_REGLINE.CH%
		USE
			FILENAME$ = "OE_REGLINE"
			CONTINUE HelpError
		END WHEN
	END IF

	REGRECORDS% = REGRECORDS% + 1%

	GOTO GetNextRec

 ExitTotal:
	GOSUB DoHeader IF TO_PERIOD$ = ""

	IF HDRRECORDS% > 0%
	THEN
		ADD_TEXT$ = " Deleted OE_REGHEADER Records."

		TEXT$ = SPACE$(9%) + FORMAT$(HDRRECORDS%, "########") + ADD_TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	IF REGRECORDS% > 0%
	THEN
		IF TO_PERIOD$ = ""
		THEN
			ADD_TEXT$ = " Deleted OE_REGLINE Records."
		ELSE
			ADD_TEXT$ = " Transfered OE_REGLINE Records."
		END IF

		TEXT$ = SPACE$(9%) + FORMAT$(REGRECORDS%, "########") + ADD_TEXT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
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

 DoHeader:
	!
	! Make sure that we have no REGLINE records if we are going to
	! delete the header.
	!
	LINE$ = "    "

	GOTO ExitDoHeader IF OE_READ_REGLINE(OE_REGHEADER::ORDNUM, &
		LINE$, "GT", OE_REGLINE_READ, QTY()) = CMC$_NORMAL

17300	WHEN ERROR IN
		DELETE #OE_REGHEADER.CH%
	USE
		FILENAME$ = "OE_REGHEADER"
		CONTINUE HelpError
	END WHEN

	HDRRECORDS% = HDRRECORDS% + 1%

 ExitDoHeader:
	RETURN

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

	%PAGE

32767	END
