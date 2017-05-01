1	%TITLE "Move Records from One WP Period to Another"
	%SBTTL "WP_SPEC_MOVEBATCH"
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
	! ID:WP0035
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program requires a Batch Number and a From WP Period.
	!	If the ^*To WP Period\* is blank, the program will open the
	!	WP_REGLINE file and the WP_REREGISTER file and delete the
	!	batch records from these files.
	!	.b
	!	If the ^*To WP Period\* field is not blank, the program will only
	!	open the WP_REQREGISTER file and update the particular batch
	!	records to the new TO Period.
	!	.table 3,25
	!	.te
	!	1) This program will not affect any system but
	!	the Work in Process System.
	!	.lm -5
	!	.END TABLE
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_SPEC_MOVEBATCH/LINE
	!	$ LINK/EXECUTABLE=WP_EXE: WP_SPEC_MOVEBATCH, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_SPEC_MOVEBATCH.OBJ;*
	!
	! Author:
	!
	!	03/16/93 - Dan Perkins
	!
	! Modification history:
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
	!	09/09/97 - Kevin Handy
	!		Lose unecessary external defintions
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/14/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Trap error 11 in 17020 instead of error 155
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

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER

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
	!	.x Batch
	!
	!--

	FROM.PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

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
	!	.x From Period
	!	.x Period>From
	!
	!--

	TO.PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

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
	!	.x To Period
	!
	!--

	!
	! Open the REGLINE file
	!
300	IF TO.PERIOD$ = ""
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.PST"
		USE
			IF ERR = 138%
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "WP_REGLINE"
			CONTINUE HelpError
		END WHEN

	END IF

	!
	! Open the REQREGISTER file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.PST"
	USE
		IF ERR = 138%
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	TITLE$(1%) = "PROCESS TO MOVE BATCH " + BATCH_NO$ + &
		" FROM " + FROM.PERIOD$ + " TO " + TO.PERIOD$

	TITLE$(2%) = "Work in Process System"

	TITLE$(3%) = ""

	IF TO.PERIOD$ = ""
	THEN
		TITLE$(4%) = "Job#       Line RTyp TTyp ItemCode       " + &
			"   Quantity       Cost"

		TITLE$(5%) = "Job#       Line Req#       RLin RTyp " + &
			"Product        TranDate      Quantity" + &
			"     Amount"

		TITLE$(6%) = "."

	ELSE
		TITLE$(4%) = "Job#       Line Req#       RLin RTyp " + &
			"Product        TranDate      Quantity" + &
			"     Amount"

		TITLE$(5%) = "."
	END IF

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	GOTO 17100 IF TO.PERIOD$ <> ""

	!
	! FIND the REGLINE record with this batch number
	!
	WHEN ERROR IN
		FIND #WP_REGLINE.CH%, KEY #2% EQ BATCH_NO$
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 17100 IF ERR = 155%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	REGRECORDS% = 0%

 GetRegRec:
17020	WHEN ERROR IN
		GET #WP_REGLINE.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE 17100 IF ERR = 11%
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Make sure it has the same Batch number
	!
	GOTO 17100 IF WP_REGLINE::BATCH <> BATCH_NO$

	TEXT$ = WP_REGLINE::JOB + " " + &
		WP_REGLINE::LLINE + " " + &
		WP_REGLINE::REC_TYPE + "   " + &
		WP_REGLINE::TTYPE + "    " + &
		WP_REGLINE::ITEMCODE + " " + &
		FORMAT$(WP_REGLINE::QTY, "#######.###") + " " + &
		FORMAT$(WP_REGLINE::COST, "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17050	WHEN ERROR IN
		DELETE #WP_REGLINE.CH%
	USE
		FILENAME$ = "WP_REGLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Add to the record counter, and go up to GET the next record
	!
	REGRECORDS% = REGRECORDS% + 1%

	GOTO GetRegRec

17100	IF REGRECORDS% > 0%
	THEN
		TEXT$ = SPACE$(9%) + FORMAT$(REGRECORDS%, "########") + &
			" Deleted WP_REGLINE records."

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

	!
	! FIND the first record with this batch number
	!
	WHEN ERROR IN
		FIND #WP_REQREGISTER.CH%, KEY #3% EQ BATCH_NO$
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	!
	! Reset record counter to zero
	!
	REQRECORDS% = 0%

 GetReqRec:
17120	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	!
	! Make sure it has the same Batch number
	!
	GOTO ExitTotal IF WP_REQREGISTER::BATCH <> BATCH_NO$

	GOTO GetReqRec IF WP_REQREGISTER::PERIOD <> FROM.PERIOD$

	!
	! Print the record
	!
	TEXT$ = WP_REQREGISTER::JOB + " " + &
		WP_REQREGISTER::LLINE + " " + &
		WP_REQREGISTER::REQNUM + " " + &
		WP_REQREGISTER::REQLIN + " " + &
		WP_REQREGISTER::RECTYP + "   " + &
		WP_REQREGISTER::PRODUCT + " " + &
		PRNT_DATE(WP_REQREGISTER::TRANDATE, 8%) + " " + &
		FORMAT$(WP_REQREGISTER::QTY, "#######.###") + " " + &
		FORMAT$(WP_REQREGISTER::AMT, "#######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17150	WHEN ERROR IN
		IF TO.PERIOD$ = ""
		THEN
			DELETE #WP_REQREGISTER.CH%
		ELSE
			WP_REQREGISTER::PERIOD = TO.PERIOD$
			UPDATE #WP_REQREGISTER.CH%
		END IF
	USE
		FILENAME$ = "WP_REQREGISTER"
		CONTINUE HelpError
	END WHEN

	!
	! Add to the record counter, and go up to GET the next record
	!
	REQRECORDS% = REQRECORDS% + 1%

	GOTO GetReqRec

 ExitTotal:
	IF REQRECORDS% > 0%
	THEN
		IF TO.PERIOD$ = ""
		THEN
			ADD.TEXT$ = " Deleted WP_REQREGISTER Records."
		ELSE
			ADD.TEXT$ = " Transfered WP_REQREGISTER Records."
		END IF

		TEXT$ = SPACE$(9%) + FORMAT$(REQRECORDS%, "########") + ADD.TEXT$

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

 HelpError:
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

	%PAGE

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
