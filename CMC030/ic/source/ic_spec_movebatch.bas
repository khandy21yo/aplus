1	%TITLE "Move Records from One IC Period to Another"
	%SBTTL "IC_SPEC_MOVEBATCH"
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
	! ID:IC011
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program requires a Batch Number and a From IC Period.
	!	If a To IC Period is not blank, the program will open the
	!	IC "from" file, find all records with that batch number, put
	!	them into the "to" file, and delete them from the "from" file.
	!	If the To IC Period field is left blank, the particular BATCH
	!	records will be DELETED from the From IC Period file.
	!	.table 3,25
	!	.te
	!	1) This program will not affect any system but
	!	the Inventory Control.
	!	.te
	!	.b
	!	2) The IC will need to be resynchronized after this
	!	program has been run.
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_SPEC_MOVEBATCH/LINE
	!	$ LINK/EXECUTABLE=IC_EXE: IC_SPEC_MOVEBATCH, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_SPEC_MOVEBATCH.OBJ;*
	!
	! Author:
	!
	!	10/29/91 - Dan Perkins
	!
	! Modification history:
	!
	!	02/28/92 - Kevin Handy
	!		Changed "CMC$NORMAL" to "CMC$_NORMAL".
	!
	!	03/03/92 - Kevin Handy
	!		Changed "CMC$UNTERROR" to "CMC$_UNTERROR" twice.
	!
	!	04/27/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/16/93 - Dan Perkins
	!		Rewrote program to simply put recods to the To
	!		Period file and delete them from the From Period
	!		file.
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/01/94 - Kevin Handy
	!		Fixed bug where it would only open to TO_PERIOD
	!		file if it was blank, instead of it it wasn't
	!		blank, thus losing all of the transfer records
	!		to channel 0 on a transfer, and creating a file
	!		without a period when deletion was asked for.
	!		(I HATE PROGRAMS THAT TRY TO DO SEVERAL DIFFERENT
	!		THINGS)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/05/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/29/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP (IC_TRANSACTION)	IC_TRANSACTION_CDD	IC_TRANSACTION

	%PAGE

	!**************************************************************
	! Get some stuff done before we start
	!**************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	%PAGE

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
	!	.x From Period
	!	.x Period>From
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
	!	.b
	!	If this field is left blank, then the records will be deleted.
	!	.lm -5
	!
	! Index:
	!	.x To Period
	!
	!--

	!
	! Open IC_CONTROL file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"
		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE IC_CONTROL.CH%
		CALL ASSG_FREECHANNEL(IC_CONTROL.CH%)
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	CLOSE_PERIOD$ = IC_CONTROL::PERIOD

	!
	! Has the FROM period been closed?
	!
	IF FROM_PERIOD$ < CLOSE_PERIOD$
	THEN
		TEXT$ = SPACE$(18%) + "IC period " + FROM_PERIOD$ + &
			" has been closed."

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram
	END IF

	!
	! Open the "from" IC ledger file
	!
	YYYYPP$ = FROM_PERIOD$

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.UPD"
	USE
		IF ERR = 138%
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "IC_TRANSACTION_" + FROM_PERIOD$
		CONTINUE HelpError
	END WHEN

	!
	! Store the channel number for the "from" file
	!
	IC_TRANSACTION_FROM_CH% = IC_TRANSACTION.CH%
	IC_TRANSACTION.CH% = 0%

320	IF TO_PERIOD$ <> ""
	THEN
		!
		! Has the TO period been closed?
		!
		IF TO_PERIOD$ < CLOSE_PERIOD$
		THEN
			TEXT$ = SPACE$(18%) + "IC period " + TO_PERIOD$ + &
				" has been closed."

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			GOTO ExitProgram
		END IF

		!
		! Reset the YYYYPP$ variable, and open the "to" file
		!
		YYYYPP$ = TO_PERIOD$

		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.CRE"
		USE
			FILENAME$ = "IC_TRANSACTION_" + TO_PERIOD$
			CONTINUE HelpError
		END WHEN

	END IF

 ReportTitle:
	TITLE$(1%) = "PROCESS TO MOVE BATCH " + BATCH_NO$ + &
		" FROM " + FROM_PERIOD$ + " TO " + TO_PERIOD$

	TITLE$(2%) = "Inventory Control System"

	TITLE$(3%) = ""

	TITLE$(4%) = "Product        Location TransDate  "      + &
		"PriRef           CrossRef   Subaccount  " + &
		"TTypeA   QuantityA   TTypeB   QuantityB"

	TITLE$(5%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	!
	! FIND the first record with this batch number
	!
	WHEN ERROR IN
		FIND #IC_TRANSACTION_FROM_CH%, KEY #1% EQ BATCH_NO$
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE ExitTotal IF ERR = 155%
		FILENAME$ = "IC_TRANSACTION_" + FROM_PERIOD$
		CONTINUE HelpError
	END WHEN

	!
	! Reset record counter to zero
	!
	RECORDS% = 0%

 GetNextRec:
	!
	! GET the (next) Ledger record
	!
17020	WHEN ERROR IN
		GET #IC_TRANSACTION_FROM_CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "IC_TRANSACTION_" + FROM_PERIOD$
		CONTINUE HelpError
	END WHEN

	!
	! Make sure it has the same Batch number
	!
	GOTO ExitTotal IF IC_TRANSACTION::BATCH <> BATCH_NO$

	!
	! Print the record
	!
	TEXT$ = IC_TRANSACTION::PRODUCT + " "	+ &
		IC_TRANSACTION::LOCATION + "     " + &
		PRNT_DATE(IC_TRANSACTION::TRANS_DATE, 8%) + " "	+ &
		IC_TRANSACTION::PRIMARY_REF + " " + &
		IC_TRANSACTION::CROSS_REF + " "	+ &
		IC_TRANSACTION::SUBACCOUNT + "  "    + &
		IC_TRANSACTION::TYPE_A + "     " + &
		FORMAT$(IC_TRANSACTION::QUANTITY_A, "#######.###") + "   " + &
		IC_TRANSACTION::TYPE_B + "     " + &
		FORMAT$(IC_TRANSACTION::QUANTITY_B, "#######.###")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17100	IF TO_PERIOD$ <> ""
	THEN
		!
		! Put the record to the new period file
		!
		WHEN ERROR IN
			PUT #IC_TRANSACTION.CH%
		USE
			FILENAME$ = "IC_TRANSACTION_" + TO_PERIOD$
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Delete the record from the from period file
	!
17150	WHEN ERROR IN
		DELETE #IC_TRANSACTION_FROM_CH%
	USE
		FILENAME$ = "IC_TRANSACTION_" + FROM_PERIOD$
		CONTINUE HelpError
	END WHEN

	!
	! Add to the record counter, and go up to GET the next record
	!
	RECORDS% = RECORDS% + 1%

	GOTO GetNextRec

 ExitTotal:
	IF RECORDS% > 0%
	THEN
		IF TO_PERIOD$ = ""
		THEN
			ADD_TEXT$ = " Deleted Records."
		ELSE
			ADD_TEXT$ = " Transfered Records."
		END IF

		TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "########") + ADD_TEXT$

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

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Trap untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
