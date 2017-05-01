1	%TITLE "Resynch Inventory Allocation and Order Qty"
	%SBTTL "IC_SPEC_RESYNCHAO"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Adjust Allocation and Order\* routine is used to create a
	!	transaction journal to reset the on-order and allocated quantities
	!	to match the amounts in the PS and OE registers.
	!	.b
	!	^*Note:\* There can be ^*NO\* outstanding journals
	!	in the "Order Entry" or "Point of Sale" systems
	!	when you process the resynch
	!	procedure. Orders/shipping/invoicing journals should be posted
	!	prior to executing this process.
	!	.lm -5
	!
	! Index:
	!	.x Resynch>Inventory Balance File
	!	.x Inventory Balance File>Resynch
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_SPEC_RESYNCHAO/LINE
	!	$ LINK/EXE=IC_EXE: IC_SPEC_RESYNCHAO, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_SPEC_RESYNCHAO.OBJ;*
	!
	! Author:
	!
	!	10/12/93 - Frank F. Starman
	!
	! Modification history:
	!
	!	01/12/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/18/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer
	!
	!	08/19/98 - Kevin Handy
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

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURNAL.HB"
	MAP (IC_JOURNAL)	IC_JOURNAL_CDD	IC_JOURNAL

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	DECLARE LONG	EXIT_STATUS

	DECLARE STRING	TITLE(10%)
	DECLARE STRING	BATCH_NUMBER

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION IC_WRIT_ALLOCATE
	EXTERNAL LONG   FUNCTION IC_WRIT_ORDER
	EXTERNAL LONG   FUNCTION IC_WRIT_35BALANCE
	EXTERNAL LONG	FUNCTION ASSG_POSTBATCH

	%PAGE

	ON ERROR GOTO 19000

	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO Aborted IF UTL_REPORTX::STAT

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Locations\*
	!	.b
	!	.lm +5
	!	The ^*Locations\* field enters the location
	!	codes that are to be included.
	!	.lm -5
	!
	! Index:
	!	.x Locations>Inventory Resynchronize
	!
	!--

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) Batch Number\*
	!	.b
	!	Batch number of the transaction journal
	!	to be created.
	!
	! Index:
	!
	!--

	TRANSDATE$ = EDIT$(DATE_STOREDATE(UTL_REPORTX::OPTDEF(2%)), -1%)
	!++
	! Abstract:FLD03
	!	^*(03) Transaction Date\*
	!	.b
	!	Date to put on transaction records created in the journal.
	!
	! Index:
	!
	!--

	OPERATOR$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)
	!++
	! Abstract:FLD04
	!	^*(04) Operator\*
	!	.b
	!	Operator to put on the transactions created in the journal.
	!
	! Index:
	!
	!--

	TITLE(1%) = "INVENTORY RESYNCH ALLOCATION AND ORDER PROTOCOL"
	TITLE(2%) = "Inventory Control System"
	TITLE(3%) = ""

	!
	! Heading
	!
	TITLE(4%) = "."

	!******************************************************************
	! Check if process has been interrupted
	!******************************************************************
	!
	! Open up batch control file and check if interrupted
	!
	EXIT_STATUS = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "IC_RESYNCH", "", "", "")

	%PAGE

300	!
	! Open IC control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.UPD"

		GET #IC_CONTROL.CH%, RECORD 1%
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	CUR_PERIOD$, YYYYPP$ = IC_CONTROL::PERIOD

	EXIT_STATUS = CMC$_NORMAL

	SELECT IC_CONTROL::CONTROLFLAG

	CASE "0", "1", "4"
		!
		! Check the current period
		!
		IF READ_PERIOD("READ", IC_CONTROL::ERA, CUR_PERIOD$, &
			PERIOD_DESCR$, STAT$, START_DATE$, &
			END_DATE$, 0%)
		THEN
			TEXT$ = "%Undefined IC period " + CUR_PERIOD$

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			EXIT_STATUS = CMC$_WARNING
		END IF

	CASE ELSE
		TEXT$ = "%IC Control flag = " + IC_CONTROL::CONTROLFLAG

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		EXIT_STATUS = CMC$_WARNING

	END SELECT

	!******************************************************************
	! Assign batch number
	!******************************************************************
	!
	! Open up batch control file and get a batch number
	!
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "IC_RESYNCH", "RESYNCH", &
		"", "") <> CMC$_NORMAL

	GOTO Aborted IF EXIT_STATUS = CMC$_WARNING

	!
	IC_CONTROL::CONTROLFLAG = "4"
	UPDATE #IC_CONTROL.CH%

305	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_JOURNAL.CRE"
	USE
		FILENAME$ = "IC_JOURNAL"
		CONTINUE HelpError
	END WHEN

	!
	! Open Product file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		CONTINUE Completed IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "PD_PRODUCT.MAS", "", "", "")

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", "",  "")

	RESET #PD_PRODUCT.CH%

	IC_JOURNAL::LOCATION	= LOCATION$
	IC_JOURNAL::TRANS_DATE	= TRANSDATE$
	IC_JOURNAL::TYPE_B	= ""
	IC_JOURNAL::QUANTITY_B	= 0.0
	IC_JOURNAL::SUBACCOUNT	= ""
	IC_JOURNAL::TOLOCATION	= ""
	IC_JOURNAL::EXPACCT	= ""
	IC_JOURNAL::CROSS_REF	= ""
	IC_JOURNAL::PRIMARY_REF = "ADJUST"
	IC_JOURNAL::STATIONMAN  = OPERATOR$

 GetNextRec:
	!
	! Main loop starts here
	!
	!
	! Get next record
	!
	GET #PD_PRODUCT.CH%, REGARDLESS

	V% = IC_WRIT_ALLOCATE(PD_PRODUCT, LOCATION$, DIFF_QTY)

	IF DIFF_QTY <> 0.0
	THEN
		IC_JOURNAL::PRODUCT	= PD_PRODUCT::PRODUCT_NUM
		IC_JOURNAL::TYPE_A	= "SO"
		IC_JOURNAL::QUANTITY_A	= DIFF_QTY
		PUT #IC_JOURNAL.CH%

		V% = IC_WRIT_35BALANCE(IC_JOURNAL::PRODUCT, &
			LOCATION$, "SO", &
			IC_JOURNAL::QUANTITY_A)

		TEXT$ = IC_JOURNAL::PRODUCT + " " + &
			IC_JOURNAL::TYPE_A + " " + &
			FORMAT$(IC_JOURNAL::QUANTITY_A, "###,###.###")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	V% = IC_WRIT_ORDER(PD_PRODUCT, LOCATION$, DIFF_QTY)

	IF DIFF_QTY <> 0.0
	THEN
		IC_JOURNAL::PRODUCT	= PD_PRODUCT::PRODUCT_NUM
		IC_JOURNAL::TYPE_A	= "PO"
		IC_JOURNAL::QUANTITY_A	= -DIFF_QTY
		PUT #IC_JOURNAL.CH%

		V% = IC_WRIT_35BALANCE(IC_JOURNAL::PRODUCT, &
			LOCATION$, "PO", &
			IC_JOURNAL::QUANTITY_A)

		TEXT$ = IC_JOURNAL::PRODUCT + " " + &
			IC_JOURNAL::TYPE_A + " " + &
			FORMAT$(IC_JOURNAL::QUANTITY_A, "###,###.###")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!CALL IC_WRIT_ORDER(PD_PRODUCT,DIFF_QTY)

	!IF DIFF_QTY <> 0.0
	!THEN
	!	IC_JOURNAL::TYPE_A	= "PO"
	!	IC_JOURNAL::QUANTITY_A	= DIFF_QTY
	!	PUT #IC_JOURNAL.CH%
	!END IF

	GOTO GetNextRec

 Completed:
	!
	! Update period file
	!
	GET #IC_CONTROL.CH%, RECORD 1%

	IC_CONTROL::CONTROLFLAG = "0"

	UPDATE #IC_CONTROL.CH%

	!
	! Complete closing process and remove batch control
	!
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "", "", "")

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

 Aborted:
	!******************************************************************
	! Abort process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "", CUR_PERIOD$, PERIOD$)

	GOTO ExitProgram

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), UTL_REPORTX, TITLE(), 0%)

	GOTO Aborted

	%Page

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
