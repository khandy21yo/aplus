1	%TITLE "Resynch One Inventory Period"
	%SBTTL "IC_SPEC_RESYNCH_HISTORY"
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Resynch Inventory Period\* routine is used to correct
	!	problems in inventory history files.
	!	.lm -5
	!
	! Index:
	!	.x Resynch>Inventory History
	!	.x Inventory History>Resynch
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_SPEC_RESYNCH_HISTORY/LINE
	!	$ LINK/EXE=IC_EXE: IC_SPEC_RESYNCH_HISTORY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_SPEC_RESYNCH_HISTORY.OBJ;*
	!
	! Author:
	!
	!	01/14/91 - Val James Allen
	!
	!	09/08/93 - Kevin Handy
	!		Taken from IC_SPEC_RESYNC and modified to update
	!		monthly history file for one period only.
	!		(I tried to make minimal changes, thus the arrays
	!		still exist.)
	!
	! Modification history:
	!
	!	09/08/93 - Kevin Handy
	!		Added error trapping for line 1180. Had removed
	!		it along with all other IC_35BALANCE code, but found
	!		out line wasn't related to IC_35BALANCE. Just
	!		had specified the wrong file name in the error.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/05/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/01/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	02/08/2001 - Kevin Handy
	!		Loose a lot of unecessary code to work across
	!		periods, since this program only does one
	!		period.
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
	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP (IC_TRANSACTION)	IC_TRANSACTION_CDD	IC_TRANSACTION

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
	!	The ^*Locations\* field
	!	determines the locations
	!	that are to be processed.
	!	.lm -5
	!
	! Index:
	!	.x Locations>Inventory Resynchronize
	!
	!--

	IC_PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) IC Period\*
	!	.b
	!	.lm +5
	!	Determines which period should be recalculated.
	!
	! Index:
	!	.x IC Period>Inventory Resynchronize
	!
	!--

	TRANSTYPE$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) Transaction Types\*
	!	.b
	!	.lm +5
	!	The ^*Transaction Types\* field determines the transaction
	!	codes that are to be processed.
	!	.lm -5
	!
	! Index:
	!	.x Transaction Types>Inventory Resynchronize
	!
	!--

	TITLE(1%) = "INVENTORY  RESYNCH  PROTOCOL"
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

	CUR.PERIOD$ = IC_CONTROL::PERIOD

	EXIT_STATUS = CMC$_NORMAL

	SELECT IC_CONTROL::CONTROLFLAG

	CASE "0", "1", "4"
		EXIT_STATUS = CMC$_NORMAL

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
		CUR.PERIOD$, CUR.PERIOD$) <> CMC$_NORMAL

	GOTO Aborted IF EXIT_STATUS = CMC$_WARNING

	!
	! Set close flag in control file
	!
	IC_CONTROL::CONTROLFLAG = "4"
	UPDATE #IC_CONTROL.CH%

1170	!
	! Open History file
	!
	YYYYPP$ = IC_PERIOD$
	YYYY$ = LEFT(YYYYPP$, 4%)

	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.CRE"
	USE
		FILENAME$ = "IC_35HISTORY"
		CONTINUE HelpError
	END WHEN

	S_INDEX% = VAL%(RIGHT(YYYYPP$, 5%))

1180	WHEN ERROR IN
		RESET #IC_35HISTORY.CH%
	USE
		CONTINUE 1185 IF ERR = 11%
		FILENAME$ = "IC_35HISTORY"
		CONTINUE HelpError
	END WHEN

 HistRec:
	WHEN ERROR IN
		GET #IC_35HISTORY.CH%
	USE
		CONTINUE 1185 IF ERR = 11%
		FILENAME$ = "IC_35HISTORY"
		CONTINUE HelpError
	END WHEN

	IF LOCATION$ = "" OR &
		COMP_STRING(EDIT$(IC_35HISTORY::LOCATION, -1%), LOCATION$)
	THEN
		IF TRANSTYPE$ = "" OR &
			COMP_STRING(EDIT$(IC_35HISTORY::TRANSTYPE, -1%), &
			TRANSTYPE$)
		THEN
			IND% = S_INDEX%
			IC_35HISTORY::PQUANTITY(IND%) = 0.0
			IC_35HISTORY::PRICEAMT(IND%) = 0.0
			IC_35HISTORY::COSTAMT(IND%) = 0.0

			UPDATE #IC_35HISTORY.CH%
		END IF
	END IF

	GOTO HistRec

1185	!

1200	!
	! Read posting quantities from the transaction file
	!
	EXIT_STATUS = CMC$_WARNING

	!
	! Open Transaction file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.OPN"
	USE
		CONTINUE 1390 IF ERR = 11% OR ERR = 5%
		FILENAME$ = "IC_TRANSACTION_" + YYYPP$
		CONTINUE HelpError
	END WHEN

	EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "IC_TRANSACTION_" + YYYYPP$ + &
		".LED", "", YYYYPP$, "")

	EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", YYYYPP$,  "")

	WHEN ERROR IN
		RESET #IC_TRANSACTION.CH%
	USE
		CONTINUE 1390 IF ERR = 11% OR ERR = 5%
		FILENAME$ = "IC_TRANSACTION_" + YYYPP$
		CONTINUE HelpError
	END WHEN

	IND% = VAL(RIGHT(YYYYPP$, 5%))

 GetNextRec:
	!
	! Main loop starts here
	!

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #IC_TRANSACTION.CH%, REGARDLESS
	USE
		CONTINUE 1390 IF ERR = 11% OR ERR = 5%
		FILENAME$ = "IC_TRANSACTION_" + YYYPP$
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec	IF LOCATION$ <> "" AND &
		COMP_STRING(EDIT$(IC_TRANSACTION::LOCATION, -1%), &
		LOCATION$) = 0%

1330	GOTO 1340 IF IC_TRANSACTION::TYPE_A = "" OR &
		IC_TRANSACTION::QUANTITY_A = 0.0

	GOTO 1340 IF TRANSTYPE$ <> "" AND &
		COMP_STRING(EDIT$(IC_TRANSACTION::TYPE_A, -1%), TRANSTYPE$) = 0%

	WHEN ERROR IN
		GET #IC_35HISTORY.CH%, KEY #0% EQ IC_TRANSACTION::PRODUCT + &
			IC_TRANSACTION::LOCATION + IC_TRANSACTION::TYPE_A + &
			IC_TRANSACTION::CROSS_REF + IC_TRANSACTION::SUBACCOUNT
	USE
		CONTINUE AddAHist IF ERR = 155% OR ERR = 131%
		FILENAME$ = "IC_35HISTORY"
		CONTINUE HelpError
	END WHEN

	IC_35HISTORY::PQUANTITY(IND%) = &
		IC_35HISTORY::PQUANTITY(IND%) + IC_TRANSACTION::QUANTITY_A
	IC_35HISTORY::PRICEAMT(IND%)  = &
		IC_35HISTORY::PRICEAMT(IND%) + IC_TRANSACTION::PRICE
	IC_35HISTORY::COSTAMT(IND%)   = &
		IC_35HISTORY::COSTAMT(IND%) + IC_TRANSACTION::COST

	UPDATE #IC_35HISTORY.CH%

	GOTO 1340

 AddAHist:
	IC_35HISTORY::PRODUCT	= IC_TRANSACTION::PRODUCT
	IC_35HISTORY::LOCATION	= IC_TRANSACTION::LOCATION
	IC_35HISTORY::CROSSREF	= IC_TRANSACTION::CROSS_REF
	IC_35HISTORY::SUBACCT	= IC_TRANSACTION::SUBACCOUNT
	IC_35HISTORY::TRANSTYPE	= IC_TRANSACTION::TYPE_A

	FOR I% = 0% TO 12%
		IC_35HISTORY::PQUANTITY(I%)	= 0.0
		IC_35HISTORY::PRICEAMT(I%)	= 0.0
		IC_35HISTORY::COSTAMT(I%)	= 0.0
	NEXT I%

	IC_35HISTORY::PQUANTITY(IND%) = IC_TRANSACTION::QUANTITY_A
	IC_35HISTORY::PRICEAMT(IND%)  = IC_TRANSACTION::PRICE
	IC_35HISTORY::COSTAMT(IND%)   = IC_TRANSACTION::COST

	PUT #IC_35HISTORY.CH%

1340	GOTO 1350 IF IC_TRANSACTION::TYPE_B = "" OR &
		IC_TRANSACTION::QUANTITY_B = 0.0

	GOTO 1350 IF TRANSTYPE$ <> "" AND &
		COMP_STRING(EDIT$(IC_TRANSACTION::TYPE_B, -1%), TRANSTYPE$) = 0%

	WHEN ERROR IN
		GET #IC_35HISTORY.CH%, KEY #0% EQ IC_TRANSACTION::PRODUCT + &
			IC_TRANSACTION::LOCATION + IC_TRANSACTION::TYPE_B + &
			IC_TRANSACTION::CROSS_REF + IC_TRANSACTION::SUBACCOUNT
	USE
		CONTINUE AddBHist IF ERR = 155% OR ERR = 131%
		FILENAME$ = "IC_35HISTORY"
		CONTINUE HelpError
	END WHEN

	IC_35HISTORY::PQUANTITY(IND%) = IC_35HISTORY::PQUANTITY(IND%) + &
		IC_TRANSACTION::QUANTITY_B

	IC_35HISTORY::PRICEAMT(IND%) = IC_35HISTORY::PRICEAMT(IND%) + &
		IC_TRANSACTION::PRICE

	IC_35HISTORY::COSTAMT(IND%) = IC_35HISTORY::COSTAMT(IND%) + &
		IC_TRANSACTION::COST

	UPDATE #IC_35HISTORY.CH%

	GOTO 1350

 AddBHist:
	IC_35HISTORY::PRODUCT	= IC_TRANSACTION::PRODUCT
	IC_35HISTORY::LOCATION	= IC_TRANSACTION::LOCATION
	IC_35HISTORY::CROSSREF	= IC_TRANSACTION::CROSS_REF
	IC_35HISTORY::SUBACCT	= IC_TRANSACTION::SUBACCOUNT
	IC_35HISTORY::TRANSTYPE	= IC_TRANSACTION::TYPE_B

	FOR I% = 0% TO 12%
		IC_35HISTORY::PQUANTITY(I%)	= 0.0
		IC_35HISTORY::PRICEAMT(I%)	= 0.0
		IC_35HISTORY::COSTAMT(I%)	= 0.0
	NEXT I%

	IC_35HISTORY::PQUANTITY(IND%) = IC_TRANSACTION::QUANTITY_B
	IC_35HISTORY::PRICEAMT(IND%)  = IC_TRANSACTION::PRICE
	IC_35HISTORY::COSTAMT(IND%)   = IC_TRANSACTION::COST

	PUT #IC_35HISTORY.CH%

1350	GOTO GetNextRec

1390	EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", YYYYPP$,  "") &
		IF EXIT_STATUS = CMC$_NORMAL

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
		UTL_REPORTX, "", "", CUR.PERIOD$, PERIOD$)

	GOTO ExitProgram

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), UTL_REPORTX, TITLE(), 0%)

	GOTO Aborted

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
