1	%TITLE "Purchase Order Register Close/Purge"
	%SBTTL "PO_SPEC_PURGE"
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
	! ID:PO099
	!
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	The ^*Purchase Order Register Close/Purge\* Report will contain
	!	the following information about the orders that are
	!	closed and/or purged.
	!	.table 3,25
	!	.te
	!	PO Number	PO Type
	!	.te
	!	Vendor Number	Batch
	!	.te
	!	Vendor Name	Line Number
	!	.te
	!	Order Date	Location
	!	.te
	!	Product	Description
	!	.te
	!	Period          Account
	!	.te
	!	Sub Account	Quantity Ordered
	!	.te
	!	Quantity Shipped	Quantity Cancelled
	!	.end table
	!	^*Note:\* Purchase Orders will only be removed if they are
	!	closed and are older than the purge date.
	!	If there is no purge date entered, only completed
	!	orders (all items shipped or cancelled) will be reported.
	!	Each report is separate.
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_SPEC_PURGE/LINE
	!	$ LINK/EXE=PO_EXE: PO_SPEC_PURGE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_SPEC_PURGE.OBJ;*
	!
	! AUTHOR:
	!
	!	06/22/92 - Dan Perkins
	!		Modified from OE_SPEC_PURGE.
	!
	! MODIFICATION HISTORY:
	!
	!	07/01/92 - Kevin Handy
	!		Clean up (check)
	!
	!	12/07/92 - Dan Perkins
	!		Modify code in ExitTotal to only kill or rename files
	!		on CASE 2.  Make sure quantity received QTY(2%) equals
	!		quantity paid for QTY(9%) or don't purge.
	!		Use COMP_ARRAY instead of COMP_STRING.
	!
	!	12/09/92 - Kevin Handy
	!		Clean up (Check).
	!		Comment out PO_CONTROL stuff left there but not used
	!		for anything.
	!
	!	08/24/93 - Frank F. Starman
	!		Use FUNC_ROUND for testing balances.
	!
	!	03/10/94 - Kevin Handy
	!		Formatted to 80 columns.
	!
	!	03/15/94 - Kevin Handy
	!		Modifications so that when user gives a from/to
	!		value, it doesn't lose all records that are
	!		not within that range.
	!
	!	03/15/94 - Kevin Handy
	!		Modified so that report error goes to ExitProgram
	!		instead of ExitTotal
	!
	!	05/09/94 - Kevin Handy
	!		Removed commented out code.
	!
	!	05/09/94 - Kevin Handy
	!		Modified to also look at order date in the header.
	!
	!	05/09/94 - Kevin Handy
	!		Modified so indentation is more consistant.
	!
	!	05/10/94 - Kevin Handy
	!		Added error messages to report file.
	!
	!	05/11/94 - Kevin Handy
	!		Modified to print lines purged, not those flagged
	!		to close.
	!
	!	05/10/94 - Kevin Handy
	!		Modified so that from/to purge option really worked.
	!
	!	05/17/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/28/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...)
	!		in several places.
	!
	!	09/20/96 - Kevin Handy
	!		Clean up.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/09/99 - Kevin Handy
	!		Put LOC on AST setup
	!
	!	05/10/99 - Kevin Handy
	!		Lose purged records instead of stuffing them
	!		into an archive file, which nobody uses and there
	!		is no way to purge it.
	!
	!	09/18/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include scope.com
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	MAP (PO_REG_SUB_LINE)	PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	DECLARE			AP_VENDOR_CDD		AP_VENDOR_EXAM

	MAP (DP_OUTP_XUNSOL)				RRR_FLAG%

	!
	! Declare variables
	!
	DECLARE REAL QTY(22%)

	!
	! Declare external functions
	!
	EXTERNAL LONG     FUNCTION AP_EXAM_VENDOR
	EXTERNAL LONG     FUNCTION OUTP_UNSOLICITED

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
	!	.ts 55
	!	^*(01) Sort by	N,T,V,P,B\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field
	!	determines the
	!	sort order in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*N\* - Purchase Order Number
	!	.te
	!	^*T\* - Purchase Order Type
	!	.te
	!	^*V\* - Vendor Number
	!	.te
	!	^*P\* - Product Number
	!	.te
	!	^*B\* - Batch Number
	!	.end table
	!	A value is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field determines the
	!	item from which the report will begin printing.
	!	The value entered must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the report to
	!	end printing with a selected item.  The value entered
	!	must be in agreement with field (01) Sort by.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	using the Wildcarding Technique.
	!	.b
	!	For information on "Wildcarding" techniques refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	LOC_WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Location Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Location Wildcard\* field selects
	!	designated locations for which records will be closed or
	!	archived by entering a "wildcard" using the Wildcarding Technique.
	!	.lm -5
	!
	! Index:
	!	.x Location Wildcard
	!
	!--

	PURGE_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(5%), 132%))

	!++
	! Abstract:FLD06
	!	.ts 55
	!	^*(06) Archive Date	MMDDYYYY or MMDDYY\*
	!	.lm +5
	!	.b
	!	The ^*Archive Date\* field controls the removal
	!	of completed orders.  Only orders older than this date will be
	!	archived and only if they are complete.
	!	.b
	!	^*Note:\*  If no date is entered (blank), completed orders
	!	will be closed and a report will be generated, but no records
	!	will be archived.
	!	.lm -5
	!
	! Index:
	!
	!--

	!
	! Open RegHeader files
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.UPD"
	USE
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.NEW"
	USE
		FILENAME$ = "PO_REG_LINE_NEW"
		CONTINUE HelpError
	END WHEN

320 !	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.ARC"

	!
	! Open RegLine files
	!
350	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.UPD"
	USE
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

360	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.NEW"
	USE
		FILENAME$ = "PO_REG_SUB_LINE_NEW"
		CONTINUE HelpError
	END WHEN

370 !	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.ARC"

 ReportTitle:
	!
	! Title
	!
	IF PURGE_DATE$ = ""
	THEN
		TITLE_DATE$ = DATE_TODAY
	ELSE
		TITLE_DATE$ = PURGE_DATE$
	END IF

	!
	! Select which method to sort by
	!
	SELECT SORTBY$

	CASE "N"

		K_NUM% = 0%
		TITLE$(1%) = "CLOSED PURCHASE ORDERS AS OF " + &
			PRNT_DATE(TITLE_DATE$, 8%) + &
			" BY PO NUMBER"

		!
		! Routine to load left justified spaces into FROM_ITEM
		! and TO_ITEM if any order numbers are entered as ranges
		!
		FROM_ITEM$ = SPACE$(LEN(PO_REG_LINE::PO) - &
			LEN(FROM_ITEM$)) + &
			FROM_ITEM$ IF FROM_ITEM$ <> ""

		TO_ITEM$ = SPACE$(LEN(PO_REG_LINE::PO) - &
			LEN(TO_ITEM$)) + &
			TO_ITEM$ IF TO_ITEM$ <> ""

	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "CLOSED PURCHASE ORDERS AS OF " + &
			PRNT_DATE(TITLE_DATE$, 8%) + &
			" BY PO TYPE"

	CASE "V"
		K_NUM% = 2%
		TITLE$(1%) = "CLOSED PURCHASE ORDERS AS OF " + &
			PRNT_DATE(TITLE_DATE$, 8%) + &
			" BY VENDOR NUMBER"

	CASE "B"
		K_NUM% = 3%
		TITLE$(1%) = "CLOSED PURCHASE ORDERS AS OF " + &
			PRNT_DATE(TITLE_DATE$, 8%) + &
			" BY BATCH NUMBER"

	CASE "P"
		K_NUM% = 4%
		TITLE$(1%) = "CLOSED PURCHASE ORDERS AS OF " + &
			PRNT_DATE(TITLE_DATE$, 8%) + &
			" BY PRODUCT NUMBER"

	END SELECT

	TITLE$(2%) = "Purchase Order System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "PO_Number   Line Typ Vendor#    Vendor         " + &
		"      Loc  Product        Description          " + &
		"     UOM OrdDate  O/C Batch  Period"

	TITLE$(5%) = SPACE$(19%) + "Action   ActDate             Qty " + &
		"         Price   GL_Account           Subaccount   Batch"

	TITLE$(6%) = "."

	!
	! Initialize some variables
	!
	ARCHIVEFLAG% = 0%

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, &
		LOC(SCOPE::SMG_KBID) BY VALUE)

	PASSFLAG$ = "1"
	RRR_FLAG% = 0%
	TOTAL_PURGED% = 0%

	!
	! Output Start Time and such
	!
	TEXT$ = "Purchase Order Purge"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Begin Searching For Closed Lines"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Started on Date: " + DATE$(0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Started at Time: " + TIME$(0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

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
			RESET #PO_REG_LINE.CH%, KEY #K_NUM%
		ELSE
			FIND #PO_REG_LINE.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN


 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check for an interrupt
	!
	IF RRR_FLAG%
	THEN
		IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
		THEN
			TEXT$ = "%Abort Key Typed"
			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
			GOTO ExitProgram
		END IF
	END IF

	!
	! Get next Order Register record
	!
	WHEN ERROR IN
		GET #PO_REG_LINE.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be closed and printed
	!
	GOTO GetNextRec &
		IF COMP_ARRAY(EDIT$(PO_REG_LINE::FROMLOCATION, -1%), &
		LOC_WLDCRD$) = 0% IF LOC_WLDCRD$ <> ""

	SELECT SORTBY$

	CASE "N"
		GOTO ExitTotal IF (PO_REG_LINE::PO > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			PO_REG_LINE::PO, -1%), WLDCRD$) = 0% &
			IF WLDCRD$ <> ""

	CASE "T"
		GOTO ExitTotal IF (PO_REG_LINE::PO_TYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			PO_REG_LINE::PO_TYPE, -1%), WLDCRD$) = 0% &
			IF WLDCRD$ <> ""

	CASE "V"
		GOTO ExitTotal IF (PO_REG_LINE::VENDOR > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			PO_REG_LINE::VENDOR, -1%), WLDCRD$) = 0% &
			IF WLDCRD$ <> ""

	CASE "B"
		GOTO ExitTotal IF (PO_REG_LINE::BATCH > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			PO_REG_LINE::BATCH, -1%), WLDCRD$) = 0% &
			IF WLDCRD$ <> ""

	CASE "P"
		GOTO ExitTotal IF (PO_REG_LINE::PRODUCT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY(EDIT$( &
			PO_REG_LINE::PRODUCT, -1%), WLDCRD$) = 0% &
			IF WLDCRD$ <> ""

	END SELECT

	GOTO GetNextRec IF PO_REG_LINE::OPEN_CLOSE = "C"

	GOSUB EvaluatePurge

	GOTO GetNextRec IF PURGEFLAG$ <> "Y"

	!
	! Set status flag = C in header
	!
	PO_REG_LINE::OPEN_CLOSE = "C"

17100	WHEN ERROR IN
		UPDATE #PO_REG_LINE.CH%
	USE
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec

 ExitTotal:
	!*******************************************************************
	! Purge out records flagged with a "C"
	!*******************************************************************

17200	CALL ENTR_3MESSAGE(SCOPE, &
		"Archiving records...", 1% + 16%)

	PASSFLAG$ = "2"

	!
	! Output Start Time and such
	!
	TEXT$ = "Begin Purging Lines"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Started on Date: " + DATE$(0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Started at Time: " + TIME$(0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	!
	! If from item is blank then reset Register file
	! else try to find the first record
	!
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PO_REG_LINE.CH%, KEY #K_NUM%
		ELSE
			FIND #PO_REG_LINE.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

17210	WHEN ERROR IN
		GET #PO_REG_LINE.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 17400 IF ERR = 11%
		FILENAME$ = "PO_REG_LINE"
		CONTINUE HelpError
	END WHEN

	SELECT SORTBY$

	CASE "N"
		GOTO 17400 IF (PO_REG_LINE::PO > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO 17210 IF COMP_ARRAY(EDIT$( &
			PO_REG_LINE::PO, -1%), WLDCRD$) = 0% &
			IF WLDCRD$ <> ""

	CASE "T"
		GOTO 17400 IF (PO_REG_LINE::PO_TYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO 17210 IF COMP_ARRAY(EDIT$( &
			PO_REG_LINE::PO_TYPE, -1%), WLDCRD$) = 0% &
			IF WLDCRD$ <> ""

	CASE "V"
		GOTO 17400 IF (PO_REG_LINE::VENDOR > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO 17210 IF COMP_ARRAY(EDIT$( &
			PO_REG_LINE::VENDOR, -1%), WLDCRD$) = 0% &
			IF WLDCRD$ <> ""

	CASE "B"
		GOTO 17400 IF (PO_REG_LINE::BATCH > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO 17210 IF COMP_ARRAY(EDIT$( &
			PO_REG_LINE::BATCH, -1%), WLDCRD$) = 0% &
			IF WLDCRD$ <> ""

	CASE "P"
		GOTO 17400 IF (PO_REG_LINE::PRODUCT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO 17210 IF COMP_ARRAY(EDIT$( &
			PO_REG_LINE::PRODUCT, -1%), WLDCRD$) = 0% &
			IF WLDCRD$ <> ""

	END SELECT

	GOSUB EvaluatePurge

17220	IF PO_REG_LINE::OPEN_CLOSE <> "C" OR PURGE_DATE$ < CLOSE_DATE$
	THEN
		!
		! Keep in open file
		!
		ARCHIVEFLAG% = 0%
		WHEN ERROR IN
			PUT #PO_REG_LINE.CH_NEW%
		USE
			CONTINUE 17230 IF ERR = 134%
			FILENAME$ = "PO_REG_LINE"
			CONTINUE HelpError
		END WHEN
	ELSE
		!
		! Display line (header)
		!
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

		!
		! Read customer name
		!
		V% = AP_EXAM_VENDOR(PO_REG_LINE::VENDOR, AP_VENDOR_EXAM)

		TEXT$ = CONV_STRING(PO_REG_LINE::PO, CMC$_LEFT) + "  " + &
			PO_REG_LINE::PO_LINE + " " + &
			PO_REG_LINE::PO_TYPE + "  " + &
			PO_REG_LINE::VENDOR + " " + &
			LEFT(AP_VENDOR_EXAM::VENNAM, 20%) + " " + &
			PO_REG_LINE::FROMLOCATION + " " + &
			PO_REG_LINE::PRODUCT + " " + &
			LEFT(PO_REG_LINE::DESCRIPTION, 25%) + " " + &
			PO_REG_LINE::UOM + "  "  + &
			PRNT_DATE(PO_REG_LINE::ORDDATE, 6%) + " " + &
			PO_REG_LINE::OPEN_CLOSE + "   " + &
			PO_REG_LINE::BATCH + " " + &
			PO_REG_LINE::PERIOD

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		!
		! Add to archive
		!
		ARCHIVEFLAG% = -1%
 !		PUT #PO_REG_LINE.CH_ARC%
		TOTAL_PURGED% = TOTAL_PURGED% + 1%
	END IF

17230	GOSUB Readline

	GOTO 17210

17400	!
	! Output Start Time and such
	!
	TEXT$ = "Finished Purging"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "  Ended on Date: " + DATE$(0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "  Ended at Time: " + TIME$(0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "   Lines Purged: " + NUM1$(TOTAL_PURGED%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	!
	! Swap PO files around
	!
	CLOSE PO_REG_LINE.CH%
	CLOSE PO_REG_LINE.CH_NEW%
 !	CLOSE PO_REG_LINE.CH_ARC%

	CLOSE PO_REG_SUB_LINE.CH%
	CLOSE PO_REG_SUB_LINE.CH_NEW%
 !	CLOSE PO_REG_SUB_LINE.CH_ARC%

17410 !	KILL PO_REG_LINE.NAME$ FOR I% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(PO_REG_LINE.NAME$ + ";*")

17420 !	KILL PO_REG_SUB_LINE.NAME$ FOR I% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(PO_REG_SUB_LINE.NAME$ + ";*")

17430	WHEN ERROR IN
		NAME PO_REG_LINE.NAME_NEW$ AS PO_REG_LINE.NAME$
	USE
		CONTINUE 17440
	END WHEN

17440	NAME PO_REG_SUB_LINE.NAME_NEW$ AS PO_REG_SUB_LINE.NAME$

17450 !	KILL PO_REG_LINE.NAME_NEW$ FOR I% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(PO_REG_LINE.NAME_NEW$ + ";*")

17460 !	KILL PO_REG_SUB_LINE.NAME_NEW$ FOR I% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(PO_REG_SUB_LINE.NAME_NEW$ + ";*")

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

 ReadLine:
	!*******************************************************************
	! Process Line File
	!*******************************************************************

17500	WHEN ERROR IN
		FIND #PO_REG_SUB_LINE.CH%, &
			KEY #0% EQ PO_REG_LINE::PO + PO_REG_LINE::PO_LINE, &
			REGARDLESS
	USE
		CONTINUE ExitReadLine IF ERR = 155%
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

17510	WHEN ERROR IN
		GET #PO_REG_SUB_LINE.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE ExitReadLine IF ERR = 11%
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN


	GOTO ExitReadLine IF PO_REG_SUB_LINE::PO <> PO_REG_LINE::PO OR &
		PO_REG_SUB_LINE::PO_LINE <> PO_REG_LINE::PO_LINE

	SELECT PASSFLAG$

 !	CASE "1"

17600	CASE "2"
		IF ARCHIVEFLAG%
		THEN
			!
			! Print line
			!
			TEXT$ = CONV_STRING(PO_REG_LINE::PO, CMC$_LEFT) + "  " + &
				PO_REG_SUB_LINE::PO_LINE + "   " + &
				PO_REG_SUB_LINE::PO_ACTION + " " + &
				PRNT_DATE(PO_REG_SUB_LINE::ACTION_DATE, 6%) + "   " + &
				FORMAT$(PO_REG_SUB_LINE::QTY, "#,###,###.##") + "   " + &
				FORMAT$(PO_REG_SUB_LINE::PRICE, "#,###,###.##") + "   " + &
				PO_REG_SUB_LINE::ACCOUNT + "   " + &
				PO_REG_SUB_LINE::SUBACCT + "   " + &
				PO_REG_SUB_LINE::BATCH

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 !			PUT #PO_REG_SUB_LINE.CH_ARC%
		ELSE
			WHEN ERROR IN
				PUT #PO_REG_SUB_LINE.CH_NEW%
			USE
				IF ARCHIVEFALG%
				THEN
					CONTINUE 17510 IF ERR = 134%
					FILENAME$ = "PO_REG_SUB_LINE_ARC"
				ELSE
					FILENAME$ = "PO_REG_SUB_LINE_NEW"
				END IF

				CONTINUE HelpError
			END WHEN
		END IF

	END SELECT

	!
	! Go for next line
	!
	GOTO 17510

 ExitReadLine:
	RETURN

	%PAGE

 EvaluatePurge:
18000	!
	! Here's where we evaluate if removal of order is to take place
	!
	PURGEFLAG$ = ""
	CLOSE_DATE$ = PO_REG_LINE::ORDDATE
	QTY(I%) = 0.0 FOR I% = 1% TO 9%

	WHEN ERROR IN
		FIND #PO_REG_SUB_LINE.CH%, &
			KEY #0% EQ PO_REG_LINE::PO + PO_REG_LINE::PO_LINE, &
			REGARDLESS
	USE
		CONTINUE 18100 IF ERR = 155%
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

18020	WHEN ERROR IN
		GET #PO_REG_SUB_LINE.CH%, REGARDLESS
	USE
		CONTINUE 18090 IF ERR = 11%	! End of file
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

	GOTO 18090 IF PO_REG_SUB_LINE::PO <> PO_REG_LINE::PO OR &
		PO_REG_SUB_LINE::PO_LINE <> PO_REG_LINE::PO_LINE

	WHEN ERROR IN
		IND% = VAL%(PO_REG_SUB_LINE::PO_ACTION)
	USE
		CONTINUE 18100 IF ERR = 52%	! Wrong data format
		FILENAME$ = "PO_REG_SUB_LINE"
		CONTINUE HelpError
	END WHEN

	QTY(IND%) = QTY(IND%) + PO_REG_SUB_LINE::QTY

	CLOSE_DATE$ = PO_REG_SUB_LINE::ACTION_DATE &
		IF PO_REG_SUB_LINE::ACTION_DATE > CLOSE_DATE$

	GOTO 18020

18090	IF FUNC_ROUND((QTY(1%) - QTY(2%) - QTY(3%)), 2%) <= 0.0
	THEN
		IF FUNC_ROUND(QTY(2%) - QTY(9%), 2%) = 0.0
		THEN
			PURGEFLAG$ = "Y"
		END IF
	END IF

18100	RETURN

	%PAGE

 HelpError:
	!***************************************************************
	! Help Message for an error
	!***************************************************************

	TEXT$ = "Error occured"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "        On Date: " + DATE$(0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "        At Time: " + TIME$(0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = NUM1$(ERL) + " " + NUM1$(ERR) + " " + TRM$(ERT$(ERR)) + " " + &
		ERN$ + " " + FILENAME$
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

	%PAGE

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Resume to display untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
