1	%TITLE "Purchase Order Type List"
	%SBTTL "PO_RPRT_CALCORDER"
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
	! ID:PO004
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This report prints the information in the Purchase Order Load Journal.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_RPRT_CALCORDER/LINE
	!	$ LINK/EXE=PO_EXE: PO_RPRT_CALCORDER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_RPRT_CALCORDER.OBJ;*
	!
	! Author:
	!
	!	05/24/94 - Kevin Handy
	!
	! Modification History:
	!
	!	06/02/94 - Kevin Handy
	!		Added code to handle "1".."5"
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/09/96 - Kevin Handy
	!		Clean up (Check).
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/29/2000 - Kevin Handy
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
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PO.OPEN]PO_CALCORDER.HB"
	MAP (PO_CALCORDER) PO_CALCORDER_CDD PO_CALCORDER

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION) UTL_LOCATION_CDD UTL_LOCATION

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR) AP_VENDOR_CDD AP_VENDOR

	%INCLUDE "SOURCE:[PO.OPEN]PO_CONTROL.HB"
	MAP (PO_CONTROL)	PO_CONTROL_CDD		PO_CONTROL

	DECLARE STRING	TEXT, &
		FROM_ITEM, &
		TO_ITEM, &
		WLDCRD, &
		LYT_LINE

	%PAGE

	!
	! Give text description of formula parts
	!
	DEF FNFORM_TEXT$(X$)

		SELECT(X$)
		CASE "A"
			FNFORM_TEXT$ = "   Allocated"
		CASE "H"
			FNFORM_TEXT$ = "     On Hand"
		CASE "O"
			FNFORM_TEXT$ = "    On Order"
		CASE "S"
			FNFORM_TEXT$ = "      Safety"
		CASE "1"
			FNFORM_TEXT$ = "        Qtr1"
		CASE "2"
			FNFORM_TEXT$ = "        Qtr2"
		CASE "3"
			FNFORM_TEXT$ = "        Qtr3"
		CASE "4"
			FNFORM_TEXT$ = "        Qtr4"
		CASE "5"
			FNFORM_TEXT$ = "        Qtr5"
		CASE ELSE
			FNFORM_TEXT$ = "            "
		END SELECT

	FNEND

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Type\*
	!	.b
	!	.lm +5
	!	A ^*From Type\* field causes the
	!	printing to begin with a
	!	selected item.
	!	.b
	!	A blank setting will cause the report to begin with the first
	!	PO Type code in the file.
	!	.lm -5
	!
	! Index:
	!	.x From>PO Type
	!	.x PO Type>From
	!
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Type\*
	!	.b
	!	.lm +5
	!	A ^*To Type\* field causes the
	!	printing to end with the selected PO
	!	Type.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	PO Type code in the file.
	!	.lm -5
	!
	! Index:
	!	.x To>PO Type
	!	.x PO Type>To
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	PO Type codes to be printed by entering a
	!	"wildcard" value in this field.
	!	.lm -5
	!
	! Index:
	!	.x PO Type>Wildcard
	!	.x Wildcard>PO Type
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_CALCORDER.OPN"
	USE
		FILENAME$ = "PO_CALCORDER"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_CONTROL.OPN"
		GET #PO_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE #PO_CONTROL.CH%
	USE
		FILENAME$ = "PO_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Purchase  Order  Load  List"
	TITLE$(2%) = "Purchase Order System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TEXT = "Product             Order     Price"
	FOR I% = 1% TO 10%
		TEXT = TEXT + &
			FNFORM_TEXT$(MID(PO_CONTROL::LOAD_FORMULA, I%, 1%))
	NEXT I%

	TITLE$(4%) = TRM$(TEXT)
	TITLE$(5%) = "."

	!
	! Line layouts
	!
	LYT_LINE = "$POType:002,$TypeDescription:053"

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	THIS_LOCATION$ = "~~~~~~~~"
	THIS_VENDOR$ = ""

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #PO_CALCORDER.CH%
		ELSE
			FIND #PO_CALCORDER.CH%, KEY #0% GE FROM_ITEM, REGARDLESS
		END IF
	USE
		FILENAME$ = "PO_CALCORDER"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PO_CALCORDER.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PO_CALCORDER"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitProgram IF (PO_CALCORDER::LOCATION > TO_ITEM) AND TO_ITEM <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(PO_CALCORDER::LOCATION, -1%), WLDCRD) = 0% &
		AND WLDCRD <> ""

	!
	! Print new title if vendor/location changes
	!
	IF (THIS_LOCATION$ <> PO_CALCORDER::LOCATION) OR &
		(THIS_VENDOR$ <> PO_CALCORDER::VENDOR)
	THEN
		GOSUB DisplayNewVendor
	END IF

	!
	! Print out one line
	!
	TEXT = PO_CALCORDER::PRODUCT + " " + &
		FORMAT$(PO_CALCORDER::ORDER, "######.###") + &
		FORMAT$(PO_CALCORDER::COST, "#######.##")

	TEXT = TEXT + FORMAT$(PO_CALCORDER::QUANITY(I%), " <%>######.###") &
		FOR I% = 1% TO 10%

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

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

 DisplayNewVendor:
18000	!*******************************************************************
	! Display a new vendor
	!*******************************************************************

	IF (UTL_LOCATION::LOCATION <> PO_CALCORDER::LOCATION)
	THEN
		WHEN ERROR IN
			GET #UTL_LOCATION.CH%, &
				KEY #0% EQ PO_CALCORDER::LOCATION, &
				REGARDLESS
		USE
			UTL_LOCATION::LOCNAME = ""
			UTL_LOCATION::LOCATION = ""

			CONTINUE 18010
		END WHEN
	END IF

18010	IF (AP_VENDOR::VENNUM <> PO_CALCORDER::VENDOR)
	THEN
		WHEN ERROR IN
			GET #AP_VENDOR.CH%, &
				KEY #0% EQ PO_CALCORDER::VENDOR, &
				REGARDLESS
		USE
			AP_VENDOR::VENNUM = ""
			AP_VENDOR::VENNAM = ""

			CONTINUE 18020
		END WHEN
	END IF

18020	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 10%)

	TEXT = "Location: " + &
		PO_CALCORDER::LOCATION + "       " + &
		UTL_LOCATION::LOCNAME
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT, 0%)

	TEXT = "Vendor:   " + &
		PO_CALCORDER::VENDOR + " " + &
		AP_VENDOR::VENNAM
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT, 0%)

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 0%)

	THIS_LOCATION$ = PO_CALCORDER::LOCATION
	THIS_VENDOR$ = PO_CALCORDER::VENDOR

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

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
