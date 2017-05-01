1	%TITLE "Query the Accounts Payable System"
	%SBTTL "AP_QURY_QUERY"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1987, 1988 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
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
	! ID:APQURY
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Query Accounts Payable File\* option
	!	makes on-line inquiries into selected vendor accounts in
	!	respect to both the open and the closed files.
	!	.lm -5
	!
	! Index:
	!	.x Query
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_QURY_QUERY/LINE
	!	$ LINK/EXECUTABLE=AP_EXE: AP_QURY_QUERY, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_QURY_QUERY.OBJ;*
	!
	! Author:
	!
	!	12/22/86 - Kevin Handy
	!
	! Modification history:
	!
	!	06/05/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	09/18/91 - Frank F. Starman
	!		Display company name on the border.
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/12/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Change SMG_QURY to SMG_QUERY%
	!
	!	08/28/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/09/97 - Kevin Handy
	!		Use OUTP_INITFORM function
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/17/99 - Kevin Handy
	!		Fix so that Discount/Due dates show up under the
	!		correct titles, and not reversed. (robison)
	!
	!	08/18/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	08/15/2005 - Kevin Handy
	!		Added a "Number of periods to print" option.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	!
	! Map file
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.HB"
	MAP (AP_CLOSE)		AP_CLOSE_CDD	AP_CLOSE

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP (AP_OPEN)		AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.HB"
	MAP (AP_CONTROL)	AP_CONTROL_CDD	AP_CONTROL

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	!
	! External functions
	!
	EXTERNAL LONG		FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	RESTORE_PROGRAM$ = SCOPE::PRG_PROGRAM

	REPORT$ = "APQURY"

300	!
	! Open Vendor File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

310	!
	! Open AP open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

320	!
	! Open AP close file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

330	!
	! Open AP control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CONTROL.OPN"
		GET #AP_CONTROL.CH%
		CLOSE AP_CONTROL.CH%
	USE
		FILENAME$ = "AP_CONTROL"
		CONTINUE HelpError
	END WHEN

500	GOSUB Initialize

900	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_QUERY%, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_QUERY%, &
		"AP Query for " + TRM$(SCOPE::PRG_COMPANY))

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, "Vendor #", 2%, 2%)

	GOSUB Repaint

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_QUERY%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	%PAGE

1000	!******************************************************************
	! Main option menu
	!******************************************************************

1100	!
	! Enter options
	!
	SCOPE::PRG_ITEM = ""
	SCOPE::PRG_PROGRAM = RESTORE_PROGRAM$

	OPTLIST$ = "Find Next Print Help eXit"
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, OPT%, 0%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Control c
	!
	CASE 3%
		GOTO 1000

	!
	! Exit key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	END SELECT

	SELECT OPT$

	!
	! Call the help message
	!
	CASE "H"
		CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, &
			"", "HELP")

	CASE "F"
1120		AP_VENDOR::VENNUM = ENTR_3STRING(SCOPE, SMG_QUERY%, "2;10", &
			"Vendor #", AP_VENDOR::VENNUM, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT
		!
		! List Choices
		!
		CASE SMG$K_TRM_F14
			SCOPE::PRG_ITEM = "FLD001"
			IF MAIN_WINDOW(AP_MAIN_VENDOR.ID, "VX") <> 1%
			THEN
				GOTO 1120
			ELSE
				GOTO 2000
			END IF

		CASE SMG$K_TRM_CTRLC
			GOTO 1120

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

2000		WHEN ERROR IN
			FIND #AP_VENDOR.CH%, &
				KEY #0% GE AP_VENDOR::VENNUM, &
				REGARDLESS
			GET #AP_VENDOR.CH%, REGARDLESS
		USE
			IF ERR = 155%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, "Vendor not found", 0%)
				CONTINUE 1000
			END IF

			FILENAME$ = "AP_VENDOR"
			CONTINUE HelpError
		END WHEN

		GOSUB Repaint

	CASE "N"
3000		WHEN ERROR IN
			GET #AP_VENDOR.CH%, REGARDLESS
		USE
			IF ERR = 11%
			THEN
				CALL ENTR_3MESSAGE(SCOPE, "End of file", 0%)
				CONTINUE 1000
			END IF

			FILENAME$ = "AP_VENDOR"
			CONTINUE HelpError
		END WHEN

		GOSUB Repaint

	CASE "P"
		CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

		GOSUB 4000

		GOTO 900

	CASE "X"
		GOTO ExitProgram

	END SELECT

	GOTO 1100

	!
	! Report printing routine
	!
4000	GOTO 1000 &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> &
		CMC$_NORMAL

	GOTO 4400 IF UTL_REPORTX::STAT

	!++
	! Abstract:FLD01
	!	^*(01) Periods To Display\*
	!	.p
	!	Determines how many periods worth of data should be used
	!	in the calculation of several fields in the summary at
	!	the top.
	!	A blank/zero entry will use all periods in the registers.
	!	.p
	!	This field is used by the "Average Days Collected"
	!	and the "Average Days Paid" fields in the summary.
	!
	! Index:
	!	.x Query>Periods
	!	.x Periods>Query
	!
	!--
	DETAIL_PERIODS% = VAL%(TRM$(UTL_REPORTX::OPTDEF(0%)))

	!
	! Detail period calculations
	!
	IF DETAIL_PERIODS% <> 0%
	THEN
		DETAIL_PERIOD% = VAL%(AP_CONTROL::YEAR) * 12 + &
			AP_CONTROL::LASTPERCLOSE - DETAIL_PERIODS% + 1%
		DETAIL_PERIOD% = 0% IF DETAIL_PERIOD% < 0%
		DETAIL_PERIOD$ = &
			FORMAT$(FIX(DETAIL_PERIOD% / 12%), "<0>###") + &
			FORMAT$(DETAIL_PERIOD% - (DETAIL_PERIOD% / 12%) * 12%, &
			"<0>#")
	ELSE
		DETAIL_PERIOD$ = "      "
	END IF


	TITLE$(1%) = "Accounts Payable Query"
	TITLE$(2%) = ""

	TITLE$(3%) = &
		"Tran    Invoice          Invoice              " + &
		"                         Discount Due      ---" + &
		"---------Check-------------      Balance"

	TITLE$(4%) = &
		"  #     Number           Date           Amount" + &
		"   Discount          Net Date     Date     Num" + &
		"ber     Date         Amount          Due"

	TITLE$(5%) = ""


	TEXT$ = AP_VENDOR::VENNUM + " " + &
		AP_VENDOR::VENNAM

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = ""

	TRANKEY$ = "ZZZZZZZZZZ"
	CHK_TEST%, FIRST_PASS% = 0%

	INV_AMT, DIS_AMT, NET_AMT, CHK_AMT, BAL_DUE = 0.0

	GOTO 4400 IF UTL_REPORTX::STAT

4100	WHEN ERROR IN
		FIND #AP_OPEN.CH%, &
			KEY #0% EQ AP_VENDOR::VENNUM, &
			REGARDLESS
	USE
		CONTINUE 4200 IF ERR = 155%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

4110	WHEN ERROR IN
		GET #AP_OPEN.CH%, REGARDLESS
	USE
		CONTINUE 4200 IF ERR = 11%
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO 4200 IF AP_VENDOR::VENNUM <> AP_OPEN::VENNUM

	IF FIRST_PASS%
	THEN
		IF AP_OPEN::TRANKEY = TRANKEY$
		THEN
			IF CHK_TEST% = 0% AND AP_OPEN::INVAMT = 0.0 AND &
				AP_OPEN::DISAMT = 0.0 AND AP_OPEN::CKAMT <> 0.0 &
				OR CHK_TEST% = 0% AND AP_OPEN::INVAMT = 0.0 AND &
				AP_OPEN::DISAMT = 0.0 AND AP_OPEN::CKNUM <> ""
			THEN
				GOTO 4190
			END IF
		ELSE
			TEXT$ = TEXT$ + SPACE$(120% - LEN(TEXT$))

			TEXT$ = TEXT$ + FORMAT$(BAL_DUE, "#########.##")
			BAL_DUE = 0.0
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO 4400 IF UTL_REPORTX::STAT

		CHK_TEST% = 0%
	END IF

	TEXT$ = ""

	IF AP_OPEN::TRANKEY = TRANKEY$ AND FIRST_PASS%
	THEN
		TEXT$ = SPACE$(34%)
	ELSE
		TEXT$ = AP_OPEN::TRANKEY + "  " + &
			AP_OPEN::INVNUM + "  " + &
			PRNT_DATE(AP_OPEN::INVDAT, 6%) + " "
	END IF

	IF AP_OPEN::INVAMT <> 0.0
	THEN
		TEXT$ = TEXT$ + FORMAT$(AP_OPEN::INVAMT, "#########.## ")
	ELSE
		TEXT$ = TEXT$ + SPACE$(LEN("#########.## "))
	END IF

	IF AP_OPEN::DISAMT <> 0.0
	THEN
		TEXT$ = TEXT$ + FORMAT$(AP_OPEN::DISAMT, "#######.## ")
	ELSE
		TEXT$ = TEXT$ + SPACE$(LEN("#######.## "))
	END IF

	NET = AP_OPEN::INVAMT - AP_OPEN::DISAMT
	IF NET <> 0.0
	THEN
		TEXT$ = TEXT$ + FORMAT$(NET, "#########.## ")
	ELSE
		TEXT$ = TEXT$ + SPACE$(LEN("#########.## "))
	END IF

	IF AP_OPEN::TRANKEY = TRANKEY$
	THEN
		TEXT$ = TEXT$ + SPACE$(18%)
	ELSE
		TEXT$ = TEXT$ + &
			PRNT_DATE(AP_OPEN::DISCDAT, 6%) + " " + &
			PRNT_DATE(AP_OPEN::DUEDAT, 6%) + " "
	END IF

4190	IF AP_OPEN::CKAMT <> 0.0 OR AP_OPEN::CKNUM <> ""
	THEN
		TEXT$ = TEXT$ + &
			AP_OPEN::CKNUM + "   " + &
			PRNT_DATE(AP_OPEN::CKDAT, 6%) + " " + &
			FORMAT$(AP_OPEN::CKAMT, "#########.## ")
		CHK_TEST% = -1%
	END IF

	FIRST_PASS% = -1%
	TRANKEY$ = AP_OPEN::TRANKEY
	INV_AMT = INV_AMT + AP_OPEN::INVAMT
	DIS_AMT = DIS_AMT + AP_OPEN::DISAMT
	NET_AMT = NET_AMT + AP_OPEN::INVAMT - AP_OPEN::DISAMT
	CHK_AMT = CHK_AMT + AP_OPEN::CKAMT

	BAL_DUE = BAL_DUE + &
		(AP_OPEN::INVAMT - AP_OPEN::DISAMT) - AP_OPEN::CKAMT

	GOTO 4110

4200	IF TEXT$ <> ""
	THEN
		TEXT$ = TEXT$ + SPACE$(120% - LEN(TEXT$))

		TEXT$ = TEXT$ + FORMAT$(BAL_DUE, "#########.## ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO 4400 IF UTL_REPORTX::STAT
	END IF

	TEMP$ = "     Vendor Open Total"

	TEXT$ = TEMP$ + SPACE$(34% - LEN(TEMP$)) + &
		FORMAT$(INV_AMT, "#########.## ") + &
		FORMAT$(DIS_AMT, "#######.## ") + &
		FORMAT$(NET_AMT, "#########.## ") + &
		SPACE$(36%) + &
		FORMAT$(CHK_AMT, "#########.## ") + &
		FORMAT$(NET_AMT - CHK_AMT, "#########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = ""

	GOTO 4400 IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	GOTO 4400 IF UTL_REPORTX::STAT

	TRANKEY$ = "ZZZZZZZZZZ"
	CHK_TEST%, FIRST_PASS% = 0%

	BAL_DUE = 0.0

	WHEN ERROR IN
		FIND #AP_CLOSE.CH%, &
			KEY #0% EQ AP_VENDOR::VENNUM, &
			REGARDLESS
	USE
		CONTINUE 4300
	END WHEN

4210	WHEN ERROR IN
		GET #AP_CLOSE.CH%, REGARDLESS
	USE
		CONTINUE 4300 IF ERR = 11%
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

	GOTO 4300 IF AP_VENDOR::VENNUM <> AP_CLOSE::VENNUM

	GOTO 4210 IF AP_CLOSE::UPDATED < DETAIL_PERIOD$

	IF FIRST_PASS%
	THEN
		IF AP_CLOSE::TRANKEY = TRANKEY$
		THEN
			IF CHK_TEST% = 0% AND AP_CLOSE::INVAMT = 0.0 AND &
				AP_CLOSE::DISAMT = 0.0 AND AP_CLOSE::CKAMT <> 0.0 &
				OR CHK_TEST% = 0% AND AP_CLOSE::INVAMT = 0.0 AND &
				AP_CLOSE::DISAMT = 0.0 AND AP_CLOSE::CKNUM <> ""
			THEN
				GOTO 4290
			END IF
		ELSE
			TEXT$ = TEXT$ + SPACE$(120% - LEN(TEXT$))

			TEXT$ = TEXT$ + &
				FORMAT$(BAL_DUE, "#########.##")
			BAL_DUE = 0.0
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO 4400 IF UTL_REPORTX::STAT

		CHK_TEST% = 0%
	END IF

	TEXT$ = ""

	IF AP_CLOSE::TRANKEY = TRANKEY$ AND FIRST_PASS%
	THEN
		TEXT$ = SPACE$(34%)
	ELSE
		TEXT$ = AP_CLOSE::TRANKEY + "  " + &
			AP_CLOSE::INVNUM + "  " + &
			PRNT_DATE(AP_CLOSE::INVDAT, 6%) + " "
	END IF

	IF AP_CLOSE::INVAMT <> 0.0
	THEN
		TEXT$ = TEXT$ + FORMAT$(AP_CLOSE::INVAMT, "#########.## ")
	ELSE
		TEXT$ = TEXT$ + SPACE$(LEN("#########.## "))
	END IF

	IF AP_CLOSE::DISAMT <> 0.0
	THEN
		TEXT$ = TEXT$ + FORMAT$(AP_CLOSE::DISAMT, "#######.## ")
	ELSE
		TEXT$ = TEXT$ + SPACE$(LEN("#######.## "))
	END IF

	NET = AP_CLOSE::INVAMT - AP_CLOSE::DISAMT
	IF NET <> 0.0
	THEN
		TEXT$ = TEXT$ + FORMAT$(NET, "#########.## ")
	ELSE
		TEXT$ = TEXT$ + SPACE$(LEN("#########.## "))
	END IF

	IF AP_CLOSE::TRANKEY = TRANKEY$
	THEN
		TEXT$ = TEXT$ + SPACE$(18%)
	ELSE
		TEXT$ = TEXT$ + &
			PRNT_DATE(AP_CLOSE::DISCDAT, 6%) + " " + &
			PRNT_DATE(AP_CLOSE::DUEDAT, 6%) + " "
	END IF

4290	IF AP_CLOSE::CKAMT <> 0.0 OR AP_CLOSE::CKNUM <> ""
	THEN
		TEXT$ = TEXT$ + &
			AP_CLOSE::CKNUM + "   " + &
			PRNT_DATE(AP_CLOSE::CKDAT, 6%) + " " + &
			FORMAT$(AP_CLOSE::CKAMT, "#########.## ")
		CHK_TEST% = -1%
	END IF

	FIRST_PASS% = -1%
	TRANKEY$ = AP_CLOSE::TRANKEY
	INV_AMT = INV_AMT + AP_CLOSE::INVAMT
	DIS_AMT = DIS_AMT + AP_CLOSE::DISAMT
	NET_AMT = NET_AMT + AP_CLOSE::INVAMT - AP_CLOSE::DISAMT
	CHK_AMT = CHK_AMT + AP_CLOSE::CKAMT

	BAL_DUE = BAL_DUE + &
		(AP_CLOSE::INVAMT - AP_CLOSE::DISAMT) - AP_CLOSE::CKAMT

	GOTO 4210

4300	IF TEXT$ <> ""
	THEN
		TEXT$ = TEXT$ + SPACE$(120% - LEN(TEXT$))

		TEXT$ = TEXT$ + FORMAT$(BAL_DUE, "#########.## ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO 4400 IF UTL_REPORTX::STAT
	END IF

	TEMP$ = "     Vendor Grand Total"

	TEXT$ = TEMP$ + SPACE$(34% - LEN(TEMP$)) + &
		FORMAT$(INV_AMT, "#########.## ") + &
		FORMAT$(DIS_AMT, "#######.## ") + &
		FORMAT$(NET_AMT, "#########.## ") + &
		SPACE$(36%) + &
		FORMAT$(CHK_AMT, "#########.## ") + &
		FORMAT$(NET_AMT - CHK_AMT, "#########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO 4400 IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

4400	!
	CALL OUTP_FINISH(UTL_REPORTX)

	RETURN

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 Repaint:
	!**************************************************************
	! Repaint vendor name
	!**************************************************************

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AP_VENDOR::VENNUM, 2%, 10%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AP_VENDOR::VENNAM, 6%, 3%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AP_VENDOR::ADD1, 7%, 3%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AP_VENDOR::ADD2, 8%, 3%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AP_VENDOR::CITY + " " + AP_VENDOR::STATE + " " + &
		AP_VENDOR::ZIP, 9%, 3%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AP_VENDOR::COUNTRY, 10%, 3%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PRNT_PHONE(AP_VENDOR::PHONE, 0%), 11%, 3%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AP_VENDOR::ALPSRT, 12%, 3%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AP_VENDOR::POADD1, 7%, 40%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AP_VENDOR::POADD2, 8%, 40%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AP_VENDOR::POCITY + " " + AP_VENDOR::POSTATE + " " + &
		AP_VENDOR::POZIP, 9%, 40%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		AP_VENDOR::POCOUNTRY, 10%, 40%,, SMG$M_BOLD)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
		PRNT_PHONE(AP_VENDOR::POPHONE, 0%), 11%, 40%,, SMG$M_BOLD)

	RETURN

	%Page

 Initialize:
	!*******************************************************************
	! Set Initialize values
	!*******************************************************************
	AP_VENDOR::VENNUM	= STRING$(10%, A"?"B)
	AP_VENDOR::VENNAM	= STRING$(40%, A"?"B)
	AP_VENDOR::ADD1		= STRING$(25%, A"?"B)
	AP_VENDOR::ADD2		= STRING$(21%, A"?"B)
	AP_VENDOR::CITY		= STRING$(15%, A"?"B)
	AP_VENDOR::STATE	= STRING$(2%, A"?"B)
	AP_VENDOR::COUNTRY	= STRING$(8%, A"?"B)
	AP_VENDOR::ZIP		= STRING$(10%, A"?"B)
	AP_VENDOR::PHONE	= STRING$(10%, A"?"B)

	AP_VENDOR::POADD1	= STRING$(25%, A"?"B)
	AP_VENDOR::POADD2	= STRING$(21%, A"?"B)
	AP_VENDOR::POCITY	= STRING$(15%, A"?"B)
	AP_VENDOR::POSTATE	= STRING$(2%, A"?"B)
	AP_VENDOR::POZIP	= STRING$(10%, A"?"B)
	AP_VENDOR::POCOUNTRY	= STRING$(8%, A"?"B)
	AP_VENDOR::POPHONE	= STRING$(10%, A"?"B)

	AP_VENDOR::ALPSRT	= STRING$(15%, A"?"B)

	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

	%PAGE

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

19999	END

20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"
	%INCLUDE "FUNC_INCLUDE:AP_WINDOW.INC"

	EXTERNAL LONG FUNCTION AP_MAIN_VENDOR

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE AP_MAIN_VENDOR.ID

		MAINT_GROUP = AP_MAIN_VENDOR(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
