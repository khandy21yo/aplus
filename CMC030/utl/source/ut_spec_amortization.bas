1	%TITLE "Amortization Schedule"
	%SBTTL "UT_SPEC_AMORTIZATION"
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program will calculate and print (display) the amortization
	!	schedule.  There are four items which can be calculated:
	!	.table 3,25
	!	.te
	!	^*Present Value
	!	.te
	!	Interest Rate
	!	.te
	!	Number or Periods
	!	.te
	!	Payment per Period\*
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Present Value
	!	.x Interest Rate
	!	.x Payment per Period
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UT_SPEC_AMORTIZATION/LINE
	!	$ LINK/EXE=UTL_EXE: UT_SPEC_AMORTIZATION, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UT_SPEC_AMORTIZATION.OBJ;*
	!
	! Author:
	!
	!	05/13/88 - Frank Starman
	!
	! Modification history:
	!
	!	03/27/90 - Frank Starman
	!		Change name from AT.. to UT...
	!		Change key for help messages.
	!
	!	04/20/90 - Frank Starman
	!		Add help messages.
	!
	!	02/13/91 - Kevin Handy
	!		Modified lookup into FLAGTYPE$() at 10300 so that
	!		if doesn't assume that all types (01)-(06) are
	!		in the appropriate places in the array.
	!
	!	05/08/91 - Frank F. Starman
	!		Fix cases for calculation. Was 01,02,03,04 now
	!		02,03,05,06.
	!
	!	03/13/92 - Kevin Handy
	!		Unrolled error trap (check)
	!
	!	09/28/92 - Kevin Handy
	!		Added error trap for 10350 so program won't die
	!		just because some fileds are not filled in.
	!
	!	06/18/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	08/04/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	09/18/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/08/97 - Kevin Handy
	!		Use OUTP_INITFORM function.
	!
	!	08/25/97 - Kevin Handy
	!		Lose definition for FUNC_FILEEXISTS, which isnt called
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	05/21/98 - Kevin Handy
	!		Increase the number of displayed digits by one
	!		so that 100 Million would work.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Lose FNRAIN and FNRIN (Dead code)
	!
	!	11/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!++
	! Abstract:COMMAND
	!	^*AMORTIZATION\*
	!	.b
	!	.lm +5
	!	The ^*Amortization\* option calculates and prints
	!	an amortization schedule. There are four items which can be calculated:
	!	.table 3,25
	!	.te
	!	^*Present Value
	!	.te
	!	Interest Rate
	!	.te
	!	Number or Periods
	!	.te
	!	Payment per Period\*
	!	.end table
	!	^*Format: AMORTIZATION\*
	!	.b
	!	^*Example:\*
	!	.literal
	!	Menu Command Level> /AMORTIZATION
	!	.END LITERAL
	!	.lm -5
	!
	! Index:
	!	.x Present Value
	!	.x Interest Rate
	!	.x Payment per Period
	!
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map file
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	COM (TT_FLAG_OPTION) &
		FLAGTITLE$ = 20%, &
		FLAGTYPE$(4%) = 20%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	!
	! Declare constants
	!
	DECLARE INTEGER CONSTANT MAX_ITEM = 8%
	DECLARE LONG XLONG, YLONG, SMG_VIEW

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize query
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$ = "UT019"

500	GOSUB SetInitial

	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_VIEW, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_VIEW, &
		"Amortization schedule for " + EDIT$(SCOPE::PRG_COMPANY, 140%))

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_VIEW, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	%PAGE

1000	!******************************************************************
	! Main option menu
	!******************************************************************

	GOSUB Repaint

1100	!
	! Enter options
	!
	SCOPE::PRG_ITEM = ""
	OPTLIST$ = "Add Change Print Help eXit"
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

	CASE "A"
		!*****************************************************
		! Add new information on the screen
		!*****************************************************

		GOSUB SetInitial
		FLAG% = 1%
		GOSUB DataEntry FOR LOOP% = 1% TO MAX_ITEM

		FOR LOOP% = 1% TO MAX_ITEM
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
			GOTO 1100

		END SELECT

 Add:		FLAG% = 0%
		GOSUB DataEntry

		SELECT SCOPE::SCOPE_EXIT

		!
		! Control c
		!
		CASE 3%
			GOTO 1000

		!
		! Uparrow
		!
		CASE SMG$K_TRM_UP
			LOOP% = LOOP% - 1% IF LOOP% > 1%
			GOTO Add

		!
		! SMG$K_TRM_DOWN
		!
		CASE SMG$K_TRM_DOWN
			LOOP% = LOOP% + 1% IF LOOP% < MAX_ITEM
			GOTO Add

		!
		! Exit key
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1100

		END SELECT

		NEXT LOOP%

		GOSUB Calcul

	CASE "C"
 Changer:
		!*****************************************************
		! Change information on the screen
		!*****************************************************

		LOOP% = ENTR_3NUMBER(SCOPE, &
			SCOPE::SMG_OPTION, "", "Item to change", &
			0.0, 4%, "##", "")

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
			GOTO 1100

		END SELECT

		IF LOOP% = 0%
		THEN
			GOSUB Calcul
			GOTO 1100
		END IF

		GOTO Changer IF LOOP% < 1% OR LOOP% > MAX_ITEM

 Changer1:	FLAG% = 0%
		GOSUB DataEntry

		SELECT SCOPE::SCOPE_EXIT

		!
		! Control c
		!
		CASE 3%
			GOTO 1000

		!
		! Uparrow
		!
		CASE SMG$K_TRM_UP
			LOOP% = LOOP% - 1% IF LOOP% > 1%
			GOTO Changer1

		!
		! SMG$K_TRM_DOWN
		!
		CASE SMG$K_TRM_DOWN
			LOOP% = LOOP% + 1% IF LOOP% < MAX_ITEM
			GOTO Changer1

		!
		! Exit key
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1100

		END SELECT

		GOTO Changer

	CASE "P"
		!*****************************************************
		! Print
		!*****************************************************

		CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

3900		!******************************************************************
		! Set up the report settings screen
		!******************************************************************

		GOTO 1000 &
			IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> &
			CMC$_NORMAL

		GOTO 4500 IF UTL_REPORTX::STAT

		!
		! Title
		!
		TITLE$(1%) = "AMORTIZATION  SCHEDULE"
		TITLE$(2%) = ""
		TITLE$(3%) = "."

		TEXT$ = "Amount of Loan($):" + &
			FORMAT$(AMO_ITEM, "###,###,###.##") + &
			SPACE$(24%) + &
			"Period per Year : " + &
			FORMAT$(PY_ITEM%, "###")
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "Future Value ($) :" + &
			FORMAT$(FAMO, "###,###,###.##") + &
			SPACE$(24%) + &
			"Total Periods   : " + &
			FORMAT$(TP_ITEM%, "###")
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "Period Payment($):" + &
			FORMAT$(PAMO_ITEM, "###,###,###.##") + &
			SPACE$(24%) + &
			"Interest Rate(%): " + &
			FORMAT$(RA_ITEM, " ##.##")
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		GOTO 4500 IF UTL_REPORTX::STAT
		TITLE$(3%) = "Date            Interest     Principal" + &
			"   BalanceLoan   InterestPTD  PrincipalPTD"

		TITLE$(4%) = "."

		TEXT$ = TITLE$(3%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		N_DATE$ = DATE_ITEM$
		IID, PD = 0.0

		IF TRM$(DATE_ITEM$) = ""
		THEN
			FOR J% = 0% TO TP_ITEM% - 1%
				GOSUB RepSub
				TEXT$ = FORMAT$(J% + 1%, "##########") + &
					FORMAT$(IPP, "###,###,###.##") + &
					FORMAT$(PPP, "###,###,###.##") + &
					FORMAT$(LPP, "###,###,###.##") + &
					FORMAT$(IID, "###,###,###.##") + &
					FORMAT$(PD, "###,###,###.##")
				CALL OUTP_LINE("", UTL_REPORTX, &
					TITLE$(), TEXT$, 0%)
				GOTO 4500 IF UTL_REPORTX::STAT
			NEXT J%
		GOTO 4500
		END IF

		IF INT(12.0 / PY_ITEM%) = 12.0 / PY_ITEM% AND PY_ITEM% <> 0%
		THEN
			NM% = 12% / PY_ITEM%
			DA% = VAL%(RIGHT(N_DATE$, 7%))
			MO% = VAL%(MID(N_DATE$, 5%, 2%))
			RO% = VAL%(LEFT(N_DATE$, 4%))
			FOR J% = 0% TO TP_ITEM% - 1%
				GOSUB RepSub
				TEXT$ = PRNT_DATE(N_DATE$, 8%) + &
					FORMAT$(IPP, "###,###,###.##") + &
					FORMAT$(PPP, "###,###,###.##") + &
					FORMAT$(LPP, "###,###,###.##") + &
					FORMAT$(IID, "###,###,###.##") + &
					FORMAT$(PD, "###,###,###.##")
				CALL OUTP_LINE("", UTL_REPORTX, &
					TITLE$(), TEXT$, 0%)
				GOTO 4500 IF UTL_REPORTX::STAT
				RO% = RO% + 1% IF MO% + NM% > 12%
				MO% = MO% + NM% - INT((MO% + NM% - 1%) / 12%) * 12%
				N_DATE$, TEST_DATE$ = FORMAT$(RO%, "<0>###") + &
					FORMAT$(MO%, "<0>#") + &
					FORMAT$(DA%, "<0>#")

				TEST% = 0%
				WHILE DATE_INVDCODE(DATE_DAYCODE(N_DATE$)) <> N_DATE$ OR &
					VAL%(MID(N_DATE$, 5%, 2%)) <> MO%
					TEST% = TEST% + 1%
					N_DATE$ = &
					DATE_INVDCODE(DATE_DAYCODE(TEST_DATE$) - TEST%)
				NEXT

				GOTO 4500 IF UTL_REPORTX::STAT
			NEXT J%
		GOTO 4500
		END IF

		IF PY_ITEM% = 52%
		THEN
			CD% = DATE_DAYCODE(DATE_ITEM$)
			FOR J% = 0% TO TP_ITEM% - 1%
				GOSUB RepSub
				TEXT$ = PRNT_DATE(DATE_INVDCODE(CD% + 7% * J%), &
					8%) + &
					FORMAT$(IPP, "###,###,###.##") + &
					FORMAT$(PPP, "###,###,###.##") + &
					FORMAT$(LPP, "###,###,###.##") + &
					FORMAT$(IID, "###,###,###.##") + &
					FORMAT$(PD, "###,###,###.##")
				CALL OUTP_LINE("", UTL_REPORTX, &
					TITLE$(), TEXT$, 0%)
				GOTO 4500 IF UTL_REPORTX::STAT
			NEXT J%
		GOTO 4500
		END IF

		INTRV = 0.0
		INTRV = 366.0 / PY_ITEM% IF PY_ITEM% <> 0%
		RO% = VAL%(LEFT(DATE_ITEM$, 4%))
		J% = 0%
 NextYear:
		CD% = DATE_DAYCODE(N_DATE$)

		FOR I% = 0% TO PY_ITEM% - 1%
			IN% = I% * INTRV

			GOSUB RepSub

			TEXT$ = PRNT_DATE(DATE_INVDCODE(CD% + IN%), 8%) + &
				FORMAT$(IPP, "###,###,###.##") + &
				FORMAT$(PPP, "###,###,###.##") + &
				FORMAT$(LPP, "###,###,###.##") + &
				FORMAT$(IID, "###,###,###.##") + &
				FORMAT$(PD, "###,###,###.##")

			CALL OUTP_LINE("", UTL_REPORTX, &
				TITLE$(), TEXT$, 0%)

			J% = J% + 1%

			GOTO 4500 IF J% = TP_ITEM%
		NEXT I%

		RO% = RO% + 1%
		N_DATE$ = FORMAT$(RO%, "####") + RIGHT(N_DATE$, 5%)
		GOTO NextYear

4500		!
		CALL OUTP_FINISH(UTL_REPORTX)

		GOTO 1000

	CASE "H"
		CALL HELP_34MESSAGE(SCOPE, "", "H", SCOPE::PRG_PROGRAM, &
			"", "HELP")
		GOTO 1000

	CASE "X"
		GOTO ExitProgram

	END SELECT

	GOTO 1100

	%PAGE

 ExitProgram:
10000	!******************************************************************
	! Exit the program
	!******************************************************************

	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_VIEW)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 SetInitial:
10100	!******************************************************************
	! Set initial value
	!******************************************************************

	FLAGTITLE$ = "Item   Description"
	FLAGTYPE$(0%) = "04"
	FLAGTYPE$(1%) = "02   Present Value"
	FLAGTYPE$(2%) = "03   Interest  Rate"
	FLAGTYPE$(3%) = "05   Total Periods"
	FLAGTYPE$(4%) = "06   Pay Per Period"

	OPTION_ITEM$ = "06"
	AMO_ITEM = 0.0
	RA_ITEM = 0.0
	PY_ITEM% = 0%
	TP_ITEM% = 0%
	PAMO_ITEM = 0.0
	J_ITEM% = 0.0
	DATE_ITEM$ = "        "

	RETURN

 Repaint:
10200	!******************************************************************
	! Repaint the screen
	!******************************************************************

	DATA	02, 03, "(01) Field  to Calc", &
		03, 03, "(02) Amount of Loan", &
		06, 03, "(03) Interest Rate", &
		09, 03, "(04) Periods Per Year", &
		12, 03, "(05) Total Periods", &
		06, 43, "(06) Payment per Period", &
		09, 43, "(07) Date of First Payment", &
		12, 43, "(08) Payment No.", &
		16, 35, "Interest      Principal    Loan Balance", &
		0, 0, ""

	RESTORE
	READ XLONG, YLONG, ATEXT$

	WHILE XLONG
		SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW, ATEXT$, XLONG, YLONG)
		READ XLONG, YLONG, ATEXT$
	NEXT

	FLAG% = 1%
	GOSUB DataEntry FOR LOOP% = 1% TO MAX_ITEM

	RETURN

 DataEntry:
10300	!******************************************************************
	! Enter/Diaplay items
	!******************************************************************

	TEMP$ = TRM$(SCOPE::PRG_ITEM)

	SCOPE::PRG_ITEM = "FLD" + FORMAT$(LOOP%, "<0>##")

	SELECT LOOP%

	CASE 1%
	!++
	! Abstract:FLD001
	!	.x Field
	!	^*(01) Field\*
	!	.b
	!	.lm +5
	!	The ^*Field\* enters one of four
	!	codes indicating which item is to be calculated.
	!	.b
	!	Valid codes may be displayed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--

		OPTION_ITEM$ = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_VIEW, &
			"2;15", "Option ", OPTION_ITEM$, &
			FLAG%, "'E", DEFLT$, FLAGTYPE$(), &
			FLAGTITLE$, "007"), -1%)

		!**** Fix? ****
		J% = 0%
		J% = I% IF LEFT(FLAGTYPE$(I%), 2%) = OPTION_ITEM$ &
			FOR I% = 1% TO 4%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW, &
			RIGHT(FLAGTYPE$(J%), 3%), &
			2%, 18%,, SMG$M_BOLD)

	CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Amount of Loan\*
	!	.b
	!	.lm +5
	!	The ^*Amount of Loan\* field enters
	!	the amount of an obligation.
	!	.b
	!	This field will accommodate a number as large as 99,999,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Amount of Loan
	!
	!--

		AMO_ITEM = ENTR_3NUMBER(SCOPE, SMG_VIEW, "4;7", "Loan ", &
			AMO_ITEM, FLAG%, "###,###,###.##", DEFLT$)

	CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Interest Rate\*
	!	.b
	!	.lm +5
	!	The ^*Interest Rate\* field enters
	!	a periodic interest rate.
	!	.b
	!	If the periodic interest rate is to be the same as
	!	an effective interest rate, there must be only one period
	!	per year.
	!	.lm -5
	!
	! Index:
	!	.x Interest Rate
	!	.x Interest>Periodic
	!	.x Periodic Interest
	!	.x Interest>Effective
	!	.x Effective Interest
	!
	!--

		RA_ITEM = ENTR_3NUMBER(SCOPE, SMG_VIEW, "7;13", "Rate ", &
			RA_ITEM, FLAG%, "##.##", DEFLT$)

	CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Period per Year\*
	!	.b
	!	.lm +5
	!	The ^*Period per Year\* field enters
	!	the number of periods there will be per year.
	!	.b
	!	For example, if the present value will be compounded monthly,
	!	enter 12 periods per year.
	!	.lm -5
	!
	! Index:
	!	.x Period
	!
	!--

		PY_ITEM% = ENTR_3NUMBER(SCOPE, SMG_VIEW, "10;14", "Periods ", &
			PY_ITEM% * 1.0, FLAG%, "###", DEFLT$)

	CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Total Periods\*
	!	.b
	!	.lm +5
	!	The ^*Total Periods\* field enters
	!	the number of periods needed to pay a loan.
	!	.lm -5
	!
	! Index:
	!	.x Total Periods>Amortization
	!	.x Amortization>Total Periods
	!	.x Periods>Total
	!
	!--
		TP_ITEM% = ENTR_3NUMBER(SCOPE, SMG_VIEW, "13;14", "Periods ", &
			TP_ITEM% * 1.0, FLAG%, "###", DEFLT$)

	CASE 6%
	!++
	! Abstract:FLD006
	!	^*(06) Payment per Period\*
	!	.b
	!	.lm +5
	!	The ^*Payment per Period\* field enters
	!	the amount required to be paid each period.
	!	.lm -5
	!
	! Index:
	!	.x Payment
	!
	!--
		PAMO_ITEM = ENTR_3NUMBER(SCOPE, SMG_VIEW, "7;47", "Payment ", &
			PAMO_ITEM, FLAG%, "###,###,###.##", DEFLT$)

	CASE 7%
	!++
	! Abstract:FLD007
	!	.x Starting Date
	!	^*(07) Date of the First Payment\*
	!	.b
	!	.lm +5
	!	The ^*Date of the First Payment\* field
	!	enters the date the first payment is due.
	!	.lm -5
	!
	! Index:
	!
	!--
		DATE_ITEM$ = ENTR_3DATE(SCOPE, SMG_VIEW, &
			"10;50", "From Date", DATE_ITEM$, FLAG%, "'E", &
			DEFLT$)

	CASE 8%
	!++
	! Abstract:FLD008
	!	^*(08) Payment Number\*
	!	.b
	!	.lm +5
	!	The ^*Payment Number\* field
	!	enters the payment which is displayed on the
	!	screen.
	!	.lm -5
	!
	! Index:
	!	.x Payment Number>Amortization
	!	.x Number>Payment
	!	.x Amortization>Payment Number
	!
	!--

		J_ITEM% = ENTR_3NUMBER(SCOPE, SMG_VIEW, "13;54", "Number ", &
			J_ITEM% * 1.0, FLAG%, "###", DEFLT$)

	END SELECT

	SCOPE::PRG_ITEM = TEMP$

	RETURN

 Calcul:
10350	FLAG% = 1%

	!
	! Ignore errors (usually caused by fields not filled in)
	!
	WHEN ERROR IN
		SELECT OPTION_ITEM$

		CASE "02"
			AMO_ITEM = PAMO_ITEM * FNANI(TP_ITEM%, &
				0.01 * RA_ITEM / PY_ITEM%) &
				IF PY_ITEM% <> 0%
			LOOP% = 2%
			GOSUB DataEntry

		CASE "03"
			RA_ITEM = 0.0008
			DELTA = 0.0008
			DELTA = PAMO_ITEM / AMO_ITEM * &
				((1.0 + RA_ITEM) ^ TP_ITEM% - 1.0) / &
				(1.0 + RA_ITEM) ^ TP_ITEM% IF AMO_ITEM <> 0.0
			WHILE ABS(RA_ITEM - DELTA) > 0.000001
				RA_ITEM = DELTA
				DELTA = PAMO_ITEM / AMO_ITEM * &
					((1.0 + RA_ITEM) ^ TP_ITEM% - 1.0) / &
					(1.0 + RA_ITEM) ^ TP_ITEM%
			NEXT
			RA_ITEM = DELTA * PY_ITEM% * 100.0
			LOOP% = 3%
			GOSUB DataEntry

		CASE "05"
			TP_ITEM% = -LOG(1% - AMO_ITEM * &
				RA_ITEM * 0.01 / PY_ITEM% / PAMO_ITEM) / &
				LOG(1% + RA_ITEM * 0.01 / PY_ITEM%) + 1% &
				IF PY_ITEM% <> 0%
			LOOP% = 5%
			GOSUB DataEntry
			RES = 0.0
			RES = AMO_ITEM * (1% + RA_ITEM * 0.01 / PY_ITEM%) ^ &
				TP_ITEM% - &
				PAMO_ITEM * FNNI(TP_ITEM% - 1%, &
				RA_ITEM * 0.01 / PY_ITEM%) * &
				(1% + RA_ITEM * 0.01 / PY_ITEM%) &
				IF PY_ITEM% <> 0%
			RES$ = FORMAT$(RES, "$###,###,###.##")
			SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW, RES$, 14%, 3%)
			SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW, &
				"( Last Payment )", 15%, 3%)

		CASE "06"
			PAMO_ITEM = AMO_ITEM / FNANI(TP_ITEM%, &
				RA_ITEM * 0.01 / PY_ITEM%) &
				IF PY_ITEM% <> 0%
			LOOP% = 6%
			GOSUB DataEntry

		END SELECT
	USE
		CONTINUE 10360
	END WHEN

10360	J% = J_ITEM%
	J% = J% - 1%

	GOSUB RepSub

	TEXT$ = FORMAT$(IPP, "##,###,###.##") + "  " + &
		FORMAT$(PPP, "##,###,###.##") + "  " + &
		FORMAT$(LPP, "###,###,###.##")

	SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW, TEXT$, 17%, 31%,, SMG$M_BOLD)

	RETURN

	%PAGE

 RepSub:
10400	IF RA_ITEM <> 0.0
	THEN
		IF J% = -1%
		THEN
			IPP, PPP =0.0
			LPP = AMO_ITEM
			GOTO RetRepSub
		END IF

		IPP = (FUNC_ROUND(AMO_ITEM, 2%) * &
			(1% + RA_ITEM * 0.01 / PY_ITEM%) ^ J% - &
			FUNC_ROUND(PAMO_ITEM, 2%) * &
			FNNI(J%, RA_ITEM * 0.01 / PY_ITEM%)) * &
			RA_ITEM * 0.01 / PY_ITEM%

		PPP = FUNC_ROUND(PAMO_ITEM, 2%) - FUNC_ROUND(IPP, 2%)

		LPP = AMO_ITEM * (1% + RA_ITEM * 0.01 / PY_ITEM%) ^ J% - &
			FUNC_ROUND(PAMO_ITEM, 2%) * &
			FNNI(J%, RA_ITEM * 0.01 / PY_ITEM%) - PPP

		IID = IID + IPP

		PD  = PD + PPP
	END IF

 RetRepSub:
	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

	%Page

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

 !20000	DEF FNRAIN(R, A, I, N%) = R / I * ((1.0 + I) ^ N% - 1.0) - A

 !20010	DEF FNRIN(R, I, N%) = R * (N% * I * (1.0 + I) ^ &
 !		(N% - 1%) - (1.0 + I) ^ N% + 1.0) / I ^ 2%

20020	DEF FNNI(N%, I) = ((1% + I) ^ N% - 1%) / I

20030	DEF FNANI(N%, I) =(1% - (1% + I) ^ (-N%)) / I

32767	END
