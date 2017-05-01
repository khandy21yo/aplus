1	%TITLE "QUERY - Query General Ledger System"
	%SBTTL "GL_QURY_QUERY"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!	The ^*General Ledger Query\* is used to scan specific General
	!	Ledger account(s), sub-account(s), cross-reference(s), check
	!	number(s), or any combination of these choices.
	!	.LM -5
	!
	! Index:
	!	.x Query>General Ledger
	!	.x General Ledger>Query
	!	.x Scan>Account
	!	.x Scan>Sub Account
	!	.x Scan>Cross Reference
	!	.x Scan>Batch
	!
	! Option:
	!
	!	GL_QURY_QUERY$PRINT
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_QURY_QUERY/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_QURY_QUERY, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_QURY_QUERY.OBJ;*
	!
	! Author:
	!
	!	12/22/86 - Kevin Handy
	!
	! Modification history:
	!
	!	11/21/90 - Kevin Handy
	!		Modified to force from/to periods to be six
	!		characters in length on input.
	!
	!	01/10/91 - Craig Tanner
	!		Where FILENAME$ = "GL_YYYY_PP" in error handler,
	!		changed to = "GL_" + YYYY_PP$.
	!
	!	02/28/91 - Kevin Handy
	!		Increased the number of files from 40 to 100.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	03/12/92 - Kevin Handy &
	!		Fixed handling of PRG_PROGRAM so it didn't
	!		get lost.
	!
	!	09/16/92 - Kevin Handy
	!		Modified to display reference number instead of
	!		check number if the check number is blank.
	!
	!	09/24/92 - Kevin Handy
	!		Added check for exit key in from/to period entry.
	!
	!	04/12/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Change SMG_QURY to SMG_QUERY%
	!
	!	04/14/95 - Kevin Handy
	!		Fix last parameter to ENTR_3CHOICES. Calico.
	!
	!	09/04/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/09/97 - Kevin Handy
	!		Use OUTP_INITFORM function
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/21/98 - Kevin Handy
	!		Reformat source code while looking for the cause
	!		of a weird lockup on [SOFT]
	!
	!	02/26/99 - Kevin Handy
	!		Increase size of Ck/Ref column by shortening year
	!		to two digits, and losing space before running total,
	!		and moving the flag from the reference to the check.
	!		(LL Chris).
	!
	!	06/10/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add several REGARDLESS clauses
	!
	!	04/10/2001 - Kevin Handy
	!		Change a lot of '.'s in variable names to '_'s.
	!
	!	11/20/2001 - Kevin Handy
	!		Added description wildcard
	!
	!	06/06/2003 - Kevin Handy
	!		Add option for hours/units/dollars
	!
	!	02/01/2005 - Kevin Handy
	!		Increase number of folders to 240 from 120.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map file
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	!
	! Array for listing
	!
	DECLARE INTEGER CONSTANT MAX_RANGE = 240%

	DIM RANGE.CH%(MAX_RANGE)
	DIM GL_YYYY_PP_CDD RANGE_BUF(MAX_RANGE)
	DIM GL_YYYY_PP_FILE$(MAX_RANGE)

	!
	! Declare constants
	!
	DECLARE INTEGER CONSTANT MAX_ITEM = 11%
	DECLARE LONG XLONG, YLONG

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initialize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	RESTORE_PROGRAM$ = SCOPE::PRG_PROGRAM
	RESTORE_IDENT$ = SCOPE::PRG_IDENT

	REPORT$ = "GLQURY"

	!
	! Allocate channels
	!
	CALL ASSG_CHANNEL(FIRST.PERIOD.CH%, STAT%)

	CALL READ_DEVICE("GL_YYYY_PP", GL_YYYY_PP.DEV$, STAT%)

300	!
	! Open chart of accounts
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

310	!
	! Open period file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
		CLOSE GL_PERIOD.CH%
		CALL ASSG_FREECHANNEL(GL_PERIOD.CH%)
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN


320	!******************************************************************
	! Get period for batch
	!******************************************************************

	CALL FIND_FILE(GL_YYYY_PP.DEV$ + "GL_*.LED", GL_YYYY_PP_FILE$(), &
		16%, "", "")

	GL_YYYY_PP_FILE% = VAL%(GL_YYYY_PP_FILE$(0%))

	IF GL_YYYY_PP_FILE% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"General ledger files do not exist", 0%)
		GOTO ExitProgram
	ELSE
		GL_YYYY_PP_FILE$(LOOP%) = &
			MID(GL_YYYY_PP_FILE$(LOOP%), 4%, 4%) + &
			MID(GL_YYYY_PP_FILE$(LOOP%), 9%, 2%) &
			FOR LOOP% = 1% TO GL_YYYY_PP_FILE%
	END IF

500	!******************************************************************
	! Declare defaults for screen
	!******************************************************************

	FROM_PERIOD$ = GL_YYYY_PP_FILE$(1%)
	FROM_PERIOD% = 1%

	TO_PERIOD$ = GL_YYYY_PP_FILE$(GL_YYYY_PP_FILE%)
	TO_PERIOD% = GL_YYYY_PP_FILE%

	BY_ITEM$ = "A"

	ACCOUNT_ITEM$ = "*" + SPACE$(19%)
	SUBACC_ITEM$ = "*" + SPACE$(19%)
	XREF_ITEM$ = "*" + SPACE$(19%)
	CHECK_ITEM$ = "*" + SPACE$(19%)
	BATCH_ITEM$ = "*" + SPACE$(19%)
	SOURCE_ITEM$ = "*" + SPACE$(19%)
	DESCR_ITEM$ = "*" + SPACE$(19%)
	DISPLAY_ITEM$ = "D"

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
		"GL View for " + TRM$(SCOPE::PRG_COMPANY))

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

	GOSUB Repaint

1100	!
	! Enter options
	!
	SCOPE::PRG_ITEM = ""
	SCOPE::PRG_IDENT = RESTORE_IDENT$
	SCOPE::PRG_PROGRAM = RESTORE_PROGRAM$

	OPTLIST$ = "Change Blank Print Help eXit"

	!++
	! Abstract:PRINT
	!	^*Print\*
	!	.b
	!	.lm +5
	!	The General Ledger Query ^*Print\* function
	!	accesses a report setting screen, which provides the option
	!	to print the specific information
	!	being queried.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Query>Print
	!	.x Query>General Ledger>Print
	!	.x Print>General Ledger>Query
	!
	!--
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
			SCOPE::PRG_PROGRAM, "", "HELP")

	CASE "C"
 Changer:
		!*****************************************************
		! Change information on the screen
		!*****************************************************

		LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, "", &
			"Item to change", 0.0, 4%, "##", "")

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

		GOTO 1100 IF LOOP% = 0%
		GOTO Changer IF LOOP% < 1% OR LOOP% > MAX_ITEM

		LOOP1% = LOOP%

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

	CASE "B"
 BlankR:	!*****************************************************
		! Blank information on the screen
		!*****************************************************

		LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, "", &
			"Item to Blank", 0.0, 4%, "##", "")

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

		GOTO 1100 IF LOOP% = 0%
		GOTO Blankr IF LOOP% < 1% OR LOOP% > MAX_ITEM

		SELECT LOOP%

		CASE 1%, 2%, 3%
			CALL ENTR_3MESSAGE(SCOPE, "Sorry, Unable to blank", 0%)

		CASE 4%
			LSET ACCOUNT_ITEM$ = "*"

		CASE 5%
			LSET SUBACC_ITEM$ = "*"

		CASE 6%
			LSET XREF_ITEM$ = "*"

		CASE 7%
			LSET CHECK_ITEM$ = "*"

		CASE 8%
			LSET BATCH_ITEM$ = "*"

		CASE 9%
			LSET SOURCE_ITEM$ = "*"

		CASE 10%
			LSET DESCR_ITEM$ = "*"

		CASE 11%
			LSET DISPLAY_ITEM$ = "D"

		END SELECT

		FLAG% = 1%
		GOSUB DataEntry

		GOTO Blankr

	CASE "P"
		!*****************************************************
		! List all records in all files
		!*****************************************************

		CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

		!
		! Calculate range
		!
		TOTAL = 0.0

		IF FROM_PERIOD$ > TO_PERIOD$
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Periods are in wrong order!", 0%)
			GOTO 1100
		END IF

		!
		! Initialize for scan
		!
		SELECT BY_ITEM$
		CASE "A"
			BY_ITEM1$ = ACCOUNT_ITEM$
			BY_ITEM% = 0%
			BY_LEN% = LEN(GL_YYYY_PP::ACCT)
		CASE "S"
			BY_ITEM1$ = SUBACC_ITEM$
			BY_ITEM% = 1%
			BY_LEN% = LEN(GL_YYYY_PP::SUBACC)
		CASE "X"
			BY_ITEM1$ = XREF_ITEM$
			BY_ITEM% = 2%
			BY_LEN% = LEN(GL_YYYY_PP::XREFNO)
		CASE "C"
			BY_ITEM1$ = CHECK_ITEM$
			BY_ITEM% = 3%
			BY_LEN% = LEN(GL_YYYY_PP::CKNO)
		CASE "B"
			BY_ITEM1$ = BATCH_ITEM$
			BY_ITEM% = 4%
			BY_LEN% = LEN(GL_YYYY_PP::BTHNUM)
		CASE ELSE
			CALL ENTR_3MESSAGE(SCOPE, "Invalid BY ITEM", 0%)
			GOTO 1100
		END SELECT

		!
		! Create FROM_ITEM$ and TO_ITEM$ from BY_ITEM1$
		!
		BY_ITEM1$ = TRM$(BY_ITEM1$)

		FROM_ITEM$, TO_ITEM$ = ""

		IF INSTR(1%, BY_ITEM1$, ",")
		THEN
			FROM_ITEM$, TO_ITEM$ = ""
		ELSE
			I% = INSTR(1%, BY_ITEM1$, "/")
			IF I%
			THEN
				FROM_ITEM$ = LEFT(BY_ITEM1$, I% - 1%)
				TO_ITEM$ = RIGHT(BY_ITEM1$, I% + 1%)
			ELSE
				I%  = INSTR(1%, BY_ITEM1$ + "?", "?")
				I1% = INSTR(1%, BY_ITEM1$, "*")
				I% = I1% IF (I1% < I%) AND (I1% <> 0%)
				IF (I% > 1%)
				THEN
					FROM_ITEM$ = LEFT(BY_ITEM1$, I% - 1%)
					TO_ITEM$ = FROM_ITEM$ + &
						STRING$(18%, -1%)
				END IF
			END IF
		END IF

		FOR COUNTER% = FROM_PERIOD% TO TO_PERIOD%

			!
			! Open up selected file
			!
			YYYY_PP$ = LEFT(GL_YYYY_PP_FILE$(COUNTER%), 4%) + "_" + &
				RIGHT(GL_YYYY_PP_FILE$(COUNTER%), 5%)
			RANGE.CH%(COUNTER%) = -1%

4010			GL_YYYY_PP.CH% = 0%

			WHEN ERROR IN
				%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
			USE
				IF ERR = 5%
				THEN
					CALL ENTR_3MESSAGE(SCOPE, &
						"Unable to open period " + &
						YYYY_PP$, 0%)
					CONTINUE 4080
				END IF
				FILENAME$ = "GL_" + YYYY_PP$
				CONTINUE HelpError
			END WHEN

			RANGE.CH%(COUNTER%) = GL_YYYY_PP.CH%

4020			!
			! Start at front of file
			!
			WHEN ERROR IN
				IF TRM$(FROM_ITEM$) = ""
				THEN
					RESET #GL_YYYY_PP.CH%, KEY #BY_ITEM%
					GET #GL_YYYY_PP.CH%, REGARDLESS
				ELSE
					GET #GL_YYYY_PP.CH%, &
						KEY #BY_ITEM% GE LEFT(FROM_ITEM$, BY_LEN%), &
						REGARDLESS
				END IF
			USE
				CONTINUE 4030
			END WHEN

			RANGE_BUF(COUNTER%) = GL_YYYY_PP

			GOTO 4040

4030			!
			! If there was an error then we do this
			!
			CLOSE GL_YYYY_PP.CH%
			CALL ASSG_FREECHANNEL(GL_YYYY_PP.CH%)
			RANGE.CH%(COUNTER%) = -1%

4040		NEXT COUNTER%

		!*****************************************************
		! List all records in all files
		!*****************************************************

		CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

4080		!
		! Ask user to change settings
		!
		GOTO 1000 &
			IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> &
			CMC$_NORMAL

		TITLE$(1%) = "GENERAL LEDGER QUERY"
		TITLE$(2%) = "From " + LEFT(FROM_PERIOD$, 4%) + "_" + &
			RIGHT(FROM_PERIOD$, 5%) + " To " + &
			LEFT(TO_PERIOD$, 4%) + "_" + &
			RIGHT(TO_PERIOD$, 5%)
		TITLE$(3%) = ""

		TITLE$(4%) = "Period  Account            SubAccount " + &
			"Xref       Ck/Ref      Batch    Date   " + &
			"Description                       Amount " + &
			"Running-Total"
		TITLE$(5%) = ""

4200		!
		ITEMX$ = ""

		PRINT_COUNTER% = 0%

		FOR COUNTER% = FROM_PERIOD% TO TO_PERIOD%

			IF RANGE.CH%(COUNTER%) > 0%
			THEN
				SELECT BY_ITEM$
				CASE "A"
					ITEM$ = RANGE_BUF(COUNTER%)::ACCT
				CASE "S"
					ITEM$ = RANGE_BUF(COUNTER%)::SUBACC
				CASE "X"
					ITEM$ = RANGE_BUF(COUNTER%)::XREFNO
				CASE "C"
					ITEM$ = RANGE_BUF(COUNTER%)::CKNO
				CASE "B"
					ITEM$ = RANGE_BUF(COUNTER%)::BTHNUM
				END SELECT

				IF ITEM$ < ITEMX$ OR PRINT_COUNTER% = 0%
				THEN
					ITEMX$ = ITEM$
					PRINT_COUNTER% = COUNTER%
				END IF
			END IF
		NEXT COUNTER%

		IF TO_ITEM$ <> "" AND ITEMX$ > TO_ITEM$
		THEN
			GOTO 4400
		END IF

4300		!
		! Now, we can print out all of the records that match
		!
		COUNTER% = PRINT_COUNTER%

		!
		! Set up for scan through
		!
		GL_YYYY_PP = RANGE_BUF(COUNTER%)

4320		!
		! Make sure record is in range
		!
		SELECT BY_ITEM$
		CASE "A"
			ITEM$ = GL_YYYY_PP::ACCT
		CASE "S"
			ITEM$ = GL_YYYY_PP::SUBACC
		CASE "X"
			ITEM$ = GL_YYYY_PP::XREFNO
		CASE "C"
			ITEM$ = GL_YYYY_PP::CKNO
		CASE "B"
			ITEM$ = GL_YYYY_PP::BTHNUM
		END SELECT

		GOTO 4380 IF ITEM$ <> ITEMX$

		!
		! Skip item if not in range
		!
		GOTO 4340 IF COMP_STRING(TRM$(GL_YYYY_PP::ACCT), &
			TRM$(ACCOUNT_ITEM$)) = 0%
		GOTO 4340 IF COMP_STRING(TRM$(GL_YYYY_PP::SUBACC), &
			TRM$(SUBACC_ITEM$))  = 0%
		GOTO 4340 IF COMP_STRING(TRM$(GL_YYYY_PP::XREFNO), &
			TRM$(XREF_ITEM$))    = 0%
		GOTO 4340 IF COMP_STRING(TRM$(GL_YYYY_PP::CKNO), &
			TRM$(CHECK_ITEM$))   = 0%
		GOTO 4340 IF COMP_STRING(TRM$(GL_YYYY_PP::BTHNUM), &
			TRM$(BATCH_ITEM$))   = 0%
		GOTO 4340 IF COMP_STRING(TRM$(GL_YYYY_PP::SOURCE), &
			TRM$(SOURCE_ITEM$))   = 0%
		GOTO 4340 IF COMP_STRING(TRM$(GL_YYYY_PP::DESCR), &
			TRM$(DESCR_ITEM$))   = 0%

		SELECT DISPLAY_ITEM$
		CASE "H"
			AMOUNT = GL_YYYY_PP::HOURS
		CASE "U"
			AMOUNT = GL_YYYY_PP::UNITS
		CASE ELSE
			AMOUNT = GL_YYYY_PP::AMOUNT
		END SELECT

		TOTAL = TOTAL + AMOUNT

		!
		! Decide weither to print check number or invoice number
		!
		IF GL_YYYY_PP::CKNO = "" AND TRM$(GL_YYYY_PP::REFNO) <> ""
		THEN
			TEMP$ = LEFT(GL_YYYY_PP::REFNO, 11%)
		ELSE
			TEMP$ = ">" + GL_YYYY_PP::CKNO + "    "
		END IF

		TEXT$ = LEFT(GL_YYYY_PP_FILE$(COUNTER%), 4%) + " " + &
			RIGHT(GL_YYYY_PP_FILE$(COUNTER%), 5%) + " " + &
			GL_YYYY_PP::ACCT + " " + &
			GL_YYYY_PP::SUBACC + " " + &
			GL_YYYY_PP::XREFNO + " " + &
			TEMP$ + " " + &
			GL_YYYY_PP::BTHNUM + " " + &
			PRNT_DATE(GL_YYYY_PP::TRANDAT, 6%) + " " + &
			LEFT(GL_YYYY_PP::DESCR, 26%) + " " + &
			FORMAT$(AMOUNT, "##,###,###.##") + &
			FORMAT$(TOTAL, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO 4400 IF UTL_REPORTX::STAT

4340		WHEN ERROR IN
			GET #RANGE.CH%(COUNTER%), REGARDLESS
		USE
			IF ERR = 11%
			THEN
				CLOSE RANGE.CH%(COUNTER%)
				CALL ASSG_FREECHANNEL(RANGE.CH%(COUNTER%))
				RANGE.CH%(COUNTER%) = -1%
			END IF
			CONTINUE 4380
		END WHEN

		RANGE_BUF(COUNTER%) = GL_YYYY_PP

		GOTO 4320

4380		FOR COUNTER% = FROM_PERIOD% TO TO_PERIOD%
			!
			! If we fall through, test to see if at end of all files
			!
			IF RANGE.CH%(COUNTER%) > 0%
			THEN
				GOTO 4200
			END IF

		NEXT COUNTER%

4400		!
		! Finish up
		!
		CALL OUTP_FINISH(UTL_REPORTX)

		FOR COUNTER% = FROM_PERIOD% TO TO_PERIOD%
			IF RANGE.CH%(COUNTER%) > 0%
			THEN
				CLOSE RANGE.CH%(COUNTER%)
				CALL ASSG_FREECHANNEL(RANGE.CH%(COUNTER%))
			END IF
		NEXT COUNTER%

		GOTO 900

	CASE "X"
		GOTO ExitProgram

	END SELECT

	GOTO 1100

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 Repaint:
	!******************************************************************
	! Repaint the screen
	!******************************************************************

	DATA	3,20, "(01) From Period", &
		4,20, "(02) To Period", &
		6,20, "(03) By Item", &
		8,20, "(04) Account Wildcard", &
		9,20, "(05) Subaccount Wildcard", &
		10,20, "(06) Xref Wildcard", &
		11,20, "(07) Check Wildcard", &
		12,20, "(08) Batch Wildcard", &
		13,20, "(09) Source Wildcard", &
		14,20, "(10) Descr. Wildcard", &
		16,20, "(11) Display Item", &
		0, 0, ""

	RESTORE
	READ XLONG, YLONG, ATEXT$

	WHILE XLONG
		SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, ATEXT$, XLONG, YLONG)
		READ XLONG, YLONG, ATEXT$
	NEXT

	FLAG% = 1%
	GOSUB DataEntry &
		FOR LOOP% = 1% TO MAX_ITEM

	RETURN

	%PAGE

 DataEntry:
	!******************************************************************
	! Enter/Diaplay items
	!******************************************************************

	TEMP$ = TRM$(SCOPE::PRG_ITEM)

	SCOPE::PRG_ITEM = "FLD" + FORMAT$(LOOP%, "<0>##")

	SELECT LOOP%

	CASE 1%

	!++
	! Abstract:FLD001
	!	.x General Ledger>Query>From Period
	!	^*(01) From Period\*
	!	.b
	!	.lm +5
	!	The ^*From Period\* field allows the selection of
	!	an accounting period from which the report is to begin.
	!	.b
	!	The format for entry is ^*YYYYPP\*.
	!	.LM -5
	!
	! Index:
	!	.x From Period>General Ledger Query
	!
	!--
 DataEntry1:	FROM_PERIOD1$ = LEFT(FROM_PERIOD$ + "      ", 6%)
		FROM_PERIOD1$ = ENTR_3STRING(SCOPE, &
			SMG_QUERY%, "3;38", "From period", &
			FROM_PERIOD1$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT
		!
		! Exit key
		!
		CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			JUNK$ = ENTR_3STRING(SCOPE, SMG_QUERY%, &
				"3;38", "From period", &
				FROM_PERIOD$, 1%, "'E", DEFLT$)
			GOTO DataEntry1a

		END SELECT

		FROM_PERIOD% = 0%

		FOR TEST% = 1% TO GL_YYYY_PP_FILE%
			FROM_PERIOD% = TEST% IF FROM_PERIOD1$ = &
				GL_YYYY_PP_FILE$(TEST%)
		NEXT TEST%

		IF FROM_PERIOD% = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"From period not in range", 0%)
			GOTO DataEntry1
		END IF

		FROM_PERIOD$ = FROM_PERIOD1$
 DataEntry1a:

	CASE 2%

	!++
	! Abstract:FLD002
	!	.x General Ledger>Query>To Period
	!	^*(02) To Period\*
	!	.b
	!	.lm +5
	!	The ^*To Period\* field allows for the selection of an
	!	accounting period with which the report will end.
	!	.b
	!	The format for entry is ^*YYYYPP\*.
	!	.lm -5
	!
	! Index:
	!	.x To Period>General Ledger Query
	!
	!--

 DataEntry2:	TO_PERIOD1$ = LEFT(TO_PERIOD$ + "      ", 6%)
		TO_PERIOD1$ = ENTR_3STRING(SCOPE, SMG_QUERY%, &
			"4;38", "To period", &
			TO_PERIOD1$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT
		!
		! Exit key
		!
		CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			JUNK$ = ENTR_3STRING(SCOPE, SMG_QUERY%, &
				"4;38", "To period", &
				TO_PERIOD$, 1%, "'E", DEFLT$)
			GOTO DataEntry2a

		END SELECT

		TO_PERIOD% = 0%

		FOR TEST% = 1% TO GL_YYYY_PP_FILE%
			TO_PERIOD% = TEST% IF TO_PERIOD1$ = &
				GL_YYYY_PP_FILE$(TEST%)
		NEXT TEST%

		IF TO_PERIOD% = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "To period not in range", 0%)
			GOTO DataEntry2
		END IF

		TO_PERIOD$ = TO_PERIOD1$

 DataEntry2a:
	CASE 3%

	!++
	! Abstract:FLD003
	!	.x General Ledger>Query>By Item
	!	^*(03) By Item\*
	!	.b
	!	.lm +5
	!	The ^*By Item\* field determines which field
	!	in the General Ledger file will be searched.
	!	.b
	!	Valid options are:
	!	.TABLE 3,25
	!	.te
	!	^*A\* - Account
	!	.TE
	!	^*S\* - Subaccount
	!	.TE
	!	^*X\* - X-ref
	!	.TE
	!	^*C\* - Check
	!	.TE
	!	^*B\* - Batch
	!	.END TABLE
	!	.b
	!	Pressing ^*List Choices\* will provide a list of valid options.
	!	.LM -5
	!
	! Index:
	!
	!--
		BY_ITEM$ = EDIT$(ENTR_3STRING(SCOPE, SMG_QUERY%, "6;38", &
			"By item", BY_ITEM$, FLAG%, "'E", DEFLT$), -1%)

		GOTO DataEntry3 IF INSTR(1%, "A,S,X,C,B", BY_ITEM$) &
			AND (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14) = 0% &
			AND BY_ITEM$ <> ""

		BY_METHOD$(1%) = "A Account"
		BY_METHOD$(2%) = "S Subaccount"
		BY_METHOD$(3%) = "X Xref"
		BY_METHOD$(4%) = "C Check"
		BY_METHOD$(5%) = "B Batch"
		BY_METHOD$(6%) = ""
		TEMP$ = "Search By"

		X% = ENTR_3CHOICE(SCOPE, "", "", BY_METHOD$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			BY_ITEM$ = LEFT(BY_METHOD$(X%), 1%)
			SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
				BY_ITEM$, &
				6%, 38%,, SMG$M_BOLD)
		END IF

 DataEntry3:
		SELECT BY_ITEM$
		CASE "A"
			SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
				"Account   ", 6%, 42%,, SMG$M_BOLD)
		CASE "S"
			SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
				"Subaccount", 6%, 42%,, SMG$M_BOLD)
		CASE "X"
			SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
				"Xref      ", 6%, 42%,, SMG$M_BOLD)
		CASE "C"
			SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
				"Check     ", 6%, 42%,, SMG$M_BOLD)
		CASE "B"
			SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
				"Batch     ", 6%, 42%,, SMG$M_BOLD)
		CASE ELSE
			SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, &
				"**********", 6%, 42%,, SMG$M_BOLD)
		END SELECT

	CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Account Wildcard\*
	!	.b
	!	.lm +5
	!	General Ledger transactions which relate to a specific
	!	account will be printed when this field contains
	!	a valid account number. Transactions for multiple accounts
	!	can be reviewed by entering multiple account numbers, each
	!	separated with a comma.
	!	.b
	!	Valid wildcard characters are an asterisk (_*) or a
	!	question mark (?). An asterisk (_*) indicates ^&all\& account
	!	numbers will be queried. A question mark (?) in a field
	!	position indicates an account number with ^&any\& character in
	!	that equivalent position will be queried.
	!	.b
	!	The field will accommodate up to 20 characters.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Account
	!	.x Account>Wildcard
	!
	!--
		ACCOUNT_ITEM$ = ENTR_3STRING(SCOPE, SMG_QUERY%, &
			"8;45", "Account wildcard", &
			ACCOUNT_ITEM$, FLAG%, "'E", DEFLT$)

	CASE 5%

	!++
	! Abstract:FLD005
	!	.x Wildcard>Subaccount
	!	^*(05) Subaccount Wildcard\*
	!	.b
	!	.lm +5
	!	General Ledger transactions which relate to a specific
	!	subaccount will be printed when this field
	!	contains a valid subaccount number. Transactions for multiple
	!	subaccounts can be reviewed by entering multiple subaccount
	!	numbers, each separated with a comma.
	!	.b
	!	Valid wildcard characters are an asterisk (_*) or a question
	!	mark (?). An asterisk (_*) indicates ^&all\& subaccount numbers will
	!	be queried. A question mark (?) in a field position indicates a
	!	subaccount number with ^&any\& character in that equivalent position
	!	will be queried.
	!	.b
	!	The field will accommodate up to 20 characters.
	!	.lm -5
	!
	! Index:
	!	.x Subaccount>Wildcard
	!	.x General Ledger>Query>Subaccount Wildcard
	!
	!--
		SUBACC_ITEM$ = ENTR_3STRING(SCOPE, SMG_QUERY%, &
			"9;45", "Subaccount wildcard", &
			SUBACC_ITEM$, FLAG%, "'E", DEFLT$)

	CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Xref Wildcard\*
	!	.b
	!	.lm +5
	!	General Ledger transactions which relate to a specific
	!	cross-reference will be printed when this field
	!	contains a valid cross-reference number. Transactions for
	!	multiple cross-reference numbers can be reviewed by entering
	!	multiple cross-reference numbers, each separated with a
	!	comma.
	!	.b
	!	Valid wildcard characters are an asterisk (_*) or a
	!	question mark (?). An asterisk (_*) indicates ^&all\& cross-
	!	reference numbers will be queried. A question mark (?) in
	!	a field position indicates a cross-reference with ^&any\&
	!	character in that equivalent position will be queried.
	!	.b
	!	The field will accommodate up to 20 characters.
	!	.lm -5
	!
	! Index:
	!	.x Cross-reference>Wildcard
	!	.x Wildcard>Cross-reference
	!
	!--
		XREF_ITEM$ = ENTR_3STRING(SCOPE, SMG_QUERY%, &
			"10;45", "Xref wildcard", &
			XREF_ITEM$, FLAG%, "'E", DEFLT$)

	CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Check Wildcard\*
	!	.b
	!	.lm +5
	!	General Ledger transactions which relate to a specific
	!	check number will be printed when this field
	!	contains a valid check number. Transactions for multiple
	!	check numbers can be reviewed by entering multiple check
	!	numbers, each separated with a comma.
	!	.b
	!	Valid wildcard characters are an asterisk (_*) or a
	!	question mark (?). An asterisk (_*) indicates ^&all\& check
	!	numbers will be queried. A question mark (?) in a field
	!	position indicates a check number with ^&any\& character in
	!	that equivalent position will be queried.
	!	.b
	!	The field will accommodate 20 characters.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Check
	!	.x Check>Wildcard
	!
	!--
		CHECK_ITEM$ = ENTR_3STRING(SCOPE, SMG_QUERY%, &
			"11;45", "Check wildcard", &
			CHECK_ITEM$, FLAG%, "'E", DEFLT$)

	CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) Batch Wildcard\*
	!	.b
	!	.lm +5
	!	General Ledger transactions which relate to a specific
	!	batch number will be printed when this field
	!	contains a valid batch number. Transactions for multiple
	!	batch numbers can be reviewed by entering multiple batch
	!	numbers, each separated with a comma.
	!	.b
	!	Valid wildcard characters are an asterisk (_*) or a
	!	question mark (?). An asterisk (_*) indicates ^&all\& batch
	!	numbers will be queried. A question mark (?) in a field
	!	position indicates a batch number with ^&any\& character in
	!	that equivalent position will be queried.
	!	.b
	!	The field will accommodate up to 20 characters.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Batch
	!	.x Batch>Wildcard
	!
	!--
		BATCH_ITEM$ = ENTR_3STRING(SCOPE, SMG_QUERY%, &
			"12;45", "Batch wildcard", &
			BATCH_ITEM$, FLAG%, "'E", DEFLT$)

	CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Source Wildcard\*
	!	.b
	!	.lm +5
	!	General Ledger transactions which relate to a specific
	!	Source Code will be printed when this field
	!	contains a valid Source code. Transactions for multiple
	!	sources can be reviewed by entering multiple source codes,
	!	each separated with a comma.
	!	.b
	!	Valid wildcard characters are an asterisk (_*) or a
	!	question mark (?). An asterisk (_*) indicates ^&all\& batch
	!	numbers will be queried. A question mark (?) in a field
	!	position indicates a batch number with ^&any\& character in
	!	that equivalent position will be queried.
	!	.b
	!	The field will accommodate up to 20 characters.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Source
	!	.x Source>Wildcard
	!
	!--
		SOURCE_ITEM$ = ENTR_3STRING(SCOPE, SMG_QUERY%, &
			"13;45", "Batch wildcard", &
			SOURCE_ITEM$, FLAG%, "'E", DEFLT$)

	CASE 10%

	!++
	! Abstract:FLD010
	!	^*(09) Description Wildcard\*
	!	.b
	!	.lm +5
	!	General Ledger transactions which relate to a specific
	!	Description will be printede. Transactions for multiple
	!	descriptions can be reviewed by entering multiple source codes,
	!	each separated with a comma.
	!	.b
	!	Valid wildcard characters are an asterisk (_*) or a
	!	question mark (?). An asterisk (_*) indicates ^&all\& batch
	!	numbers will be queried. A question mark (?) in a field
	!	position indicates a batch number with ^&any\& character in
	!	that equivalent position will be queried.
	!	.b
	!	The field will accommodate up to 20 characters.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Description
	!
	!--
		DESCR_ITEM$ = ENTR_3STRING(SCOPE, SMG_QUERY%, &
			"14;45", "Description wildcard", &
			DESCR_ITEM$, FLAG%, "'E", DEFLT$)

	CASE 11%

	!++
	! Abstract:FLD011
	!	.x General Ledger>Query>Dollars
	!	.x General Ledger>Query>Units
	!	.x General Ledger>Query>Hours
	!	^*(11) Dollars, Units, Hours\*
	!	.b
	!	.lm +5
	!	Determines if the query should diplay dollars, units,
	!	or hours.
	!	.b
	!	Valid options are:
	!	.TABLE 3,25
	!	.te
	!	^*D\* - Dollars
	!	.TE
	!	^*U\* - Units
	!	.TE
	!	^*H\* - Hours
	!	.END TABLE
	!	.b
	!	Pressing ^*List Choices\* will provide a list of valid options.
	!	.LM -5
	!
	! Index:
	!
	!--

		BY_METHOD$(1%) = "D Dollars"
		BY_METHOD$(2%) = "H Hours"
		BY_METHOD$(3%) = "U Units"
		BY_METHOD$(4%) = ""

		DISPLAY_ITEM$ = ENTR_3STRINGLIST(SCOPE, &
			SMG_QUERY%, "16;45", &
			"Display item", DISPLAY_ITEM$, FLAG% OR 32%, &
			"'E", DEFLT$, &
			BY_METHOD$(), "Display Item", "008")

	END SELECT

	SCOPE::PRG_ITEM = TEMP$

	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
