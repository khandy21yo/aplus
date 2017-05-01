1	%TITLE "User Defined Summary/Detail Report"
	%SBTTL "GL_RPRT_VENCOL"
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
	! ID:GLVCOL
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*User Defined Summary/Detail Report\* option allows access to a user
	!	defined report containing up to six columns of selected data. The accounting
	!	period, report heading, column heading(s) and data, and the level of detail
	!	to be printed are all defined by the user.
	!	.lm -5
	!
	! Index:
	!	.x Reports>User Defined
	!	.x User Defined Reports
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_VENCOL/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_RPRT_VENCOL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_VENCOL.OBJ;*
	!
	! Author:
	!
	!	10/25/88 - Kevin Handy
	!
	! Modification history:
	!
	!	11/15/88 - Kevin Handy
	!		Fixed to print last vendor in Xref report.
	!
	!	08/16/89 - Kevin Handy
	!		Fixed bug where if you exited out of report
	!		settings screen, it did not remove the screen.
	!
	!	01/10/91 - Craig Tanner
	!		Where FILENAME$ = "GL_YYYY_PP" in error handler,
	!		changed to = "GL_" + YYYY_PP$.
	!
	!	03/08/91 - Kevin Handy
	!		Modified to that REPORT$ was defined, otherwise
	!		the program would crash and burn a horrible
	!		death.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	06/14/93 - Kevin Handy
	!		Added REGARDLESS to GL_PERIOD.
	!
	!	04/12/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Change SMG_QUERY to SMG_QUERY%
	!
	!	08/14/96 - Kevin Handy
	!		Lose extra '&' before 'end if'
	!		Reformat source code.
	!
	!	10/18/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/09/97 - Kevin Handy
	!		Use OUTP_INITFORM function.
	!
	!	08/25/97 - Kevin Handy
	!		Clean up (Check)
	!		Lose error trap for 4100, which doesn't exist
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/21/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Format record structures
	!
	RECORD TEMP_FORMAT_CDD
		STRING	STORE = 18%
		STRING	XREFNO = 10%
		STRING	ACCT = 18%
		RFA	PTR
	END RECORD

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP	(GL_PERIOD)	GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_VENCOL.HB"
	MAP	(GL_VENCOL)	GL_VENCOL_CDD	GL_VENCOL

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP	(GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	MAP	(TEMP_FORMAT)	TEMP_FORMAT_CDD	TEMP_FORMAT

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	!
	! Declare constants and/or variables
	!
	DECLARE	LONG	CONSTANT	MAX_COLUMNS = 6%
	DECLARE	LONG	CONSTANT	MAX_ITEM = 9%
	DECLARE	LONG			XLONG, YLONG

	!
	! Dimension arrays
	!
	DIM	CHOICES$(100%)

	%PAGE

	!*******************************************************************
	! Initialize maintainence
	!*******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Initialize the program
	!
	CALL READ_INITIALIZE

	REPORT$ = "GLVCOL"
	JJ$ = READ_SYSJOB

	%PAGE

	!*******************************************************************
	!
	!*******************************************************************

	!
	! Allocate channels
	!
	CALL ASSG_CHANNEL(FIRST.PERIOD.CH%, STAT%)

	!
	! Read the device specs
	!
	CALL READ_DEVICE("GL_YYYY_PP", GL_YYYY_PP.DEV$, STAT%)

	!
	! Set up list of choices for field 8 in the initial screen
	!
	ECTITLE$ = "Code Description"
	EC$(0%) = "3"
	EC$(1%) = "D    Detail"
	EC$(2%) = "X    XREF"
	EC$(3%) = "S    Store"

	%PAGE

	!*******************************************************************
	! Open most of the files
	!*******************************************************************

	!
	! Open the General Ledger Chart of Accounts file
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

	!
	! Open the GL "Vendor Scan by Column" Information Table
	!
305	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_VENCOL.CRE"
	USE
		FILENAME$ = "GL_VENCOL"
		CONTINUE HelpError
	END WHEN

	!
	! Open the GL Period Definition file and get the controlling record
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.OPN"
		GET #GL_PERIOD.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "GL_PERIOD"
		CONTINUE HelpError
	END WHEN

	!
	! Close the Period file and free the channel
	!
	CLOSE GL_PERIOD.CH%
	CALL ASSG_FREECHANNEL(GL_PERIOD.CH%)

	%PAGE

400	!******************************************************************
	! Set defaults, and draw the initial screen
	!******************************************************************

	!
	! Set the defaults for the screen
	!
	GL_VENCOL::FROMPER = SPACE$(6%)
	GL_VENCOL::COL_FLAG = "D"
	GL_VENCOL::COL_TITLE = SPACE$(40%)

	FOR I% = 1% TO MAX_COLUMNS
		GL_VENCOL::COL_TITLEA(I%) = SPACE$(12%)
		GL_VENCOL::COL_TITLEB(I%) = SPACE$(12%)
		GL_VENCOL::COL_ACCOUNT(I%) = SPACE$(45%)
	NEXT I%

	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, 78%, &
		SMG_QUERY%, SMG$M_BORDER)

	!
	! Label the display border
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_QUERY%, &
		" GL Column Report for " + TRM$(SCOPE::PRG_COMPANY) + " ")

	!
	! Put the stuff on the screen
	!
	GOSUB Repaint

	!
	! Paste the display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_QUERY%, &
		SCOPE::SMG_PBID, 2%, 2%)

	%PAGE

1000	!******************************************************************
	! Main option menu
	!******************************************************************

	!
	! Paint the stuff onto the initial screen
	!
	GOSUB Repaint

1100	!
	! Reset Help information
	!
	SCOPE::PRG_ITEM = ""

	!
	! Enter options
	!
	OPTLIST$ = "Change Blank Print Store Recall Erase Help eXit"
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, OPT%, 0%)

	!
	! Check for any special keys
	!
	SELECT SCOPE::SCOPE_EXIT

	!
	! Cancel (^C, F8)
	!
	CASE 3%, SMG$K_TRM_F8
		GOTO 1000

	!
	! Exit keys (^Z, F10)
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	END SELECT

	!
	! Which option did the user select?
	!
	SELECT OPT$

	!
	! Change the information in one or more fields
	!
	CASE "C"
 ChangeOpt:
		!
		! Which field does the user want to change?
		!
		LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, &
			"", "Item to change", &
			0.0, 4%, "##", "")

		!
		! Check for special keys
		!
		SELECT SCOPE::SCOPE_EXIT

		!
		! Cancel (^C, F8)
		!
		CASE 3%, SMG$K_TRM_F8
			GOTO 1000

		!
		! Exit keys (^Z, F10)
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1100

		END SELECT

		!
		! Did the user input an invalid field number?
		!
		GOTO 1100 IF LOOP% = 0%
		GOTO ChangeOpt IF (LOOP% < 1%) OR (LOOP% > MAX_ITEM)

		LOOP1% = LOOP%

 EntrChange:
		!
		! Actually enter the change
		!
		FLAG% = 0%
		GOSUB DataEntry

		!
		! Check for special keys
		!
		SELECT SCOPE::SCOPE_EXIT

		!
		! Cancel (^C, F8)
		!
		CASE 3%, SMG$K_TRM_F8
			GOTO 1000

		!
		! Uparrow
		!
		CASE SMG$K_TRM_UP
			IF (LOOP% = 1%)
			THEN
				LOOP% = MAX_ITEM
			ELSE
				LOOP% = LOOP% - 1%
			END IF
			GOTO EntrChange

		!
		! Downarrow
		!
		CASE SMG$K_TRM_DOWN
			IF (LOOP% = MAX_ITEM)
			THEN
				LOOP% = 1%
			ELSE
				LOOP% = LOOP% + 1%
			END IF
			GOTO EntrChange

		!
		! Exit keys (^Z, F10)
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1100

		END SELECT

		!
		! Does the user wish to change another field?
		!
		GOTO ChangeOpt

	!
	! Blank some fields on the screen
	!
	CASE "B"
 BlankOpt:
		!
		! Blank which field(s)?
		!
		LOOP% = ENTR_3NUMBER(SCOPE, SCOPE::SMG_OPTION, &
			"", "Item to Blank", &
			0.0, 4%, "##", "")

		!
		! Check for special keys
		!
		SELECT SCOPE::SCOPE_EXIT

		!
		! Cancel (^C, F8)
		!
		CASE 3%, SMG$K_TRM_F8
			GOTO 1000

		!
		! Exit keys (^Z, F10)
		!
		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1100

		END SELECT

		!
		! Is the field number valid?
		!
		GOTO 1100 IF (LOOP% = 0%)
		GOTO BlankOpt IF (LOOP% < 1%) OR (LOOP% > MAX_ITEM)

		!
		! Blank the field specified, if possible
		!
		SELECT LOOP%

		!
		! Cannot blank the first field
		!
		CASE 1%
			CALL ENTR_3MESSAGE(SCOPE, "Sorry, Unable to blank", 0%)

		!
		! Blank the column fields
		!
		CASE 2% to 7%
			GL_VENCOL::COL_TITLEA(LOOP% - 1%) = ""
			GL_VENCOL::COL_TITLEB(LOOP% - 1%) = ""
			GL_VENCOL::COL_ACCOUNT(LOOP% - 1%) = ""

		!
		! Set the flag to a default value
		!
		CASE 8%
			GL_VENCOL::COL_FLAG = "D"

		!
		! Blank the field
		!
		CASE 9%
			LSET GL_VENCOL::COL_TITLE = ""

		END SELECT

		FLAG% = 1%
		GOSUB DataEntry

		!
		! Does the user wish to blank another field?
		!
		GOTO BlankOpt

	!
	! Move on to the next part of the program
	!
	CASE "P"
		!
		! If there is a period, then go
		!
		GOTO 1000 IF (GL_VENCOL::FROMPER = "      ")

		CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)
		GOTO 4000

	!
	! Store these values for later use
	!
	CASE "S"

		!
		! Reset Help information
		!
		SCOPE::PRG_ITEM = "STORE"

		!
		! Have user input RECord KEY value
		!
		GL_VENCOL::RECKEY = ENTR_3STRING(SCOPE, SMG_QUERY%, "", &
			"Key to Insert with", GL_VENCOL::RECKEY, &
			16%, "'E", "")

		!
		! Check if key is unique
		!
		WHEN ERROR IN
			FIND #GL_VENCOL.CH%, KEY #0% EQ GL_VENCOL::RECKEY

			!
			! Key is not unique - replace/update record
			!
			UPDATE #GL_VENCOL.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE PutRecord &
				IF (SCOPE::PRG_ITEM = "STORE") AND (ERR = 155%)
			FILENAME$ = "INITIAL SCREEN"
			CONTINUE HelpError
		END WHEN

		GOTO 1000

	!
	! Recall a set of initial screen values
	!
	CASE "R"
		!
		! Reset Help information
		!
		SCOPE::PRG_ITEM = "RECALL"

		!
		! Have user input RECord KEY value
		!
		GL_VENCOL::RECKEY = ENTR_3STRING(SCOPE, SMG_QUERY%, "", &
			"Key to Search with", GL_VENCOL::RECKEY, &
			16%, "'E", "")

		!
		! Go to the <ListChoices> subsection, if necessary
		!
		GOSUB ListKeyChoices IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)

		!
		! Get the record
		!
		WHEN ERROR IN
			GET #GL_VENCOL.CH%, KEY #0% EQ GL_VENCOL::RECKEY
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE RecNotExist &
				IF (SCOPE::PRG_ITEM = "RECALL") AND (ERR = 155%)
			FILENAME$ = "INITIAL SCREEN"
			CONTINUE HelpError
		END WHEN

		GOTO 1000

	!
	! Erase a set of screen defaults
	!
	CASE "E"
		!
		! Reset Help information
		!
		SCOPE::PRG_ITEM = "ERASE"

		!
		! Have user input RECord KEY value
		!
		GL_VENCOL::RECKEY = ENTR_3STRING(SCOPE, SMG_QUERY%, "", &
			"Key to Erase with", GL_VENCOL::RECKEY, &
			16%, "'E", "")

		!
		! Go to the <ListChoices> subsection, if necessary
		!
		GOSUB ListKeyChoices IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)

		!
		! Find the record
		!
		WHEN ERROR IN
			FIND #GL_VENCOL.CH%, KEY #0% EQ GL_VENCOL::RECKEY

			!
			! Kill it
			!
			DELETE #GL_VENCOL.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE RecNotExist &
				IF (SCOPE::PRG_ITEM = "ERASE") AND (ERR = 155%)
			FILENAME$ = "INITIAL SCREEN"
			CONTINUE HelpError
		END WHEN

		GOTO 1000

	!
	! Call the help message subprogram
	!
	CASE "H"
		CALL HELP_3MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, "HELP")

	!
	! Exit the program
	!
	CASE "X"
		GOTO ExitProgram

	END SELECT

	!
	! Go back up and ask the user for his/her option again
	!
	GOTO 1100

 PutRecord:
	!
	! Put the record into the table
	!
	WHEN ERROR IN
		PUT #GL_VENCOL.CH%
	USE
		FILENAME$ = "INITIAL SCREEN"
		CONTINUE HelpError
	END WHEN

	GOTO 1000

 RecNotExist:
	!
	! Key does not exist
	!
	CALL ENTR_3MESSAGE(SCOPE, "Record doesn't exist!", 1%)
	GOTO 1000

	%PAGE

4000	!*******************************************************************
	! Process and print report
	!*******************************************************************

	!
	! Open up selected file
	!
	YYYY_PP$ = LEFT(GL_VENCOL::FROMPER, 4%) + "_" + &
		RIGHT(GL_VENCOL::FROMPER, 5%)

4010	GL_YYYY_PP.CH% = 0%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"
	USE
		IF ERR = 5%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, &
				"Unable to open period " + YYYY_PP$, 0%)
			CONTINUE 4080
		END IF
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN


4020	!*****************************************************
	! List all records in all files
	!*****************************************************
	CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

4080	!
	! Ask user to change settings
	!
	GOTO 1000 &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

4112	RESET #GL_YYYY_PP.CH%

	CALL ENTR_3MESSAGE(SCOPE, "Creating work file . . .", 1%)

4115	CALL ASSG_CHANNEL(GL_TEMP.CH%, STAT%)
	CALL READ_DEVICE("UT_WORK", GL_TEMP.DEV$, STAT%)
	TEMP_FORMAT.NAME$ = GL_TEMP.DEV$ + &
		"TEMPS_" + JJ$ + ".TMP"

	WHEN ERROR IN
		OPEN TEMP_FORMAT.NAME$ FOR OUTPUT AS FILE GL_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP TEMP_FORMAT, &
			PRIMARY KEY (TEMP_FORMAT::STORE, &
				TEMP_FORMAT::XREFNO, &
				TEMP_FORMAT::ACCT) DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, &
			ALLOW NONE
	USE
		FILENAME$ = "UT_WORK"
		CONTINUE HelpError
	END WHEN

4117	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		CONTINUE 4120 IF ERR = 11%
		FILENAME$ = "UT_WORK"
		CONTINUE HelpError
	END WHEN

	FOR LOOP% = 1% TO MAX_COLUMNS
		IF (COMP_STRING(TRM$(GL_YYYY_PP::ACCT), &
			GL_VENCOL::COL_ACCOUNT(LOOP%)) <> 0%)
		THEN
			GOTO 4118
		END IF
	NEXT LOOP%

	GOTO 4117

4118	!
	!
	TEMP% = INSTR(1%, GL_YYYY_PP::ACCT, "-")
	TEMP_FORMAT::STORE = RIGHT(GL_YYYY_PP::ACCT, TEMP% + 1%)
	IF GL_VENCOL::COL_FLAG = "D"
	THEN
		TEMP_FORMAT::ACCT = GL_YYYY_PP::ACCT
	ELSE
		TEMP_FORMAT::ACCT = GL_YYYY_PP::DESCR
	END IF
	TEMP_FORMAT::XREFNO = GL_YYYY_PP::XREFNO
	TEMP_FORMAT::PTR = GETRFA(GL_YYYY_PP.CH%)

	WHEN ERROR IN
		PUT #GL_TEMP.CH%
	USE
		FILENAME$ = "UT_WORK"
		CONTINUE HelpError
	END WHEN

	GOTO 4117

4120	WHEN ERROR IN
		RESET #GL_TEMP.CH%
	USE
		FILENAME$ = "GL_" + YYYY_PP$
		CONTINUE HelpError
	END WHEN

	TITLE$(1%) = "GENERAL LEDGER VENDOR COLUMN"
	TITLE$(2%) = "For " + LEFT(GL_VENCOL::FROMPER, 4%) + "_" + &
		RIGHT(GL_VENCOL::FROMPER, 5%)
	IF GL_VENCOL::COL_TITLE = ""
	THEN
		SELECT GL_VENCOL::COL_FLAG
		CASE "D"
			TITLE$(3%) = "In Detail"
		CASE "X"
			TITLE$(3%) = "By XREF"
		CASE "S"
			TITLE$(3%) = "By Store"
		END SELECT
	ELSE
		TITLE$(3%) = TRM$(GL_VENCOL::COL_TITLE)
	END IF
	TITLE$(4%) = ""

	TITLE$(5%) = "                   " + &
		"           " + &
		"                                         "
	TITLE$(5%) = TITLE$(5%) + " " + &
		GL_VENCOL::COL_TITLEA(I%) &
		FOR I% = 1% TO MAX_COLUMNS
	TITLE$(5%) = "." IF TITLE$(5%) = ""

	TITLE$(6%) = "Account            " + &
		"Xref       " + &
		"Description                       Date   "
	TITLE$(6%) = TITLE$(6%) + " " + &
		GL_VENCOL::COL_TITLEB(I%) FOR I% = 1% TO MAX_COLUMNS
	TITLE$(7%) = ""

	LYT_LINE$ = "$Account:19,$Xref:30,$Descr:61,DDate:75"
	LYT_LINE$ = LYT_LINE$ + ",Vcol" + NUM1$(I%) + &
		":" + NUM1$(I% * 13% + 75%) &
		FOR I% = 1% TO MAX_COLUMNS

	COL_TOTAL(I%), COL_TOTAL1(I%), COL_TOTAL2(I%) = 0.0 &
		FOR I% = 1% TO MAX_COLUMNS
	COL_TOTAL2_FLAG%(I%) = 0% &
		FOR I% = 1% TO MAX_COLUMNS

	THIS_GROUP$ = ""
	THIS_FLAG% = 0%

4200	!
	! Scan through the periods for the next group to display
	!
	!
	WHEN ERROR IN
		GET #GL_TEMP.CH%, REGARDLESS
		GET #GL_YYYY_PP.CH%, RFA TEMP_FORMAT::PTR, REGARDLESS
	USE
		CONTINUE 4390
	END WHEN

4300	!
	! Now, we can print out all of the records that match
	!
	IF (GL_VENCOL::COL_FLAG = "X") AND &
		(THIS_ITEM$ <> TEMP_FORMAT::STORE + &
			GL_YYYY_PP::XREFNO + &
			GL_YYYY_PP::DESCR)
	THEN
		GOSUB SFlagTotal
	END IF

	!
	! Go to new page if the store number changes
	!
	IF THIS_GROUP$ <> TEMP_FORMAT::STORE
	THEN
		GOSUB StoreTotal

		THIS_GROUP$ = TEMP_FORMAT::STORE
		THIS_FLAG% = -1%

		IF GL_VENCOL::COL_FLAG <> "S"
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 5%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
				"Store: " + TEMP_FORMAT::STORE, 0%)
		END IF

	END IF

	!
	! Skip item if not in range
	!
	TEMP$ = ""
	FOR LOOP% = 1% TO MAX_COLUMNS
		IF (GL_VENCOL::COL_ACCOUNT(LOOP%) = "") OR &
			(COMP_STRING(TRM$(GL_YYYY_PP::ACCT), &
			GL_VENCOL::COL_ACCOUNT(LOOP%)) = 0%)
		THEN
			TEMP$ = TEMP$ + SPACE$(13%)
		ELSE
			TEMP$ = TEMP$ + &
				FORMAT$(GL_YYYY_PP::AMOUNT, &
				"#########.## ")
			COL_TOTAL(LOOP%) = COL_TOTAL(LOOP%) + &
				GL_YYYY_PP::AMOUNT
			COL_TOTAL1(LOOP%) = COL_TOTAL1(LOOP%) + &
				GL_YYYY_PP::AMOUNT
			COL_TOTAL2(LOOP%) = COL_TOTAL2(LOOP%) + &
				GL_YYYY_PP::AMOUNT
			COL_TOTAL2_FLAG%(LOOP%) = -1%
		END IF
	NEXT LOOP%

	! Write out line

	IF GL_VENCOL::COL_FLAG = "D"
	THEN
		TEXT$ = GL_YYYY_PP::ACCT + " " + &
			GL_YYYY_PP::XREFNO + " " + &
			GL_YYYY_PP::DESCR + " " + &
			PRNT_DATE(GL_YYYY_PP::TRANDAT, 8%) + " " + &
			TEMP$

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO 4400 IF UTL_REPORTX::STAT
	END IF

	GOTO 4200

	!
	! Print out grand totals for this one store
	!
 StoreTotal:
	IF THIS_FLAG%
	THEN
		TEMP1$ = SPACE$(19%) + "Store " + &
			LEFT(THIS_GROUP$, 18%)
		TEMP1$ = TEMP1$ + SPACE$(72% - LEN(TEMP1$))

		FOR LOOP% = 1% TO MAX_COLUMNS
			IF GL_VENCOL::COL_ACCOUNT(LOOP%) = ""
			THEN
				TEMP1$ = TEMP1$ + "             "
			ELSE
				TEMP1$ = TEMP1$ + &
					FORMAT$(COL_TOTAL1(LOOP%), &
					"#########.## ")
			END IF
		NEXT LOOP%

		IF (GL_VENCOL::COL_FLAG <> "S")
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -5%)
		END IF

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEMP1$, -5%)

		IF (GL_VENCOL::COL_FLAG <> "S")
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)
		END IF
		GOTO 4400 IF UTL_REPORTX::STAT

		COL_TOTAL1(LOOP%) = 0.0 FOR LOOP% = 1% TO MAX_COLUMNS
	END IF

	RETURN

	!
	! Print out totals for Summary group
	!
 SFlagTotal:
	IF THIS_ITEM$ <> ""
	THEN
		TEXT$ = SPACE$(18%) + " " + &
			MID(THIS_ITEM$, 19%, 10%) + " " + &
			RIGHT(THIS_ITEM$, 29%) + " " + &
			SPACE$(10%) + " "

		FOR LOOP% = 1% TO MAX_COLUMNS
			IF COL_TOTAL2_FLAG%(LOOP%) = 0%
			THEN
				TEXT$ = TEXT$ + "             "
			ELSE
				TEXT$ = TEXT$ + &
					FORMAT$(COL_TOTAL2(LOOP%), &
					"#########.## ")
			END IF
		NEXT LOOP%

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO 4400 IF UTL_REPORTX::STAT
	END IF

	THIS_ITEM$ = TEMP_FORMAT::STORE + &
		GL_YYYY_PP::XREFNO + &
		GL_YYYY_PP::DESCR

	COL_TOTAL2(LOOP%) = 0.0 FOR LOOP% = 1% TO MAX_COLUMNS
	COL_TOTAL2_FLAG%(LOOP%) = 0% FOR LOOP% = 1% TO MAX_COLUMNS

	RETURN

4390	!
	! Finish up
	!
	IF (GL_VENCOL::COL_FLAG = "X")
	THEN
		GOSUB SFlagTotal
	END IF

	GOSUB StoreTotal

	TEXT$ = "             Grand Totals" + SPACE$(45%)
	FOR LOOP% = 1% TO MAX_COLUMNS
		IF GL_VENCOL::COL_ACCOUNT(LOOP%) = ""
		THEN
			TEXT$ = TEXT$ + "             "
		ELSE
			TEXT$ = TEXT$ + &
				FORMAT$(COL_TOTAL(LOOP%), &
				" #########.##")
		END IF
	NEXT LOOP%

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 30000%)
	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

4400	CALL OUTP_FINISH(UTL_REPORTX)

	GOTO ExitProgram

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************
	!
	! Exit to the menu
	!
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 Repaint:
	!******************************************************************
	! Repaint the screen
	!******************************************************************

	DATA	2, 1, "(01) Period", &
		4,16, "Title A/B     Account Wildcard", &
		5, 1, "(02) Column 1", &
		7, 1, "(03) Column 2", &
		9, 1, "(04) Column 3", &
		11, 1, "(05) Column 4", &
		13, 1, "(06) Column 5", &
		15, 1, "(07) Column 6", &
		17, 1, "(08) Det/Summ", &
		18, 1, "(09) Title", &
		0, 0, ""

	RESTORE
	READ XLONG, YLONG, ATEXT$

	WHILE XLONG
		SMG_STATUS% = SMG$PUT_CHARS(SMG_QUERY%, ATEXT$, XLONG, YLONG)
		READ XLONG, YLONG, ATEXT$
	NEXT

	FLAG% = 1%
	GOSUB DataEntry FOR LOOP% = 1% TO MAX_ITEM

	RETURN

	%PAGE

 DataEntry:
	!******************************************************************
	! Enter/Display items
	!******************************************************************

	TEMP$ = TRM$(SCOPE::PRG_ITEM)

	SCOPE::PRG_ITEM = "FLD" + FORMAT$(LOOP%, "<0>##")

	SELECT LOOP%

	CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Period\*
	!	.b
	!	.lm +5
	!	The ^*Period\* field
	!	enters the accounting
	!	period relative to the report to be printed.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	! Required:
	!--
		GL_VENCOL::FROMPER = ENTR_3STRING(SCOPE, SMG_QUERY%, &
			"2;16", "Period", &
			GL_VENCOL::FROMPER, FLAG%, "'E", DEFLT$)

	CASE 2% TO 7%

	!++
	! Abstract:FLD002
	!	^*(02) Column 1 - (07) Column 6\*
	!	.b
	!	.lm +5
	!	Each of the six (6) "Column" fields provide a means to
	!	enter a two line column heading and a General Ledger
	!	account(s) selection.
	!	.b
	!	The heading lines accommodate up to eleven (11) characters. The
	!	right-most character position aligns in it's respective column
	!	directly above the right-most position of the amounts displayed.
	!	.b
	!	The account(s) selection can be made by entering a specific General
	!	Ledger account, a series of accounts separated by commas, or by
	!	utilizing wildcarding techniques.
	!	.lm -5
	!
	! Index:
	!
	! Required:
	!--

		GL_VENCOL::COL_TITLEA(LOOP% - 1%) = &
			ENTR_3STRING(SCOPE, SMG_QUERY%, &
			NUM1$(LOOP% * 2% + 1%) + ";16", "Top Title", &
			GL_VENCOL::COL_TITLEA(LOOP% - 1%), FLAG%, "'E", DEFLT$)

		GL_VENCOL::COL_TITLEB(LOOP% - 1%) = &
			ENTR_3STRING(SCOPE, SMG_QUERY%, &
			NUM1$(LOOP% * 2% + 2%) + ";16", "Top Title", &
			GL_VENCOL::COL_TITLEB(LOOP% - 1%), FLAG%, "'E", DEFLT$)

		GL_VENCOL::COL_ACCOUNT(LOOP% - 1%) = &
			ENTR_3STRING(SCOPE, SMG_QUERY%, &
			NUM1$(LOOP% * 2% + 1%) + ";31", "Top Title", &
			GL_VENCOL::COL_ACCOUNT(LOOP% - 1%), FLAG%, "'E", DEFLT$)

	CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) Detail/Summary\*
	!	.b
	!	.lm +5
	!	The ^*Detail/Summary\* field
	!	controls the level of detail
	!	which will be displayed.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*D\* - Detail
	!	.te
	!	^*S\* - Summary
	!	.end table
	!	The Detail report lists for each selection each specific transaction
	!	in the General Ledger including specific account number, cross-reference,
	!	cross-reference description and date of the transaction. There is a page
	!	break after the information is printed for a specific location.
	!	.b
	!	The Summary report lists a line item for each location and a total of
	!	all period transactions for each defined selection.
	!	.lm -5
	!
	! Index:
	!
	! Required:
	!--
		GL_VENCOL::COL_FLAG = ENTR_3STRINGLIST(SCOPE, &
			SMG_QUERY%, "17;16", &
			"Code (D/S)", GL_VENCOL::COL_FLAG, FLAG%, &
			"'", DEFLT$, EC$(), ECTITLE$, "007")

	CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Title\*
	!	.b
	!	.lm +5
	!	The ^*Title\* field allows entry of a
	!	title for a specific report.
	!	.b
	!	Forty spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	! Required:
	!--
		GL_VENCOL::COL_TITLE = ENTR_3STRING(SCOPE, &
			SMG_QUERY%, "18;16", "Title", &
			GL_VENCOL::COL_TITLE, FLAG%, "'E", DEFLT$)

	END SELECT

	SCOPE::PRG_ITEM = TEMP$

	RETURN

	%PAGE

 ListKeyChoices:
4500	!
	! If the user presses <ListChoices>, list all of the Keys in the file
	!

	!
	! Tell them to hold on a minute
	!
	CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

	!
	! Reset the file so that we can list all of the Keys
	!
	WHEN ERROR IN
		RESET #GL_VENCOL.CH%
	USE
		CONTINUE ListArray IF ERR = 11%
		FILENAME$ = "GL_VENCOL"
		CONTINUE HelpError
	END WHEN

	LOOP% = 0%

 GETLoop:
	!
	! Get the next record
	!
	WHEN ERROR IN
		GET #GL_VENCOL.CH%, REGARDLESS
	USE
		CONTINUE ListArray IF ERR = 11%
		FILENAME$ = "GL_VENCOL"
		CONTINUE HelpError
	END WHEN

	!
	! Put the Key into the array set aside for this
	!
	LOOP% = LOOP% + 1%
	CHOICES$(LOOP%) = GL_VENCOL::RECKEY

	!
	! Go back to get the next record
	!
	GOTO GETLoop

 ListArray:
	!
	! List array of Keys
	!
	CALL ENTR_3MESSAGE(SCOPE, " ", 1%)

	SELECTED% = ENTR_3CHOICE(SCOPE, ABLE.POS$, NUM1$(LOOP%) + ";12", &
		CHOICES$(), &
		"", 128% + 8% + 2%, "Key Values", "", 1%)

	GL_VENCOL::RECKEY = CHOICES$(SELECTED%)

 ListKeyReturn:
	RETURN

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

	!
	! Trap untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32000	!******************************************************************
	! End of program GL_RPRT_VENCOL
	!******************************************************************
	END
