1	%TITLE "Employee Accrual Maintenance"
	%SBTTL "PR_MAIN_EMP_ACCRUAL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_EMP_ACCRUAL(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	! Abstract:HELP
	!	.p
	!
	! Index:
	!	.x Employee>Accrual
	!	.x Accrual>Employee
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_EMP_ACCRUAL/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_EMP_ACCRUAL
	!	$ DELETE PR_MAIN_EMP_ACCRUAL.OBJ;*
	!
	! Author:
	!
	!	12/26/91 - Kevin Handy
	!
	! Modification history:
	!
	!	01/04/91 - Kevin Handy
	!		Modified to access ACCRUAL_RATE journal.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	12/24/92 - Kevin Handy
	!		Added option to enter Accuue to GL.
	!		Initialized BATCH.
	!
	!	12/29/92 - Kevin Handy
	!		Modified to force GLFLAG to default to 'Y' instead
	!		of 'N'.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/22/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/08/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/14/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	07/26/2002 - Kevin Handy
	!		Don't let them go into the rate screen if they don't
	!		have any records in this screen.
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.HB"
	MAP	(PR_EMP_ACCRUAL)	PR_EMP_ACCRUAL_CDD	PR_EMP_ACCRUAL
	MAP	(PR_EMP_ACCRUAL_OLD)	PR_EMP_ACCRUAL_CDD	PR_EMP_ACCRUAL_OLD, PR_EMP_ACCRUAL2

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)		PR_EMP_MASTER_CDD	PR_EMP_MASTER

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_PR_EMP_ACCRUAL) &
		RARRAY_RECORD RARRAY(300%)	! Allocate for 300

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	! This common area must be mapped in some of the MAIN programs,
	! PR_MAST_EMPLOYEE.BAS, and PR_MAST_WC_WORK.BAS.
	!
	COM (CH_PR_EMP_ACCRUAL) &
		PR_EMP_ACCRUAL.CH%, &
		PR_EMP_ACCRUAL.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_JOURNAL

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!
	CASE OPT_INIT

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Employee accrual"
		SMG_WINDOW::NHELP = "PR_MAIN_EMP_ACCRUAL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 15%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 5%
		SMG_WINDOW::NITEMS= 8%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 2%
		SMG_WINDOW::BOTLIN = 14%

		!
		! Load in defaults for Status
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF PR_EMP_ACCRUAL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_EMP_ACCRUAL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_EMP_ACCRUAL = ERR
			CONTINUE 770
		END WHEN

		PR_EMP_ACCRUAL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_ACCRUAL.OPN"
		USE
			PR_MAIN_EMP_ACCRUAL = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_EMP_ACCRUAL.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_EMP_ACCRUAL.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = PR_EMP_ACCRUAL.CH%
		WHEN ERROR IN
			RESET #PR_EMP_ACCRUAL.CH%
			GET #PR_EMP_ACCRUAL.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! More options
	!
	CASE OPT_OPTLIST
		MVALUE = MVALUE + " raTe"

	!
	! Handle additional options
	!
	CASE OPT_MOREMENU
		SELECT EDIT$(MVALUE, -1%)

		!
		! Employee status
		!
		CASE "RATE"
	!++
	! Abstract:RATE
	!	.x Rate>Employee Accrual
	!	^*raTe\*
	!	.p
	!	The ^*raTe\* option
	!	accesses the screen were specific rate information relative to
	!	employee accruals can be entered.
	!	.p
	!	The Rate screen includes the following fields:
	!	.b
	!	.lm +5
	!	.list 0,"*"
	!	.le
	!	Start Date
	!	.le
	!	Minimum Hours
	!	.le
	!	Maximum Hours
	!	.le
	!	Rate
	!	.le
	!	Method
	!	.le
	!	Frequency
	!	.end list
	!	.lm -5
	!	For more information, see explanations in reference to each field.
	!
	! Index:
	!	.x Employee Accrual>Rate
	!
	!--
			IF SMG_WINDOW::TOTREC <> 0%
			THEN
				PR_MAIN_EMP_ACCRUAL = &
					MAIN_JOURNAL(PR_MAIN_EMP_ACCRUAL_RATE.ID, "")
			END IF

		END SELECT

	!
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			" (01) (02) HrsUna  (03) DlrUna  (04) HrsAva  " + &
			"(05) DlrAva  (06) (07)AvDate (08)", &
			1%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

 !->XX |#########.##|#########.##|#########.##|#########.##| X  |xx/xx/xxxx| X
 !0        1         2         3         4         5         6         7
 !123456789012345678901234567890123456789012345678901234567890123456789012345678

	!
	! Extra display stuff
	!
	CASE OPT_DISPLAY

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		!
		! Display totals
		!
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"Number of lines" + &
			FORMAT$(SMG_WINDOW::TOTREC, "####") + SPACE$(60%), &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 7%
			A% = VAL%(MID("006,019,032,045,058,063,074", &
				I% * 4% - 3%, 3%))

			SMG_STATUS% = SMG$DRAW_LINE(SMG_WINDOW::WNUMBER, &
				1%, A%, SMG_WINDOW::BOTLIN, A%)
		NEXT I%

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Type\*
	!	.p
	!	The ^*Type\* column refers to the specific
	!	accrual type as defined in the Pay Type definition file.
	!	.b
	!	Examples of possible types are:
	!	.b
	!	.lm +5
	!	.LIST 0,"*"
	!	.LE
	!	VA = Vacation
	!	.LE
	!	SP = Sick Pay
	!	.LE
	!	PL = Personal Leave
	!	.ELS
	!	.lm -5
	!	.p
	!	Types could be numeric or alphanumeric.
	!
	! Index:
	!	.x Employee Accrual>Type
	!	.x Type>Employee Accrual
	!
	!--

			PR_EMP_ACCRUAL::ATYPE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";3", TEMP$, &
				PR_EMP_ACCRUAL::ATYPE, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) HrsUna\*
	!	.p
	!	The ^*Hours Unavailable\* field indicates the
	!	number of hours which have been accrued, but are not yet available for an
	!	employee's use.  When the hours become available, they are transferred into the
	!	^*Hours Available\* field.
	!	.p
	!	This field are multiplied by an employee's pay rate.  The result
	!	is stored in the ^*Dollars Unavailable\* field.
	!	.p
	!	There will be no value stored in this field and the related ^*Dollars
	!	Unavailable\* field unless field ^*(06) Available Flag\* contains a "2".
	!	.p
	!	When an ^*Available Date\* is reached the "unavailable"
	!	fields will be transferred to the "available" fields.
	!
	! Index:
	!	.x Employee Accrual>Hours Unavailable
	!	.x Hours Unavailable>Employee Accrual
	!
	!--

			PR_EMP_ACCRUAL::HOURSUNA = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";7", TEMP$, &
				PR_EMP_ACCRUAL::HOURSUNA, MFLAG, "#########.##", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) DlrUna\*
	!	.p
	!	The c^*Dollars Unavailable\* field is the result of
	!	the ^*Hours Unavailable\* field multiplied by an employee's pay rate.
	!	.p
	!	See explanation relative to the ^*Hours Unavailable\* field for further
	!	information.
	!
	! Index:
	!	.x Employee Accrual>Dollar Unavailable
	!	.x Dollar Unavailable>Employee Accrual
	!
	!--

			PR_EMP_ACCRUAL::DOLLARUNA = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";20", TEMP$, &
				PR_EMP_ACCRUAL::DOLLARUNA, MFLAG, "#########.##", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) HrsAva\*
	!	.p
	!	The ^*Hours Available\* field indicates the number of accrued
	!	hours currently available to an employee.  The number of ^*Hours Available\* is
	!	multiplied by an employee's pay rate.  The result is stored in the ^*Dollars
	!	Available\* field.
	!	.p
	!	If the value of the ^*(06) Available Flag\* field is "1", the "available"
	!	fields are posted with accrued hours and dollars as they are accrued.  If the
	!	value of the ^*(06) Available Flag\* is "2", the "unavailable"
	!	fields are transferred to the "available" fields when the available date,
	!	stored in the ^*(07) Available Date\* field, is reached.
	!
	! Index:
	!	.x Employee Accrual>Hours Available
	!	.x Hours Available>Employee Accrual
	!
	!--

			PR_EMP_ACCRUAL::HOURSAVA = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";33", TEMP$, &
				PR_EMP_ACCRUAL::HOURSAVA, MFLAG, "#########.##", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) DlrAva\*
	!	.p
	!	The ^*Dollars Available\* field is the result of the ^*Hours
	!	Available\* multiplied by an employee's hourly pay rate.
	!	.p
	!	See the explanation relative to the ^*Hours Available\* for further
	!	information.
	!
	! Index:
	!	.x Employee Accrual>Dollars Available
	!	.x Dollars Available>Employee Accrual
	!
	!--

			PR_EMP_ACCRUAL::DOLLARAVA = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";46", TEMP$, &
				PR_EMP_ACCRUAL::DOLLARAVA, MFLAG, "#########.##", MVALUE)


		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) AvailFlag\*
	!	.p
	!	The ^*Available Flag\* indicates the method or criteria
	!	which determines when accrued hours relative to a fringe benefit will become
	!	available.
	!	.b
	!	Valid codes are:
	!	.lm +5
	!	.b
	!	.i -2
	!	1 Immediately Available.
	!	.b
	!	.i -2
	!	2 Unavailable until the month of the ^*Avail Date\* field,
	!	then becomes avaiable with the accrual adjustment post.
	!	Accrues into unavailable for another year.
	!	.b
	!	.i -2
	!	3 Unavailable until the date in the ^*Avail Date\* field,
	!	then becomes available with the accrual adjustment post.
	!	Accrues into available after that date.
	!	.lm -5
	!	.p
	!	The Avail[able] Date field could be an employee's
	!	anniversary date or the first day of a calendar year, for example.
	!
	! Index:
	!	.x Employee Accrual>Available Flag
	!	.x Available Flag>Employee Accruals
	!
	!--

			PR_EMP_ACCRUAL::AVAILFLAG = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";60", TEMP$, &
				PR_EMP_ACCRUAL::AVAILFLAG, MFLAG, "'", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Avail Date\*
	!	.p
	!	The ^*Available Date\* field can be used in
	!	cases when a company's policy dictates that though a fringe benefit may be
	!	accrued, the benefit does not become available to an employee until a
	!	specified date.  That date could be an employment anniversary date or the
	!	first day of a calendar year, for example.  If the ^*(06) Available Flag\*
	!	field contains a "2", the system will transfer the appropriate data from the
	!	"unavailable" fields to the "available" fields when the ^*Available Date\*
	!	occurs.  The system will read only the month and the day of the ^*Available
	!	Date\*, even though the initial year would be entered.
	!	.p
	!	If the ^*(06) Available Flag\* field contains a "1", the ^*Available Date\*
	!	field may be left blank.
	!
	! Index:
	!	.x Employee Accrual>Available Date
	!	.x Available Date>Employee Accrual
	!
	!--

			PR_EMP_ACCRUAL::AVAILDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";64", TEMP$, &
				PR_EMP_ACCRUAL::AVAILDATE, MFLAG, "'E", MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) Accrue to GL (Y/N)\*
	!
	! Index:
	!	.x Employee Accrual>Accrue to GL
	!	.x Accrue to GL>Employee Accrual
	!
	!--

			PR_EMP_ACCRUAL::GLFLAG = ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";76", TEMP$, &
				PR_EMP_ACCRUAL::GLFLAG, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		PR_MAIN_EMP_ACCRUAL = 0%

	!
	! Handle finishing various options specially
	!
	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM

		!
		! Erase record
		!
		CASE "Add"
			!
			! Erase any line items under the header
			!
			PR_MAIN_EMP_ACCRUAL = MAIN_JOURNAL(PR_MAIN_EMP_ACCRUAL_RATE.ID, "A")

		!
		! Change records
		!
		CASE "Change"
			!
			! Change line items to match new header
			! if the key was changed.
			!
			! The original record must be the one in the
			! MAP for this to be able to work.  The new
			! key is passed through the QUERY$ variable.
			!
			IF PR_EMP_ACCRUAL_OLD::EMPNUM <> PR_EMP_ACCRUAL::EMPNUM OR &
				PR_EMP_ACCRUAL_OLD::ATYPE <> PR_EMP_ACCRUAL::ATYPE
			THEN
				TEMP$ = PR_EMP_ACCRUAL::EMPNUM + PR_EMP_ACCRUAL::ATYPE
				PR_EMP_ACCRUAL = PR_EMP_ACCRUAL_OLD
				PR_MAIN_EMP_ACCRUAL = MAIN_JOURNAL(PR_MAIN_EMP_ACCRUAL_RATE.ID, "C" + TEMP$)
			END IF

		!
		! Erase record
		!
		CASE "Erase"
			!
			! Erase any line items under the header
			!
			PR_MAIN_EMP_ACCRUAL = MAIN_JOURNAL(PR_MAIN_EMP_ACCRUAL_RATE.ID, "E")

		END SELECT


	!
	! Set PR_EMP_ACCRUAL_OLD value
	!
20500	CASE OPT_SETOLD
		PR_EMP_ACCRUAL_OLD = PR_EMP_ACCRUAL

	!
	! Restore PR_EMP_ACCRUAL_OLD value
	!
	CASE OPT_RESETOLD
		PR_EMP_ACCRUAL = PR_EMP_ACCRUAL_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PR_EMP_ACCRUAL2 = PR_EMP_ACCRUAL

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PR_EMP_ACCRUAL = PR_EMP_ACCRUAL2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		PR_EMP_ACCRUAL::EMPNUM = PR_EMP_MASTER::EMPNUM
		PR_EMP_ACCRUAL::BATCH = ""
		PR_EMP_ACCRUAL::GLFLAG = "Y" IF PR_EMP_ACCRUAL::GLFLAG <> "N"

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #PR_EMP_ACCRUAL.CH%, &
				KEY #0% GE PR_EMP_ACCRUAL::EMPNUM + "", &
				REGARDLESS
		END SELECT

	!
	! Handle array of records
	!
27000	CASE OPT_ARRAY

		!
		! Select sub-option of array
		!
		SELECT MLOOP

		!
		! Load array with line items
		!
		CASE 1%

			!
			! Empty array
			!
			SMG_WINDOW::TOTREC = 0%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% GE PR_EMP_MASTER::EMPNUM + "", &
					REGARDLESS
			USE
				CONTINUE 28000
			END WHEN

27120			!
			! Get a record
			!
			WHEN ERROR IN
				GET #SMG_WINDOW::CHAN
			USE
				CONTINUE 28000 IF ERR = 11%
				EXIT HANDLER
			END WHEN

			IF PR_EMP_ACCRUAL::EMPNUM = PR_EMP_MASTER::EMPNUM
			THEN
				!
				! Add information to array
				!
				SMG_WINDOW::TOTREC = SMG_WINDOW::TOTREC + 1%
				RARRAY(SMG_WINDOW::TOTREC)::LINRFA = &
					GETRFA(SMG_WINDOW::CHAN)
				GOTO 27120
			END IF

		!
		! Remove one element of the array
		!
		CASE 2%
			!
			! Remove item pointed to by Mflag
			!
			RARRAY(I%) = RARRAY(I% + 1%) &
				FOR I% = MFLAG TO SMG_WINDOW::TOTREC - 1%

		!
		! Set array item to current record
		!
		CASE 3%
			RARRAY(MFLAG)::LINRFA = GETRFA(SMG_WINDOW::CHAN)

		!
		! Load in current record, locked
		!
		CASE 4%
27200			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA

		!
		! Load in current record, unlocked
		!
		CASE 5%
			GET #SMG_WINDOW::CHAN, RFA RARRAY(MFLAG)::LINRFA, &
				REGARDLESS

		!
		! Change the current record's key to match header.  The
		! new key probably passes through MVALUE, unless some
		! other means is devised.
		!
		CASE 6%
			PR_EMP_ACCRUAL::EMPNUM = RIGHT(MVALUE, 2%)

		!
		! Print descriptions in journal window.
		!
		CASE 7%
			! Nop right now

		END SELECT
	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
