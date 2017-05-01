1	%TITLE "Maintain List of Customer Contact"
	%SBTTL "AP_MAIN_CONTACT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG AP_MAIN_CONTACT(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	!	The ^*Customer Contact\* program maintains the customer contact file.
	!	.lm -5
	!
	! Index:
	!	.x Customer>Contact
	!	.x Contact>Customer
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_MAIN_CONTACT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN AP_MAIN_CONTACT
	!	$ DELETE AP_MAIN_CONTACT.OBJ;*
	!
	! Author:
	!
	!	03/20/88 - Robert Peterson
	!
	! Modification history:
	!
	!	05/24/88 - Aaron Redd
	!		Modified to allow R/O open of file if R/W open fails.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/19/95 - Kevin Handy
	!		Fix format parameter in ENTR_3PHONE.
	!
	!	10/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/20/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/20/98 - Kevin Handy
	!		Lose SLINE field in array of pointers, since it
	!		isn't used by anything.
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/09/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTACT.HB"
	MAP	(AP_CONTACT)		AP_CONTACT_CDD	AP_CONTACT
	MAP	(AP_CONTACT_OLD)	AP_CONTACT_CDD	AP_CONTACT_OLD, AP_CONTACT2

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)		AP_VENDOR_CDD	AP_VENDOR

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_AP_CONTACT) &
		AP_CONTACT.CH%, &
		AP_CONTACT.READONLY%

	!
	! Create array to contain pointers and totals
	!
	RECORD RARRAY_RECORD
		RFA	LINRFA		! Rfa pointer for record
	END RECORD

	MAP (TT_AP_CONTACT) RARRAY_RECORD RARRAY(300%)	! Allocate for 300

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
		SMG_WINDOW::DESCR = "Contact Maintenance"
		SMG_WINDOW::NHELP = "AP_MAIN_CONTACT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 9%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 9%
		SMG_WINDOW::NITEMS= 4%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::TOPLIN = 3%
		SMG_WINDOW::BOTLIN = 8%

		!
		! Load in defaults for CONTACT
		!
		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF AP_CONTACT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF AP_CONTACT.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_CONTACT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			AP_MAIN_CONTACT = ERR
			CONTINUE 770
		END WHEN

		AP_CONTACT.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[AP.OPEN]AP_CONTACT.OPN"
		USE
			AP_MAIN_CONTACT = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		AP_CONTACT.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(AP_CONTACT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = AP_CONTACT.CH%
		WHEN ERROR IN
			RESET #AP_CONTACT.CH%
			GET #AP_CONTACT.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

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
			"  (01)" + SPACE$(29%) + "(02)" + SPACE$(17%) + &
			"(03)" + SPACE$(11%) + "(04)   ", &
			1%, 1%, , SMG$M_REVERSE)
		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			"  Contact Name                     Title" + &
			SPACE$(16%) + "Phone          Ext     ", &
			2%, 1%, , SMG$M_REVERSE)

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

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
			FORMAT$(SMG_WINDOW::TOTREC, "###") + SPACE$(60%), &
			SMG_WINDOW::VSIZE, 1%, , SMG$M_REVERSE)

		!
		! Paint lines on screen
		!
		FOR I% = 1% TO 3%

			A% = VAL%(MID("035,056,071", I% * 4% - 3%, 3%))

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

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

		XLINE$ = NUM1$(SMG_WINDOW::CURLIN)

 E0Loop:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Contact Name\*
	!	.b
	!	.lm +5
	!	The ^*Contact Name\* field refers to the name of the person
	!	to contact concerning the Accounts Payable.
	!	.lm -5
	!
	! Index:
	!	.x Contact>Name
	!	.x Name>Contact
	!
	!--
			AP_CONTACT::CONTACT_NAME = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				XLINE$ + ";3", TEMP$, &
				AP_CONTACT::CONTACT_NAME, MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Title\*
	!	.b
	!	.lm +5
	!	The ^*Title\* field enters the company title of the contact
	!	name. For example, the title field may contain President, Purchasing Manager,
	!	etc.
	!	.lm -5
	!
	! Index:
	!	.x Title>Contact
	!	.x Contact>Title
	!
	!--
			AP_CONTACT::TITLE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";36", TEMP$, &
				AP_CONTACT::TITLE, MFLAG, &
				"'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Phone\*
	!	.b
	!	.lm +5
	!	The ^*Phone\* field enters the phone number where the
	!	contact person may be reached.
	!	.lm -5
	!
	! Index:
	!	.x Phone>Contact
	!	.x Contact>Phone
	!
	!--
			AP_CONTACT::PHONE = ENTR_3PHONE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";57", TEMP$, &
				AP_CONTACT::PHONE, MFLAG, &
				0%, MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Extension\*
	!	.b
	!	.lm +5
	!	The ^*Extension\* field enters the number of the phone
	!	extension where the contact person may be reached.
	!	.lm -5
	!
	! Index:
	!	.x Extension>Contact
	!	.x Contact>Extension
	!
	!--
			AP_CONTACT::EXTENSION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				XLINE$ + ";72", TEMP$, &
				AP_CONTACT::EXTENSION, MFLAG, &
				"'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		AP_MAIN_CONTACT = 0%

		SELECT MLOOP
		CASE 1%
			IF AP_CONTACT::CONTACT_NAME = ""
			THEN
				AP_MAIN_CONTACT = -1
			END IF
		END SELECT

	!
	! Set AP_CONTACT_OLD value
	!
20500	CASE OPT_SETOLD
		AP_CONTACT_OLD = AP_CONTACT

	!
	! Restore AP_CONTACT_OLD value
	!
	CASE OPT_RESETOLD
		AP_CONTACT = AP_CONTACT_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		AP_CONTACT2 = AP_CONTACT

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		AP_CONTACT = AP_CONTACT2

		!
		! Set special default values for the key of a new record
		! which is the most likely case when this function is to
		! be called.
		!
		AP_CONTACT::CUSNUM = AP_VENDOR::VENNUM

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #AP_CONTACT.CH%, &
				KEY #0% GE AP_VENDOR::VENNUM + "", &
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
					KEY #0% GE AP_VENDOR::VENNUM + "", &
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

			!
			! Add information to array
			!
			IF (AP_VENDOR::VENNUM = AP_CONTACT::CUSNUM)
			THEN
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
			FOR I% = MFLAG TO SMG_WINDOW::TOTREC - 1%
				RARRAY(I%) = RARRAY(I% + 1%)
			NEXT I%

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
		! new key is probibly passes through MVALUE, unless some
		! other means is devised.
		!
		CASE 6%
			AP_CONTACT::CUSNUM = MID(MVALUE, 2%, 10%)

		END SELECT
	END SELECT

28000	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
