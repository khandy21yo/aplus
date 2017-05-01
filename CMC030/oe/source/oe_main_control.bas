1	%TITLE "Order Entry Control File"
	%SBTTL "OE_MAIN_CONTROL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_MAIN_CONTROL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1990 BY
	!
	! Computer Management Center
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
	!	The ^*Order Entry Control File\* displays and
	!	changes to the last order number and reflects the last purge
	!	date, the system status flag and the backorder aging dates/titles.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_MAIN_CONTROL/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN OE_MAIN_CONTROL
	!	$ DELETE OE_MAIN_CONTROL.OBJ;*
	!
	! Author:
	!
	!	06/25/90 - Lance Williams
	!
	! Modification history:
	!
	!	12/12/90 - Val James Allen
	!		Modified to add activity status flag (0 or 1)
	!
	!	07/15/91 - Val James "Hurt me harder" Allen
	!		Added backorder aging stuff.
	!
	!	09/10/91 - Jeff Beard
	!		add last memo and last invoice #'s
	!
	!	09/16/91 - Dan Perkins
	!		Modified error trapping.
	!
	!	04/09/92 - Dan Perkins
	!		Changed from ENTR_NUMBER to ENTR_3STRING function.
	!		ENTR_3STRING is RSET by using (MVALUE OR 2%).
	!
	!	04/30/92 - Dan Perkins
	!		Added more fields to accomodate changes in file
	!		layout.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	12/15/92 - Dan Perkins
	!		Changed field 9 to ask if user wants to display
	!		balances.  If this field is set to "Yes" then
	!		the customer aging balances and the inventory
	!		balances will be displayed.
	!
	!	04/03/93 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Lose lines 760, 770 (Dead code)
	!
	!	11/21/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include Files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:PC_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_CONTROL.HB"
	MAP (OE_CONTROL)	OE_CONTROL_CDD		OE_CONTROL
	MAP (OE_CONTROL_OLD)	OE_CONTROL_CDD	OE_CONTROL_OLD, OE_CONTROL2

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRCTYPE.HB"
	MAP (PC_PRCTYPE)	PC_PRCTYPE_CDD		PC_PRCTYPE

	!
	! Common Statements
	!
	COM (CH_OE_CONTROL) &
		OE_CONTROL.CH%, &
		OE_CONTROL.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	CASE OPT_INIT

		!******************************************************************
		! Set up information
		!******************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Order Entry Control File"
		SMG_WINDOW::NHELP = "OE_MAIN_CONTROL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 128% !Relative file
		SMG_WINDOW::NITEMS= 19%

		SMG_WINDOW::NKEYS = 0%

		COM (FRM) FRM$(15%)

		CALL READ_DEFAULTS(SMG_WINDOW)
700		!
		! Declare channels
		!
		IF OE_CONTROL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF OE_CONTROL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_CONTROL.CRE"
		USE
			CONTINUE 790 IF ERR = 7% OR ERR = 16%
			EXIT HANDLER
		END WHEN

		OE_CONTROL.READONLY% = 0%
		GOTO 790

 !760
		!
		! If unable to open for modify, try to open
		! with read access only.
		!
 !		%INCLUDE "SOURCE:[OE.OPEN]OE_CONTROL.OPN"
 !		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
 !		OE_CONTROL.READONLY% = -1%
 !
 !		GOTO 790
 !
 !770
		!
		! File not able to open, so reset channel
		!
 !		CALL ASSG_FREECHANNEL(OE_CONTROL.CH%)
 !		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = OE_CONTROL.CH%
		GOSUB 28000


	!
	! Select function
	!
	CASE OPT_OPTLIST

		MVALUE = "Change Blank Help eXit "

	!
	! Display window background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	01,  09, "(01) Last Order #", &
			02,  09, "(02) Last Close/Purge Date", &
			03,  09, "(03) Activity Status Flag", &
			04,  09, "(04) Last Credit Memo #", &
			05,  09, "(05) Last Invoice #", &
			06,  09, "(06) Misc Charges Price Type", &
			07,  09, "(07) List Price Type", &
			08,  09, "(08) Display Price", &
			09,  09, "(09) Display Balances", &
			14,  09, "First  (10)", &
			14,  25, "(11)", &
			15,  09, "Second (12)", &
			15,  25, "(13)", &
			16,  09, "Third  (14)", &
			16,  25, "(15)", &
			17,  09, "Fourth (16)", &
			17,  25, "(17)", &
			18,  09, "Fifth  (18)", &
			18,  25, "(19)", &
			11,  09, "***********Backorder Aging***********", &
			12,  09, "Period     Days      Title", &
			13,  09, "------     ----      ----------------", &
			0,   0, ""

		RESTORE

		READ XPOS%, YPOS%, XSTR$
		I% = 0%
		WHILE (XPOS% <> 0%)
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS%, YPOS%) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
			READ XPOS%, YPOS%, XSTR$
		NEXT

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	.x Order Number
	!	^*(01) Last Order Number\*
	!	.b
	!	.lm +5
	!	The ^*Last Order Number\* field contains the number
	!	of the last order number entered in a journal entry.
	!	.lm -5
	!
	! Index:
	!	.x Last Order Number
	!
	!--
			OE_CONTROL::ORDNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"01;46", TEMP$, OE_CONTROL::ORDNUM, &
				MFLAG OR 2%, "~R 'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	.x Last Purge Date
	!	^*(02) Last Closed/Purge Date\*
	!	.b
	!	.lm +5
	!	The ^*Last Closed/Purge Date\* field contains the date in which
	!	the orders were closed and purged.
	!	.lm -5
	!
	! Index:
	!	.x Last Closed Date
	!
	!--
			OE_CONTROL::PURGDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;46", TEMP$, OE_CONTROL::PURGDATE, MFLAG, &
				"'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Activity Status Flag\*
	!	.b
	!	.lm +5
	!	The ^*Activity Status Flag\* field contains the flag which
	!	indicates that the processes are normal (flag = 0) or that
	!	the close/purge process is underway (flag = 1).
	!	No posting/updating of the Order register file will be allowed
	!	if the status flag is other than 0.
	!	.lm -5
	!
	! Index:
	!	.x Activity Status Flag
	!
	!--
			OE_CONTROL::STATUS_FLAG = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"03;46", TEMP$, &
				OE_CONTROL::STATUS_FLAG, MFLAG, "'E",MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Credit Memo Number
	!	^*(04) Last Credit Memo Number\*
	!	.b
	!	.lm +5
	!	The ^*Last Credit Memo Number\* contains the Memo
	!	number assigned for the Credit Memo form.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CONTROL::LAST_MEMO = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;46", TEMP$, &
				OE_CONTROL::LAST_MEMO, MFLAG, "'E",MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	.x Invoice Number
	!	^*(05) Last Invoice Number\*
	!	.b
	!	.lm +5
	!	The ^*Last Invoice Number\* field contains the Invoice
	!	number assigned to the Invoice Form.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CONTROL::LAST_INV = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;46", TEMP$, &
				OE_CONTROL::LAST_INV, MFLAG, "'E",MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	.x Miscellaneous Charge Price Type
	!	^*(06) Misc Charges Price Type\*
	!	.b
	!	.lm +5
	!	The ^*Miscellaneous Charges Price Type\* field enters
	!	the valid price type code as established in the Price
	!	Type description file. If a price is assigned to this price
	!	code, then it appears as a miscellaneous charge in the Order
	!	journal line file.
	!	.b
	!	The field will accept two characters.
	!	.b
	!	Valid Price Type codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CONTROL::MISCTYPE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;46", TEMP$, &
				OE_CONTROL::MISCTYPE, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, "V0") = 1%
				THEN
					OE_CONTROL::MISCTYPE = PC_PRCTYPE::CODE
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, "M")
				OE_CONTROL::MISCTYPE =  PC_PRCTYPE::CODE
				GOTO ReEnter
			END IF


			CASE 7%
	!++
	! Abstract:FLD007
	!	.x List Price Type
	!	^*(07) List Price Type\*
	!	.b
	!	.lm +5
	!	The ^*List Price Type\* field
	!	enters a valid price type code as established in the Price
	!	Type description file. If a price is assigned to this price
	!	code, then it appears as a list price on the Order form.
	!	.b
	!	Valid List Price Types may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CONTROL::LISTCODE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;46", TEMP$, &
				OE_CONTROL::LISTCODE, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, "V0") = 1%
				THEN
					OE_CONTROL::LISTCODE = PC_PRCTYPE::CODE
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, "M")
				OE_CONTROL::LISTCODE =  PC_PRCTYPE::CODE
				GOTO ReEnter
			END IF


			CASE 8%
	!++
	! Abstract:FLD008
	!	.x Display Price
	!	^*(08) Display Price\*
	!	.b
	!	.lm +5
	!	The ^*Display Price\* field indicates whether a table of prices
	!	should be displayed on the screen during Order Entry.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CONTROL::DSPLPRICE = ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"08;46", TEMP$, &
				OE_CONTROL::DSPLPRICE, MFLAG, "'E",MVALUE)

			CASE 9%
	!++
	! Abstract:FLD009
	!	.x Display Balance
	!	^*(09) Display Invoice Balance\*
	!	.b
	!	.lm +5
	!	The ^*Display Balance\* field indicates whether a table of
	!	customer aging balances and a table of inventory balances
	!	should be displayed on the screen during Order Entry.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_CONTROL::DSPLQTY = ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"09;46", TEMP$, &
				OE_CONTROL::DSPLQTY, MFLAG, "'E",MVALUE)

		CASE 10%
			SCOPE::PRG_ITEM = "FLD006PDT"
	!++
	! Abstract:FLD010PDT
	!	^*(10) - (19) Period, Days and Title\*
	!	.b
	!	.lm +5
	!	The ^*Period, Days and Title\* fields
	!	establishes the intervals (number of days) and related aging
	!	description as pertains to aging schedules of backorders.
	!	In the following example if the user chooses aging period
	!	three for the backorder report then the report will reflect
	!	those backorders between 11 days and 15 days old.
	!	.b
	!	Shown below is an example aging interval:
	!	.table 3,25
	!	.te
	!	^*Period######Days####Title\*
	!	.te
	!	First  (10) ^*5\*  (11) ^*1 to 5 days\*
	!	.te
	!	Second (12) ^*5\*  (13) ^*6 to 10 days\*
	!	.te
	!	Third  (14) ^*5\*  (15) ^*11 to 15 days\*
	!	.te
	!	Fourth (16) ^*15\* (17) ^*16 to 30 days\*
	!	.te
	!	Fifth  (18) ^*0\*  (19) ^*31 days and over\*
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Period>Control
	!	.x Title>Control
	!	.x Days>Control
	!	.x Intervals>Control
	!	.X Control>Period
	!	.x Control>Title
	!	.x Control>Days
	!	.x Control>Intervals
	!
	!--
			OE_CONTROL::AGEPER(0) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;21", TEMP$, &
				OE_CONTROL::AGEPER(0) * 1.0, &
				MFLAG, "##", MVALUE)

		CASE 11%
			SCOPE::PRG_ITEM = "FLD011PDT"
			OE_CONTROL::AGENAM(0) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;30", TEMP$, &
				OE_CONTROL::AGENAM(0), &
				MFLAG, "'E", MVALUE)

		CASE 12%
			SCOPE::PRG_ITEM = "FLD012PDT"
			OE_CONTROL::AGEPER(1) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;21", TEMP$, &
				OE_CONTROL::AGEPER(1) * 1.0, &
				MFLAG, "##", MVALUE)

		CASE 13%
			SCOPE::PRG_ITEM = "FLD013PDT"
			OE_CONTROL::AGENAM(1) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;30", TEMP$, &
				OE_CONTROL::AGENAM(1), &
				MFLAG, "'E", MVALUE)

		CASE 14%
			SCOPE::PRG_ITEM = "FLD014PDT"
			OE_CONTROL::AGEPER(2) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;21", TEMP$, &
				OE_CONTROL::AGEPER(2) * 1.0, &
				MFLAG, "##", MVALUE)

		CASE 15%
			SCOPE::PRG_ITEM = "FLD015PDT"
			OE_CONTROL::AGENAM(2) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;30", TEMP$, &
				OE_CONTROL::AGENAM(2), &
				MFLAG, "'E", MVALUE)

		CASE 16%
			SCOPE::PRG_ITEM = "FLD016PDT"
			OE_CONTROL::AGEPER(3) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"17;21", TEMP$, &
				OE_CONTROL::AGEPER(3) * 1.0, &
				MFLAG, "##", MVALUE)

		CASE 17%
			SCOPE::PRG_ITEM = "FLD017PDT"
			OE_CONTROL::AGENAM(3)= &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"17;30", TEMP$, &
				OE_CONTROL::AGENAM(3), &
				MFLAG, "'E", MVALUE)

		CASE 18%
			SCOPE::PRG_ITEM = "FLD018PDT"
			OE_CONTROL::AGEPER(4) = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"18;21", TEMP$, &
				OE_CONTROL::AGEPER(4) * 1.0, &
				MFLAG, "##", MVALUE)

		CASE 19%
			SCOPE::PRG_ITEM = "FLD019PDT"
			OE_CONTROL::AGENAM(4) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"18;30", TEMP$, &
				OE_CONTROL::AGENAM(4), &
				MFLAG, "'E", MVALUE)


		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Set OE_CONTROL_OLD value
	!
20500	CASE OPT_SETOLD
		OE_CONTROL_OLD = OE_CONTROL

	!
	! Restore OE_CONTROL_OLD value
	!
	CASE OPT_RESETOLD
		OE_CONTROL = OE_CONTROL_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_CONTROL2 = OE_CONTROL

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_CONTROL = OE_CONTROL2

	END SELECT


	!
	! Exit the Function
	!
 ExitFunction:
	EXIT FUNCTION

28000	!
	! Get control record
	!
	WHEN ERROR IN
		GET #OE_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		CONTINUE 28030 IF ERR = 155%
		EXIT HANDLER
	END WHEN

	GOTO 28040

28030	!
	! Load in defaults for control file
	!
	OE_CONTROL::ORDNUM	= SPACE$(LEN(OE_CONTROL::ORDNUM))
	OE_CONTROL::PURGDATE	= SPACE$(LEN(OE_CONTROL::PURGDATE))
	OE_CONTROL::STATUS_FLAG = "0"
	OE_CONTROL::LAST_MEMO   = SPACE$(LEN(OE_CONTROL::LAST_MEMO))
	OE_CONTROL::LAST_INV    = SPACE$(LEN(OE_CONTROL::LAST_INV))
	OE_CONTROL::AGEPER(0) = 5%
	OE_CONTROL::AGENAM(0) = "1 to 5 days"
	OE_CONTROL::AGEPER(1) = 5%
	OE_CONTROL::AGENAM(1) = "6 to 10 days"
	OE_CONTROL::AGEPER(2) = 5%
	OE_CONTROL::AGENAM(2) = "11 to 15 days"
	OE_CONTROL::AGEPER(3) = 15%
	OE_CONTROL::AGENAM(3) = "16 to 30 days"
	OE_CONTROL::AGEPER(4) = 0%
	OE_CONTROL::AGENAM(4) = "31 days and over"

	WHEN ERROR IN
		PUT #OE_CONTROL.CH%
	USE
		CONTINUE 32767 IF ERR = 5%
		EXIT HANDLER
	END WHEN

28040	RETURN

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
