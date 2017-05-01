1	%TITLE "POS Controlling File"
	%SBTTL "PS_MAIN_CONTROL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PS_MAIN_CONTROL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	The ^*Controlling File\* screen is used to display and allow
	!	changes to the last ticket number and reflects the last purge
	!	date, the system status flag and the last invoice number.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PS_SOURCE:PS_MAIN_CONTROL/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PS_MAIN_CONTROL
	!	$ DELETE PS_MAIN_CONTROL.OBJ;*
	!
	! Author:
	!
	!	11/02/91 - Frank F. Starman
	!
	! Modification history:
	!
	!	01/14/92 - Frank F. Starman
	!		Added fields.
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	02/20/92 - Frank F. Starman
	!		Add the list price type
	!
	!	05/04/92 - Dan Perkins
	!		Added more fields to accomodate changes in file
	!		layout.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/17/94 - Kevin Handy
	!		Modifications to handle additional information for
	!		two miscelanous codes with flags, instead of one
	!		with no flags.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	06/02/95 - Kevin Handy
	!		Changed title of field from "Exclude customer
	!		category" to "Exclude product category", due to
	!		change in operation of field.
	!
	!	06/02/95 - Kevin Handy
	!		Change titles "Misc charges ..." to "Misc (1) ..."
	!		Un-abbreviate words in display.
	!
	!	11/14/95 - Kevin Handy
	!		Reformat source close to 80 columns.
	!
	!	10/25/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Lose lines 760, 770 (Dead code)
	!
	!	12/07/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PS.OPEN]PS_CONTROL.HB"
	MAP (PS_CONTROL)	PS_CONTROL_CDD		PS_CONTROL
	MAP (PS_CONTROL_OLD)	PS_CONTROL_CDD	PS_CONTROL_OLD, PS_CONTROL2

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRCTYPE.HB"
	MAP (PC_PRCTYPE)	PC_PRCTYPE_CDD		PC_PRCTYPE

	! Common Statements
	!
	COM (CH_PS_CONTROL) &
		PS_CONTROL.CH%, &
		PS_CONTROL.READONLY%

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

		!**************************************************************
		! Set up information
		!**************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "POS Controlling Record"
		SMG_WINDOW::NHELP = "PS_MAIN_CONTROL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 128% !Relative file
		SMG_WINDOW::NITEMS= 13%

		SMG_WINDOW::NKEYS = 0%

		CALL READ_DEFAULTS(SMG_WINDOW)
700		!
		! Declare channels
		!
		IF PS_CONTROL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PS_CONTROL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PS.OPEN]PS_CONTROL.CRE"
		USE
			CONTINUE 790 IF ERR = 7% OR ERR = 16%
			EXIT HANDLER
		END WHEN

		PS_CONTROL.READONLY% = 0%
		GOTO 790

 !760
		!
		! If unable to open for modify, try to open
		! with read access only.
		!
 !		%INCLUDE "SOURCE:[PS.OPEN]PS_CONTROL.OPN"
 !		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
 !		PS_CONTROL.READONLY% = -1%
 !
 !		GOTO 790
 !
 !770
		!
		! File not able to open, so reset channel
		!
 !		CALL ASSG_FREECHANNEL(PS_CONTROL.CH%)
 !		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = PS_CONTROL.CH%
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

		DATA	02,  09, "(01) Last Ticket #", &
			03,  09, "(02) Last Close/Purge Date", &
			04,  09, "(03) Activity Status Flag", &
			05,  09, "(04) List Price Type", &
			06,  09, "(05) Display Price", &
			07,  09, "(06) Display Invoice Balance", &
			08,  09, "(07) Exclude Product Category", &
			09,  09, "(08) Misc (1) Price Type", &
			10,  09, "(09) Misc (1) Taxable", &
			11,  09, "(10) Misc (1) Exempt", &
			12,  09, "(11) Misc (2) Price Type", &
			13,  09, "(12) Misc (2) Taxable", &
			14,  09, "(13) Misc (2) Exempt", &
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
	!	.x Ticket Number
	!	^*(01) Last Ticket Number\*
	!	.b
	!	.lm +5
	!	The ^*Last Ticket Number\* field assigns an order
	!	number for the journal entry.
	!	.B
	!	The field will accommodate ten (10) characters.
	!	.lm -5
	!
	! Index:
	!	.x Last Ticket Number
	!
	!--

			PS_CONTROL::LAST_TICKET = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"02;46", TEMP$, &
				PS_CONTROL::LAST_TICKET, MFLAG OR 2%, &
				"~R 'E", MVALUE)


		CASE 2%

	!++
	! Abstract:FLD002
	!	.x Last Purge Date
	!	^*(02) Last Closed/Purge Date\*
	!	.b
	!	.lm +5
	!	The ^*Last Closed/Purge Date\* field contains the date in which
	!	the tickets were closed and purged.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Last Closed Date
	!
	!--

			PS_CONTROL::PURGDATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;46", TEMP$, PS_CONTROL::PURGDATE, MFLAG, &
				"'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Activity Status Flag\*
	!	.b
	!	.lm +5
	!	The ^*Activity Status Flag\* field contains the flag which
	!	indicates that the processes are normal (flag = 0) or that
	!	the Close/purge process is underway (flag = 1).
	!	No Posting/updating of the Ticket register file will be allowed
	!	if the status flag is other than 0.
	!	.lm -5
	!
	! Index:
	!	.x Activity Status Flag
	!
	!--

			PS_CONTROL::STATUS_FLAG = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;46", TEMP$, &
				PS_CONTROL::STATUS_FLAG, MFLAG, "'E",MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	.x List Price Type
	!	^*(04) List Price Type\*
	!	.b
	!	.lm +5
	!	The ^*List Price Type\* field enters a price
	!	type code as established in the price type description file.
	!	If a price is assigned to this price code it will appear
	!	as the list price on the ticket form.
	!	.b
	!	Valid price type codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	The field will accommodate two (2) characters.
	!	.lm -5
	!
	! Index:
	!
	!--

			PS_CONTROL::LISTCODE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;46", TEMP$, &
				PS_CONTROL::LISTCODE, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, "V0") = 1%
				THEN
					PS_CONTROL::LISTCODE = PC_PRCTYPE::CODE
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, "M")
				PS_CONTROL::LISTCODE =  PC_PRCTYPE::CODE
				GOTO ReEnter
			END IF

		CASE 5%

	!++
	! Abstract:FLD005
	!	.x Display Price
	!	^*(05) Display Price\*
	!	.b
	!	.lm +5
	!	The ^*Display Price\* field determines if
	!	a table of prices is to be displayed on the screen during
	!	Order Entry.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

			PS_CONTROL::DSPLPRICE = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;46", TEMP$, &
				PS_CONTROL::DSPLPRICE, MFLAG, "'E",MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	.x Display Inventory Balance
	!	^*(06) Display Invoice Balance\*
	!	.b
	!	.lm +5
	!	The ^*Display Invoice Balance\* field
	!	determines if a table of inventory balances should be displayed
	!	on the screen during Order Entry.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

			PS_CONTROL::DSPLQTY = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"7;46", TEMP$, &
				PS_CONTROL::DSPLQTY, MFLAG, "'E",MVALUE)



		CASE 7%

	!++
	! Abstract:FLD007
	!	.ts 55
	!	^*(07) Exclude Product Categories\*
	!	.b
	!	This field specifies which product categories are
	!	non-state sales taxable.
	!	Normally the category used for labor is defined here.
	!
	! Index:
	!	.x Exclude Product Category
	!
	!--

			PS_CONTROL::CUSBAL = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"8;46", TEMP$, &
				PS_CONTROL::CUSBAL, MFLAG, "'E",MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	.x Miscellaneous Charge Price Type
	!	^*(08) Misc Charges Price Type\*
	!	.b
	!	.lm +5
	!	The ^*Miscellaneous Charges Price Type\* field
	!	enters a valid price
	!	type code as established in the price type description file.
	!	If a price is assigned to this price code, it will then appear as
	!	a miscellaneous charge in the ticket journal line file.
	!	.b
	!	Valid price type codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	The field will accommodate two (2) characters.
	!	.LM -5
	!
	! Index:
	!
	!--

			PS_CONTROL::MISCTYPE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"09;46", TEMP$, &
				PS_CONTROL::MISCTYPE, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, "V0") = 1%
				THEN
					PS_CONTROL::MISCTYPE = PC_PRCTYPE::CODE
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, "M")
				PS_CONTROL::MISCTYPE =  PC_PRCTYPE::CODE
				GOTO ReEnter
			END IF

		CASE 9%

	!++
	! Abstract:FLD009
	!	.x Miscellaneous Charge Taxable
	!	^*(08) Misc Charges Taxable\*
	!	.b
	!	.lm +5
	!	This field specifies if the miscellaneous amount
	!	is subject to state sales tax.
	!	.LM -5
	!
	! Index:
	!
	!--

			PS_CONTROL::MISCTAXABLE = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;46", TEMP$, &
				PS_CONTROL::MISCTAXABLE, MFLAG, "'E", MVALUE)

		CASE 10%

	!++
	! Abstract:FLD010
	!	.x Miscellaneous Charge Exempt
	!	^*(10) Misc Charges Exempt\*
	!	.b
	!	.lm +5
	!	This field specifies which customer tax types, as specified in the
	!	customer master file, are not subject to
	!	the miscellaneous amount.
	!	The following categories are currently defined:
	!	.lm +5
	!	.b
	!	*1 Taxable
	!	.br
	!	*4 Resale
	!	.br
	!	*5 Out of State
	!	.br
	!	*6 Church, School, and Government.
	!	.lm -5
	!	.LM -5
	!
	! Index:
	!
	!--

			PS_CONTROL::MISCEXEMPT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;46", TEMP$, &
				PS_CONTROL::MISCEXEMPT, MFLAG, "'E", MVALUE)


		CASE 11%

	!++
	! Abstract:FLD011
	!	.x Miscellaneous Charge Price Type
	!	^*(11) Misc Charges (2) Price Type\*
	!	.b
	!	.lm +5
	!	The ^*Miscellaneous Charges (2) Price Type\* field
	!	enters a price
	!	type code as established in the price type description file.
	!	If a price is assigned to this price code, it will then appear as
	!	a miscellaneous charge in the ticket journal line file.
	!	.b
	!	Valid price type codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	The field will accommodate two (2) characters.
	!	.LM -5
	!
	! Index:
	!
	!--

			PS_CONTROL::MISC2TYPE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;46", TEMP$, &
				PS_CONTROL::MISC2TYPE, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, "V0") = 1%
				THEN
					PS_CONTROL::MISC2TYPE = PC_PRCTYPE::CODE
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(PC_MAIN_PRCTYPE.ID, "M")
				PS_CONTROL::MISC2TYPE =  PC_PRCTYPE::CODE
				GOTO ReEnter
			END IF

		CASE 12%

	!++
	! Abstract:FLD012
	!	.x Miscellaneous Charge (2) Taxable
	!	^*(12) Misc Charges (2) Taxable\*
	!	.b
	!	.lm +5
	!	This field specifies if the miscellanous type is subject to state sales
	!	tax.
	!	.LM -5
	!
	! Index:
	!
	!--

			PS_CONTROL::MISC2TAXABLE = &
				ENTR_3YESNO(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;46", TEMP$, &
				PS_CONTROL::MISC2TAXABLE, MFLAG, "'E", MVALUE)

		CASE 13%

	!++
	! Abstract:FLD013
	!	.x Miscellaneous Charge (2) Exempt
	!	^*(13) Misc Charges (2) Exempt\*
	!	.b
	!	.lm +5
	!	This field specifies which customer tax types, as specified in the
	!	customer master file, are not subject to
	!	the miscellaneous amount.
	!	The following categories are currently defined:
	!	.lm +5
	!	.b
	!	*1 Taxable
	!	.br
	!	*4 Resale
	!	.br
	!	*5 Out of State
	!	.br
	!	*6 Church, School, and Government.
	!	.lm -5
	!	.LM -5
	!
	! Index:
	!
	!--

			PS_CONTROL::MISC2EXEMPT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;46", TEMP$, &
				PS_CONTROL::MISC2EXEMPT, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Set PS_CONTROL_OLD value
	!
20500	CASE OPT_SETOLD
		PS_CONTROL_OLD = PS_CONTROL

	!
	! Restore PS_CONTROL_OLD value
	!
	CASE OPT_RESETOLD
		PS_CONTROL = PS_CONTROL_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		PS_CONTROL2 = PS_CONTROL

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		PS_CONTROL = PS_CONTROL2

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
		GET #PS_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		CONTINUE 28030 IF ERR = 155%
		EXIT HANDLER
	END WHEN

	GOTO 28040

28030	!
	! Load in defaults for control file
	!
	PS_CONTROL::LAST_TICKET	= SPACE$(LEN(PS_CONTROL::LAST_TICKET))
	PS_CONTROL::PURGDATE	= SPACE$(LEN(PS_CONTROL::PURGDATE))
	PS_CONTROL::STATUS_FLAG = "0"

	WHEN ERROR IN
		PUT #PS_CONTROL.CH%
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
