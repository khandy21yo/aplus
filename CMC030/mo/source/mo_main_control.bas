1	%TITLE "Manufacture Order Control File"
	%SBTTL "MO_MAIN_CONTROL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MO_MAIN_CONTROL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
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
	!	change to last order number and the following:
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_MAIN_CONTROL/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN MO_MAIN_CONTROL
	!	$ DELETE MO_MAIN_CONTROL.OBJ;*
	!
	! Author:
	!
	!	03/05/91 - Frank F. Starman
	!
	! Modification history:
	!
	!	10/07/92 - Dan Perkins
	!		Added LAST_INV field.
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Lose line 760, 770 (Dead code)
	!
	!	12/12/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[MO.OPEN]MO_CONTROL.HB"
	MAP (MO_CONTROL)	MO_CONTROL_CDD		MO_CONTROL
	MAP (MO_CONTROL_OLD)	MO_CONTROL_CDD		MO_CONTROL_OLD, MO_CONTROL2

	!
	! Common Statements
	!
	COM (CH_MO_CONTROL) &
		MO_CONTROL.CH%, &
		MO_CONTROL.READONLY%

	COM (TT_MO_CONTROL) &
		CLOSETITLE$ = 20%, &
		CLOSETYPE$(2%) = 20%

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
		SMG_WINDOW::DESCR = "Manufacture Order Control File"
		SMG_WINDOW::NHELP = "MO_MAIN_CONTROL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 128% !Relative file
		SMG_WINDOW::NITEMS= 4%

		SMG_WINDOW::NKEYS = 0%

		CLOSETITLE$ = "Type   Description"
		CLOSETYPE$(0%) = "2"
		CLOSETYPE$(1%) = "0    No status"
		CLOSETYPE$(2%) = "1    Close/Purge"

		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF MO_CONTROL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF MO_CONTROL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		%INCLUDE "SOURCE:[MO.OPEN]MO_CONTROL.CRE"
		MO_CONTROL.READONLY% = 0%
		GOTO 790

 !760
		!
		! If unable to open for modify, try to open
		! with read access only.
		!
 !		%INCLUDE "SOURCE:[MO.OPEN]MO_CONTROL.OPN"
 !		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
 !		MO_CONTROL.READONLY% = -1%
 !
 !		GOTO 790
 !
 !770
		!
		! File not able to open, so reset channel
		!
 !		CALL ASSG_FREECHANNEL(MO_CONTROL.CH%)
 !		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = MO_CONTROL.CH%
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

		DATA	04,  09, "(01) Order Number", &
			05,  09, "(02) Last Close/Purge Date", &
			06,  09, "(03) Status Flag", &
			07,  09, "(04) Last Invoice", &
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
	!	^*(01) Order Number\*
	!	.b
	!	.lm +5
	!	The ^*Order Number\* field enters the Order
	!	number for the journal entry.
	!	This field will automatically increment by one number each time
	!	an order is entered.  The number may be overridden by entering
	!	the correct number and pressing ^*Return\*.
	!	.b
	!	An entry is required in this field. The field will
	!	accommodate ten (10) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Order Number
	!
	!--
			MO_CONTROL::ORDNUM = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"04;46", TEMP$, MO_CONTROL::ORDNUM, MFLAG, &
				"'E",MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Last Close/Purge Date\*
	!	.b
	!	.lm +5
	!	The ^*Last Close/Purge Date\* field contains the date in which
	!	the order was last purged.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Purge Date
	!	.x Close Date
	!
	!--
			MO_CONTROL::PURGDATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;46", TEMP$, MO_CONTROL::PURGDATE, MFLAG, &
				"'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Status Flag\*
	!	.b
	!	.lm +5
	!	The ^*Status Flag\* field is
	!	system generated.
	!	.b
	!	Valid flags are:
	!	.table 3,25
	!	.te
	!	^*0\* - No Status
	!	.te
	!	^*1\* - Close/Purge
	!	.end table
	!	Valid status flags may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Status>Flag
	!
	!--
			MO_CONTROL::STATUS_FLAG = EDIT$(ENTR_3STRINGLIST(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;46", TEMP$, MO_CONTROL::STATUS_FLAG, &
				MFLAG, "'", MVALUE, CLOSETYPE$(), &
				CLOSETITLE$, "005"), -1%)

		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Invoice Number
	!	^*(04) Last Invoice Number
	!	.b
	!	.lm +5
	!	The ^*Last Invoice Number\* field contains the invoice
	!	number assigned to the invoice form.
	!	.lm -5
	!
	! Index:
	!
	!--
			MO_CONTROL::LAST_INV = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;46", TEMP$, &
				MO_CONTROL::LAST_INV, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Set MO_CONTROL_OLD value
	!
20500	CASE OPT_SETOLD
		MO_CONTROL_OLD = MO_CONTROL

	!
	! Restore MO_CONTROL_OLD value
	!
	CASE OPT_RESETOLD
		MO_CONTROL = MO_CONTROL_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		MO_CONTROL2 = MO_CONTROL

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		MO_CONTROL = MO_CONTROL2

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
		GET #MO_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		CONTINUE 28030
	END WHEN

	GOTO 28040

28030	!
	! Load in defaults for control file
	!
	MO_CONTROL::ORDNUM	= SPACE$(LEN(MO_CONTROL::ORDNUM))
	MO_CONTROL::PURGDATE	= SPACE$(LEN(MO_CONTROL::PURGDATE))
	MO_CONTROL::STATUS_FLAG	= "0"
	MO_CONTROL::LAST_INV	= SPACE$(LEN(MO_CONTROL::LAST_INV))

	WHEN ERROR IN
		PUT #MO_CONTROL.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "Unable to add a record", 0%)
		CONTINUE 32767
	END WHEN

28040	RETURN

29000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
