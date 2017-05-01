1	%TITLE "Maintain Bill of Materials Control Record"
	%SBTTL "BM_MAIN_CONTROL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG BM_MAIN_CONTROL(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1992 BY
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
	!	The ^*Maintain Bill of Materials Control Record\*
	!	establishs several different records
	!	that once are established need not be
	!	accessed again.
	!	.lm -5
	!
	! Index:
	!	.x Maintain>Control Record
	!	.x Control Record>Maintain
	!	.x Maintain>Control
	!	.x Control>Maintain
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_MAIN_CONTROL/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP BM_MAIN_CONTROL
	!	$ DELETE BM_MAIN_CONTROL.OBJ;*
	!
	! Author:
	!
	!	06/29/92 - Dan Perkins
	!
	! Modification history:
	!
	!	07/10/92 - Kevin Handy
	!		Clean up (check)
	!
	!	07/22/92 - Frank F. Starman
	!		Added STD Labor Rate.
	!
	!	10/20/92 - Frank F. Starman
	!		Added Raw material type.
	!
	!	12/26/92 - Frank F. Starman
	!		Added Burden Labor Percentage field.
	!
	!	01/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.HB"
	MAP (BM_CONTROL)	BM_CONTROL_CDD		BM_CONTROL
	MAP (BM_CONTROL2)	BM_CONTROL_CDD		BM_CONTROL_OLD, &
							BM_CONTROL2

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODTYPE.HB"
	MAP (PD_PRODTYPE)	PD_PRODTYPE_CDD		PD_PRODTYPE

	!
	! This common area must be mapped in both the main program and
	! in MAINT_GROUP.
	!
	COM (CH_BM_CONTROL) &
		BM_CONTROL.CH%, &
		BM_CONTROL.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW

	ON ERROR GOTO 29000

	%PAGE

	SELECT MOPTION

	!
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!
	CASE OPT_INIT

		!******************************************************************
		! Set up information
		!******************************************************************

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "BM Control File Maintenance"
		SMG_WINDOW::NHELP = "BM_MAIN_CONTROL"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 5%
		SMG_WINDOW::FLAGS = 128%
		! Relative file

		SMG_WINDOW::NKEYS = 0%

700		!
		! Declare channels
		!
		IF BM_CONTROL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF BM_CONTROL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			BM_MAIN_CONTROL = ERR
			CONTINUE 770
		END WHEN

		BM_CONTROL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.OPN"
		USE
			BM_MAIN_CONTROL = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		BM_CONTROL.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(BM_CONTROL.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = BM_CONTROL.CH%
		GOSUB 28000

	CASE OPT_OPTLIST
		MVALUE = "Change Blank Help eXit"

	!
	! Display the background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	5, 5, "(01) Burden Hourly Rate", &
			6, 5, "(02) Burden Labor Perc", &
			7, 5, "(03) Labor Hourly Rate", &
			8, 5, "(04) Component Type", &
			9, 5, "(05) Raw Mat Type", &
			0,  0, ""

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
		TEMP$ = TRM$(SCOPE::PRG_ITEM)

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) Standard Burden Hourly Rate\*
	!	.b
	!	.lm +5
	!	The ^*Standard Burden Hourly Rate\* field records
	!	the Burden Hourly Rate.
	!	.lm -5
	!
	! Index:
	!
	!--
			BM_CONTROL::BURDENRATE = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"05;30",TEMP$, BM_CONTROL::BURDENRATE, MFLAG, &
				"###.###", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) Burden Labor Percentage\*
	!	.b
	!	.lm +5
	!	The ^*Burden Labor Percentage\* field records the current
	!	burden labor percentage.
	!	.lm -5
	!
	! Index:
	!
	!--
			BM_CONTROL::BURDENPERC = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"06;30",TEMP$, BM_CONTROL::BURDENPERC, MFLAG, &
				"###.###%", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) Standard Labor Hourly Rate\*
	!	.b
	!	.lm +5
	!	The ^*Standard Labor Hourly Rate\* field records
	!	the Standard Labor Hourly Rate.
	!	.lm -5
	!
	! Index:
	!
	!--
			BM_CONTROL::LABORRATE = ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"07;30",TEMP$, BM_CONTROL::LABORRATE, MFLAG, &
				"###.###", MVALUE)


		CASE 4%
	!++
	! Abstract:FLD004
	!	^*(04) Component Type\*
	!	.b
	!	.lm +5
	!	The ^*Component Type\* field determines which products
	!	are low level components used to calculate the standard
	!	costs.
	!	.lm -5
	!
	! Index:
	!
	!--
			BM_CONTROL::PRODTYPE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"08;30",TEMP$, BM_CONTROL::PRODTYPE, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODTYPE.ID, "VX") = 1%
				THEN
					IF BM_CONTROL::PRODTYPE = ""
					THEN
						BM_CONTROL::PRODTYPE = &
							PD_PRODTYPE::CODE
					ELSE
						BM_CONTROL::PRODTYPE = &
							TRM$(BM_CONTROL::PRODTYPE) + &
							"," + PD_PRODTYPE::CODE
					END IF
				END IF
				GOTO Reenter
			END IF

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Raw Material Type\*
	!	.p
	!	The ^*Raw Material Type\* field defines what product
	!	types will appear in the "Raw Material" column of the
	!	WP standards.
	!	All other material will show in the other column.
	!
	! Index:
	!
	!--
			BM_CONTROL::RMAT = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"09;30",TEMP$, BM_CONTROL::RMAT, MFLAG, &
				"'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODTYPE.ID, "VX") = 1%
				THEN
					IF BM_CONTROL::RMAT = ""
					THEN
						BM_CONTROL::RMAT = &
							PD_PRODTYPE::CODE
					ELSE
						BM_CONTROL::RMAT = &
							TRM$(BM_CONTROL::PRODTYPE) + &
							"," + PD_PRODTYPE::CODE
					END IF
				END IF
				GOTO Reenter
			END IF

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

	!
	! Set BM_CONTROL_OLD value
	!
20500	CASE OPT_SETOLD
		BM_CONTROL_OLD = BM_CONTROL

	!
	! Restore BM_CONTROL_OLD value
	!
	CASE OPT_RESETOLD
		BM_CONTROL = BM_CONTROL_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		BM_CONTROL2 = BM_CONTROL

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		BM_CONTROL = BM_CONTROL2

	END SELECT

	EXIT FUNCTION

28000	!
	! Get control record
	!
	WHEN ERROR IN
		GET #BM_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		CONTINUE 28030 IF ERR = 155% OR ERR = 11%
		!
		! What do we do now
		!
		CONTINUE 28040
	END WHEN

	GOTO 28040

28030	!
	! Load in defaults for control file
	!
	BM_CONTROL::BURDENRATE = 0.0
	BM_CONTROL::BURDENPERC = 0.0
	BM_CONTROL::LABORRATE = 0.0
	BM_CONTROL::PRODTYPE  = ""
	BM_CONTROL::RMAT      = ""

	PUT #BM_CONTROL.CH%

28040	RETURN

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
