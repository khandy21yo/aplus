1	%TITLE "Broker and Salesman File Maintenance"
	%SBTTL "SA_MAIN_SALESMAN"
	%IDENT "V3.6a Calico"

	FUNCTION LONG SA_MAIN_SALESMAN(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1989 BY
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
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_MAIN_SALESMAN/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN SA_MAIN_SALESMAN
	!	$ DELETE SA_MAIN_SALESMAN.OBJ;*
	!
	! Author:
	!
	!	06/29/90 - Frank F. Starman
	!
	! Modification history:
	!
	!	05/23/90 - J. Shad Rydalch
	!		Increased size of the file. Overlay with SA_SALESMAN
	!
	!	08/19/91 - Dan Perkins
	!		Added F17 key feature to fields #3 and #4.
	!
	!	06/01/92 - Dan Perkins
	!		Allow entry of a blank salesman.
	!
	!	07/15/92 - Dan Perkins
	!		Changed entry on phone field from ENTR_3STRING to
	!		ENTR_3PHONE.
	!
	!	11/17/92 - Dan Perkins
	!		Added CASE 2 to OPT_SUBWIND so that VIEW would
	!		work properly.
	!
	!	12/01/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/20/95 - Kevin Handy
	!		Fix format parameter to entr_3phone.
	!
	!	06/07/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
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
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN
	MAP (SA_SALESMAN_OLD)	SA_SALESMAN_CDD		SA_SALESMAN_OLD
	MAP (SA_SALESMAN_DEF)	SA_SALESMAN_CDD		SA_SALESMAN_DEF

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[SA.OPEN]SA_TYPE.HB"
	MAP (SA_TYPE)		SA_TYPE_CDD		SA_TYPE

	%INCLUDE "SOURCE:[SA.OPEN]SA_CLASS.HB"
	MAP (SA_CLASS)		SA_CLASS_CDD		SA_CLASS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP (UTL_COUNTRY)	UTL_COUNTRY_CDD		UTL_COUNTRY

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_SB_SUBACCOUNT) &
		SB_SUBACCOUNT.CH%, &
		SB_SALESMAN.READONLY%

	COM (TT_SA_SALESMAN) &
		STITLE$ = 30%, &
		SSTAT$(6%) = 30%

	!
	! Default Subject
	!
	DEF_SUBJECT$ = "S"

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

		!************************************************************
		! Set up information
		!************************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Broker and Salesman Maintenance"
		SMG_WINDOW::NHELP = "SA_MAIN_SALESMAN"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 19%

		SMG_WINDOW::NKEYS = 3%
		SMG_WINDOW::KNAME(0%) = "Salesman"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Type"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 3%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
		SMG_WINDOW::KNAME(2%) = "Class"
			SMG_WINDOW::KFIELD(2%, 0%) = 2%
			SMG_WINDOW::KFIELD(2%, 1%) = 4%
			SMG_WINDOW::KFIELD(2%, 2%) = 1%

		SMG_WINDOW::HVIEW = 76%
		SMG_WINDOW::VVIEW = 18%

		STITLE$ = "Status   Description"
		SSTAT$(0%) = "3"
		SSTAT$(1%) = "A      Active"
		SSTAT$(2%) = "I      Inactive"
		SSTAT$(3%) = "P      Purge"
		SSTAT$(4%) = "T      Terminated"

		!
		! Read Defaults
		!

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF SB_SUBACCOUNT.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if it was
			! that way from the last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF SB_SALESMAN.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			SA_MAIN_SALESMAN = ERR
			CONTINUE 770
		END WHEN

		SB_SALESMAN.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
		USE
			SA_MAIN_SALESMAN = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		SB_SALESMAN.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(SB_SUBACCOUNT.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = SB_SUBACCOUNT.CH%
		WHEN ERROR IN
			RESET #SB_SUBACCOUNT.CH%
			GET #SB_SUBACCOUNT.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
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


		DATA	01,05, "(01) Broker or Salesman #", &
			02,05, "(02) Name", &
			03,05, "(03) Type", &
			04,05, "(04) Class", &
			05,05, "(05) Onset Date", &
			06,05, "(06) Current Status", &
			07,05, "(07) Term Date", &
			08,05, "(08) Address 1", &
			09,05, "(09) Address 2", &
			10,05, "(10) City", &
			11,05, "(11) State", &
			12,05, "(12) ZIP", &
			13,05, "(13) Country", &
			14,05, "(14) Phone", &
			15,05, "(15) Initials", &
			16,05, "(16) Region", &
			17,05, "(17) Comm %", &
			0, 0, ""

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
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
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
	!	.x Broker
	!	^*(01) Broker or Salesman Number\*
	!	.b
	!	.lm +5
	!	The ^*Broker or Salesman Number\* field enters a number which
	!	references a particular broker or salesman.
	!	.b
	!	The field will accommodate ten characters.
	!	.lm -5
	!
	! Index:
	!	.x Salesman
	!
	!--

			SA_SALESMAN::SALESMAN = &
				ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"1;32",TEMP$, SA_SALESMAN::SALESMAN, &
				MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Name\*
	!	.b
	!	.lm +5
	!	The ^*Name\* field enters the name identifying
	!	the salesman entered in field (01).
	!	.b
	!	Forty spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--

			SA_SALESMAN::DESCR = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;32",	TEMP$, SA_SALESMAN::DESCR, &
				MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	.x Type
	!	^*(03) Type\*
	!	.b
	!	.lm +5
	!	The ^*Type\* field enters the type classification which
	!	relates to the salesman.
	!	.b
	!	Example: ^*RT\* - Retail Salesman
	!	.b
	!	Valid types may be viewed by pressing ^*List Choices\*, or additional types
	!	may be added by pressing ^*F17\*.
	!	.b
	!	The field will accommodate two characters.
	!	.lm -5
	!
	! Index:
	!
	!--


			SA_SALESMAN::TTYPE = &
				ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"3;32",	TEMP$, SA_SALESMAN::TTYPE, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(SA_MAIN_TYPE.ID, "V0") = 1%
				THEN
					SA_SALESMAN::TTYPE = SA_TYPE::TTYPE
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(SA_MAIN_TYPE.ID, "M")
				SA_SALESMAN::TTYPE = SA_TYPE::TTYPE
				GOTO ReEnter
			END IF

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Class\*
	!	.b
	!	.lm +5
	!	The ^*Class\* field enters the class to which the
	!	salesman is assigned.
	!	.b
	!	Example: ^*BUSI\* - Business Class
	!	.b
	!	Valid class codes may be viewed by pressing ^*List Choices\*, or additional
	!	class codes may be added by pressing ^*F17\*.
	!	.b
	!	Four spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--


			SA_SALESMAN::CLASS = &
				ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"04;32", TEMP$, SA_SALESMAN::CLASS, &
				MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF MAIN_WINDOW(SA_MAIN_CLASS.ID, "V0") = 1%
				THEN
					SA_SALESMAN::CLASS = SA_CLASS::CLASS
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(SA_MAIN_CLASS.ID, "M")
				SA_SALESMAN::CLASS = SA_CLASS::CLASS
				GOTO ReEnter
			END IF

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Onset Date\*
	!	.b
	!	.lm +5
	!	The ^*Onset Date\* field enters the date which the salesman
	!	began working with the company.
	!	.B
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!
	!--

			SA_SALESMAN::BDATE = &
				ENTR_3DATE(SCOPE,  SMG_WINDOW::WNUMBER, &
				"05;32", TEMP$, SA_SALESMAN::BDATE, &
				MFLAG, "'E", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Current Status\*
	!	.b
	!	.lm +5
	!	The ^*Current Status\* field indicates the status of the salesman.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*A\* - Active
	!	.te
	!	^*I\* - Inactive
	!	.te
	!	^*P\* - Purged
	!	.te
	!	^*T\* - Terminated
	!	.end table
	!	Valid current status codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--

			SA_SALESMAN::SSTATUS = EDIT$(ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::WNUMBER, "06;32", TEMP$, &
				SA_SALESMAN::SSTATUS, MFLAG, "!", MVALUE, &
				SSTAT$(), STITLE$, "008"), -1%)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Termination Date\*
	!	.b
	!	.lm +5
	!	The ^*Termination Date\* field enters the date that
	!	the salesman was released from employment.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!
	!--

			SA_SALESMAN::EDATE = &
				ENTR_3DATE(SCOPE,  SMG_WINDOW::WNUMBER, &
				"07;32",TEMP$, SA_SALESMAN::EDATE, &
				MFLAG, "'E", MVALUE)


		CASE 8%
	!++
	! Abstract:FLD008
	!	.x Salesman>Address
	!	^*(08) Address 1\*
	!	.b
	!	.lm +5
	!	The ^*Address 1\* field enters the address for the
	!	salesman. It is recommended that in the event of a one line street
	!	address, this field be left blank and field (09) be used for the street
	!	address.
	!	.b
	!	Twenty five spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Address>Salesman
	!
	!--

			SA_SALESMAN::ADD1 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"08;32", TEMP$, &
				SA_SALESMAN::ADD1, MFLAG, "'E", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	.x Salesman>Address
	!	^*(09) Address 2\*
	!	.b
	!	.lm +5
	!	The ^*Address 2\* field enters the address for the
	!	salesman. It is recommended that in the event of a one line street
	!	address, field (08) be left blank and this field (09) be used for the street
	!	address.
	!	.b
	!	Twenty five spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!	.x Address>Salesman
	!
	!--

			SA_SALESMAN::ADD2 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"09;32", TEMP$, &
				SA_SALESMAN::ADD2, MFLAG, "'E", MVALUE)
		CASE 10%
	!++
	! Abstract:FLD010
	!	.x City
	!	^*(10) City\*
	!	.b
	!	.lm +5
	!	The ^*City\* field enters the city in which the
	!	salesman is located.
	!	.b
	!	The field will accommodate fifteen characters.
	!	.lm -5
	!
	! Index:
	!
	!--

			SA_SALESMAN::CITY = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;32", TEMP$, &
				SA_SALESMAN::CITY, MFLAG, "'E", MVALUE)
		CASE 11%
	!++
	! Abstract:FLD011
	!	.x State>
	!	^*(11) State\*
	!	.b
	!	.lm +5
	!	The ^*State\* field enters the state in which
	!	the salesman is located.
	!	.b
	!	The field will accommodate a two (2) character state postal
	!	code.
	!	.lm -5
	!
	! Index:
	!
	!--

			SA_SALESMAN::STATE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;32", TEMP$, &
				SA_SALESMAN::STATE, MFLAG, "'E", MVALUE)
		CASE 12%
	!++
	! Abstract:FLD012
	!	.x Zip
	!	^*(12) ZIP\*
	!	.b
	!	.lm +5
	!	The ^*Zip\* field enters the zip or postal
	!	code for the area in which a salesman is located.
	!	.b
	!	Up to ten characters will be accepted.
	!	.lm -5
	!
	! Index:
	!
	!--

			SA_SALESMAN::ZIP = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;32", TEMP$, &
				SA_SALESMAN::ZIP, MFLAG, "'E", MVALUE)
		CASE 13%
	!++
	! Abstract:FLD013
	!	.x Country
	!	^*(13) Country\*
	!	.b
	!	.lm +5
	!	The ^*Country\* field enters the country if
	!	a salesman is located in a foreign country.
	!	.b
	!	Valid country codes may be viewed by pressing ^*List Choices\*.
	!	.b
	!	Two spaces are available for the entry.
	!	.lm -5
	!
	! Index:
	!
	!--

			SA_SALESMAN::COUNTRY = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;32", TEMP$, &
				SA_SALESMAN::COUNTRY, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_COUNTRY.ID, "V0" + &
					SA_SALESMAN::COUNTRY) = 1%
				THEN
					SA_SALESMAN::COUNTRY = &
						UTL_COUNTRY::COUNTRY
				END IF
				GOTO Reenter
			END IF

		CASE 14%
	!++
	! Abstract:FLD014
	!	.x Telephone>
	!	^*(14) Phone\*
	!	.b
	!	.lm +5
	!	The ^*Phone\* field is for entry of a salesman's telephone number.
	!	.b
	!	It is not required to enter the special characters. The system
	!	will insert them automatically.
	!	.b
	!	Up to ten characters will be accepted.
	!	.lm -5
	!
	! Index:
	!
	!--

			SA_SALESMAN::PHONE = &
				ENTR_3PHONE(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;32", TEMP$, &
				SA_SALESMAN::PHONE, MFLAG, 0%, MVALUE)
		CASE 15%
	!++
	! Abstract:FLD015
	!	.x Initials
	!	^*(15) Initials\*
	!	.b
	!	.lm +5
	!	The ^*Initials\* field enters the initials of the
	!	salesman which will be used throughout the system to identify a
	!	particular salesman.
	!	.b
	!	Up to two characters will be accepted.
	!	.lm -5
	!
	! Index:
	!
	!--

			SA_SALESMAN::INITIALS = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;32", TEMP$, &
				SA_SALESMAN::INITIALS, MFLAG, "'E", MVALUE)
		CASE 16%
	!++
	! Abstract:FLD016
	!	.x Region
	!	^*(16) Region\*
	!	.b
	!	.lm +5
	!	The ^*Region\* field enters the region for which the
	!	salesman is responsible. Examples entries:  NW, SW, NE, SE.
	!	.b
	!	The field will accept up to two characters.
	!	.lm -5
	!
	! Index:
	!
	!--

			SA_SALESMAN::REGION = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;32", TEMP$, &
				SA_SALESMAN::REGION, MFLAG, "'E", MVALUE)
		CASE 17%
	!++
	! Abstract:FLD017
	!	.x Commission Percentage
	!	^*(17) Commission Percentage\*
	!	.b
	!	.lm +5
	!	The ^*Commission Percentage\* enters the percentage of
	!	the sales amount which the salesman will receive as compensation for each sale
	!	made. The amount is entered as a percentage.  As an example, if the commission
	!	percentage is to be 10%, the entry would be made as 10.00.
	!	.lm -5
	!
	! Index:
	!
	!--

			SA_SALESMAN::COMMPER = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::WNUMBER, &
				"17;32", TEMP$, &
				SA_SALESMAN::COMMPER, MFLAG, "###.##", MVALUE)
		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		SA_MAIN_SALESMAN = 0%

		SELECT MLOOP

		CASE 1%
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, KEY #0% EQ &
						SA_SALESMAN::SUBJECT + &
						SA_SALESMAN::SALESMAN, REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				SA_MAIN_SALESMAN = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)
			END IF

		END SELECT

	!
	! Set SB_SUBACCOUNT_OLD value
	!
	CASE OPT_SETOLD
		SA_SALESMAN_OLD = SA_SALESMAN

	!
	! Restore SB_SUBACCOUNT_OLD value
	!
	CASE OPT_RESETOLD
		SA_SALESMAN = SA_SALESMAN_OLD
	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		SA_SALESMAN_DEF = SA_SALESMAN
	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		SA_SALESMAN = SA_SALESMAN_DEF
		SA_SALESMAN::SUBJECT = DEF_SUBJECT$

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE ="  Salesman   Name                   " + &
				"           Ty Clas OnsetDate  S TermDate"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "013,047,050,055,066,068"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = SA_SALESMAN::SALESMAN	+ " " + &
				LEFT(SA_SALESMAN::DESCR, 33%) + " " + &
				SA_SALESMAN::TTYPE + " " + &
				SA_SALESMAN::CLASS + " " + &
				PRNT_DATE(SA_SALESMAN::BDATE, 8%) + " " + &
				SA_SALESMAN::SSTATUS + " " + &
				PRNT_DATE(SA_SALESMAN::EDATE, 8%)

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE SA_SALESMAN::SUBJECT + &
				SA_SALESMAN::SALESMAN, REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE SA_SALESMAN::SUBJECT + &
				SA_SALESMAN::TTYPE + &
				SA_SALESMAN::SALESMAN, REGARDLESS

		CASE 2%
			FIND #SMG_WINDOW::CHAN, &
				KEY #2% GE SA_SALESMAN::SUBJECT + &
				SA_SALESMAN::CLASS + &
				SA_SALESMAN::SALESMAN, REGARDLESS

		END SELECT

	CASE OPT_SUBWIND

		SELECT MLOOP

		!
		! Find first record (if there is any)
		!
		CASE 1%

			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27110			!
			! Search for first record
			!
			WHEN ERROR IN
				FIND #SMG_WINDOW::CHAN, &
					KEY #0% EQ DEF_SUBJECT$, &
					REGARDLESS

				!
				! Get a record
				!
				GET #SMG_WINDOW::CHAN
				SMG_WINDOW::CURREC = 0%
			USE
				CONTINUE ExitFunction IF ERR = 155%
				EXIT HANDLER
			END WHEN

		!
		! Find starting record (if there is any)
		!
		CASE 2%
			!
			! Set init value
			!
			SMG_WINDOW::CURREC = -1%

27115			!
			! Search for starting record
			!
			WHEN ERROR IN
				SELECT MFLAG
				CASE 0%
					FIND #SMG_WINDOW::CHAN, &
						KEY #0% GE SA_SALESMAN::SUBJECT + &
						SA_SALESMAN::SALESMAN, REGARDLESS

				CASE 1%
					FIND #SMG_WINDOW::CHAN, &
						KEY #1% GE SA_SALESMAN::SUBJECT + &
						SA_SALESMAN::TTYPE + &
						SA_SALESMAN::SALESMAN, REGARDLESS

				CASE 2%
					FIND #SMG_WINDOW::CHAN, &
						KEY #2% GE SA_SALESMAN::SUBJECT + &
						SA_SALESMAN::CLASS + &
						SA_SALESMAN::SALESMAN, REGARDLESS

				END SELECT
			USE
				CONTINUE ExitFunction IF ERR = 155%
				EXIT HANDLER
			END WHEN


			!
			! Get a record
			!
			SMG_WINDOW::CURREC = 0%

		!
		! Check if still right key
		!
		CASE 3%
			SMG_WINDOW::CURREC = -1%

			SMG_WINDOW::CURREC = 0% &
				IF SA_SALESMAN::SUBJECT = DEF_SUBJECT$


		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
