1	%TITLE "Sales File Maintenance"
	%SBTTL "PR_MAIN_SALES"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_MAIN_SALES(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987 BY
	!
	! Computer Management Center
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
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
	!	The ^*Maintain Sales Information\* routine enters
	!	sales volume data for specified locations on specified dates.
	!
	! Index:
	!	.x Maintain>Sales Information
	!	.x Sales>Maintain
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_MAIN_SALES/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PR_MAIN_SALES
	!	$ DELETE PR_MAIN_SALES.OBJ;*
	!
	! Author:
	!
	!	03/06/89 - Frank Starman
	!
	! Modification history:
	!
	!	04/23/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	04/29/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/14/93 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/22/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/29/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/01/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION MAIN_WINDOW, FUNC_TESTENTRY

	%PAGE


	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[PR.OPEN]PR_SALES.HB"
	MAP (PR_SALES)		PR_SALES_CDD	PR_SALES
	MAP (PR_SALES_OLD)	PR_SALES_CDD	PR_SALES_OLD
	MAP (PR_SALES_DEF)	PR_SALES_CDD	PR_SALES_DEF

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)		UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEPARTMENT.HB"
	MAP (UTL_DEPARTMENT)		UTL_DEPARTMENT_CDD	UTL_DEPARTMENT

	!
	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_PR_SALES) &
		PR_SALES.CH%, &
		PR_SALES.READONLY%

	!
	! Set up error trapping
	!
	ON ERROR GOTO 29000

	%PAGE

	SELECT MOPTION
	!******************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!******************************************************************
	CASE OPT_INIT

		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Sales Maintenance"
		SMG_WINDOW::NHELP = "PR_MAIN_SALES"
		SMG_WINDOW::HSIZE = 77%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 13%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Location"
			SMG_WINDOW::KFIELD(0%, 0%) = 3%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
			SMG_WINDOW::KFIELD(0%, 3%) = 3%
		SMG_WINDOW::KNAME(1%) = "Date"
			SMG_WINDOW::KFIELD(1%, 0%) = 3%
			SMG_WINDOW::KFIELD(1%, 1%) = 3%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
			SMG_WINDOW::KFIELD(1%, 3%) = 2%

		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%

		!
		! Load in the defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Declare channels
		!
		IF PR_SALES.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF PR_SALES.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_SALES.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			PR_MAIN_SALES = ERR
			CONTINUE 770
		END WHEN

		PR_SALES.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_SALES.OPN"
		USE
			PR_MAIN_SALES = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		PR_SALES.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(PR_SALES.CH%)

		GOTO ExitFunction

790		SMG_WINDOW::CHAN  = PR_SALES.CH%
		WHEN ERROR IN
			RESET #PR_SALES.CH%
			GET #PR_SALES.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	%PAGE

20100	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!******************************************************************
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	05,05, "(01) Location #", &
			06,05, "(02) Department #", &
			07,05, "(03) Sales Date", &
			08,05, "(04) Amount", &
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


20200	!******************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!******************************************************************
	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)

		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")
 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Location _#\*
	!	.p
	!	The ^*Location _#\* field
	!	enters the location identifier.  This field will
	!	accommodate up to four (4) alphanumeric characters.
	!
	! Index:
	!	.x Location>Sales
	!
	!--

			PR_SALES::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "05;25", TEMP$, &
				PR_SALES::LOCATION, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
					"V0  ") = 1%)
				THEN
					PR_SALES::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Department _#\*
	!	.p
	!	The ^*Department _#\* field
	!	enters a department number to which
	!	the sales volume relates.  The field will accommodate up to six
	!	(6) alphanumeric characters.
	!	.p
	!	If the sales volume to be entered relates to the total location
	!	sales, this field may be left blank.  Labor costs in any department
	!	in the location will be compared to the total location sales.
	!
	! Index:
	!	.x Sales>Department
	!	.x Department>Sales
	!
	!--


			PR_SALES::DEPARTMENT= ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "06;25", TEMP$, &
				PR_SALES::DEPARTMENT, MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_DEPARTMENT.ID, &
					"V0" + PR_SALES::LOCATION)
				THEN
					PR_SALES::DEPARTMENT = &
						UTL_DEPARTMENT::DEPT_NUM
				END IF
				GOTO Reenter
			END IF

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Sales Date\*
	!	.p
	!	The ^*Sales Date\* field
	!	enters the ending date relative to the sales
	!	volume to be entered in the ^*Amount\* field.  This date could
	!	represent a single day or group of days.  If a group of days is
	!	represented, it would normally be a date equal to a payroll week or
	!	a payroll ended date.  It could be the last day of an accounting
	!	period, regardless on the number of days included.  It is important
	!	to keep in mind that the period included is to be compared to payroll
	!	costs for the same period.
	!	.p
	!	The format for this field is MM/DD/YYYY.
	!
	! Index:
	!	.x Sales>Date
	!
	!--

			PR_SALES::SALEDATE = ENTR_3DATE(SCOPE, &
				SMG_WINDOW::WNUMBER, "07;25", TEMP$, &
				PR_SALES::SALEDATE, MFLAG, "'E", &
				MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Amount\*
	!	.p
	!	The ^*Amount\* field
	!	enters the dollar amount of the sales related to
	!	a specific location on a specific date or date ended.
	!
	! Index:
	!	.x Sales>Amount
	!
	!--

			PR_SALES::AMOUNT = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "08;25",TEMP$, &
				PR_SALES::AMOUNT, MFLAG, &
				"##,###,###.##", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!***********************************************************************
	! Test values
	!***********************************************************************
20300	CASE OPT_TESTENTRY
		PR_MAIN_SALES = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			PR_MAIN_SALES = FUNC_TESTENTRY(SMG_WINDOW, &
				PR_SALES::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"PR", MLOOP, "PRG", &
				"Location", UTL_MAIN_LOCATION.ID)

		CASE 2%
			!
			! Is the input defined?
			!
			PR_MAIN_SALES = FUNC_TESTENTRY(SMG_WINDOW, &
				PR_SALES::LOCATION + PR_SALES::DEPARTMENT, &
				UTL_DEPARTMENT::DESCRIPTION, &
				"PR", MLOOP, "PRG", &
				"Department", UTL_MAIN_DEPARTMENT.ID)

		CASE 3%
			IF (MVALUE = "ADD")
			THEN
				WHEN ERROR IN
					GET #SMG_WINDOW::CHAN, &
						KEY #0% EQ PR_SALES::LOCATION + &
						PR_SALES::DEPARTMENT + &
						PR_SALES::SALEDATE, &
						REGARDLESS
				USE
					CONTINUE 32767 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				PR_MAIN_SALES = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 0%)
			END IF

		END SELECT

	!******************************************************************
	! Set PR_SALES_OLD value
	!******************************************************************
	CASE OPT_SETOLD
		PR_SALES_OLD = PR_SALES


	!******************************************************************
	! Restore PR_SALES_OLD value
	!******************************************************************
	CASE OPT_RESETOLD
		PR_SALES = PR_SALES_OLD

	!******************************************************************
	! Set default value
	!******************************************************************
	CASE OPT_SETDEFAULT
		PR_SALES_DEF = PR_SALES

	!******************************************************************
	! Restore default value
	!******************************************************************
	CASE OPT_RESETDEFAULT
		PR_SALES = PR_SALES_DEF

	!***********************************************************
	! View header
	!***********************************************************
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Loc  Dept   SaleDate      Amount"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,014,023"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = PR_SALES::LOCATION + " " + &
				PR_SALES::DEPARTMENT + " " + &
				PRNT_DATE(PR_SALES::SALEDATE, 6%) + " " + &
				FORMAT$(PR_SALES::AMOUNT, "#######.##")

		END SELECT

	!***********************************************************************
	! Find
	!***********************************************************************
	CASE OPT_FIND
		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE PR_SALES::LOCATION + &
				PR_SALES::DEPARTMENT + &
				PR_SALES::SALEDATE, &
				REGARDLESS
		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE PR_SALES::SALEDATE + &
				PR_SALES::LOCATION+ &
				PR_SALES::DEPARTMENT, &
				REGARDLESS
		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
