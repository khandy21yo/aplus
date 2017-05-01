1	%TITLE "Job Register Query"
	%SBTTL "WP_MAIN_QUERY"
	%IDENT "V3.6a Calico"

	FUNCTION LONG WP_MAIN_QUERY(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1991 BY
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
	!	The ^*Job Query\* option
	!	views on-line job register records.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!	WP_OUTP_QUERYJOB$HELP
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_MAIN_QUERY/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN WP_MAIN_QUERY
	!	$ DELETE WP_MAIN_QUERY.OBJ;*
	!
	! Author:
	!
	!	07/30/91 - Craig Tanner
	!
	! Modification history:
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/30/92 - Dan Perkins
	!		Hard coded "J" in all GETS so we only get
	!		records relating to jobs.
	!
	!	06/22/93 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:WP_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:JC_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! Include CDD's
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB
	MAP (SB_SUBACCOUNT2)	JC_JOB_CDD		JC_JOB_OLD, JC_JOB2

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[JC.OPEN]JC_TYPE.HB"
	MAP (JC_TYPE)		JC_TYPE_CDD		JC_TYPE

	%INCLUDE "SOURCE:[JC.OPEN]JC_CLASS.HB"
	MAP (JC_CLASS)		JC_CLASS_CDD		JC_CLASS

	%INCLUDE "SOURCE:[WP.OPEN]WP_CONTROL.HB"
	MAP (WP_CONTROL)	WP_CONTROL_CDD		WP_CONTROL

	!
	! This common area must be mapped in both the main program and
	! in WP_MAIN_ORDERLINE.
	!
	COM (CH_SB_SUBACCOUNT) &
		SB_SUBACCOUNT.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION WP_OUTP_QUERYJOB

	%PAGE

	ON ERROR GOTO 29000

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
		SMG_WINDOW::DESCR = "Examine Job Register"
		SMG_WINDOW::NHELP = "WP_MAIN_QUERY"
		SMG_WINDOW::CHAN  = JC_JOB.CH%
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 32%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 3%
		SMG_WINDOW::KNAME(0%)	= "Job"
			SMG_WINDOW::KFIELD(0%, 0%)	= 1%
			SMG_WINDOW::KFIELD(0%, 1%)	= 1%
		SMG_WINDOW::KNAME(1%)	= "Type"
			SMG_WINDOW::KFIELD(1%, 0%)	= 2%
			SMG_WINDOW::KFIELD(1%, 1%)	= 3%
			SMG_WINDOW::KFIELD(1%, 2%)	= 1%
		SMG_WINDOW::KNAME(2%)	= "Class"
			SMG_WINDOW::KFIELD(2%, 0%)	= 2%
			SMG_WINDOW::KFIELD(2%, 1%)	= 4%
			SMG_WINDOW::KFIELD(2%, 2%)	= 1%

		SMG_WINDOW::HVIEW	= 78%
		SMG_WINDOW::VVIEW	= 18%

		!
		! Read Defaults
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

20010		GOTO 20040 IF SB_SUBACCOUNT.CH% > 0%

		!
		! Open JC_JOB
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
		USE
			MVALUE = "JC_JOB"
			EXIT HANDLER
		END WHEN

20040		SMG_WINDOW::CHAN  = SB_SUBACCOUNT.CH%
		!RESET	#SB_SUBACCOUNT.CH%
		GET	#SB_SUBACCOUNT.CH%, KEY #0% EQ "J", REGARDLESS

	CASE OPT_OPTLIST

		MVALUE = "Find Next Restore Help View eXit Print"

	!
	! More option
	!
	CASE OPT_MOREMENU

		SELECT EDIT$(MVALUE, -1%)

		!
		! Print
		!
		CASE "PRINT"
			WP_MAIN_QUERY = WP_OUTP_QUERYJOB(JC_JOB)

		END SELECT

	!++
	! Abstract:PRINT
	!	^*Print\*
	!	.b
	!	.lm +5
	!	The ^*Print\* option prints
	!	a report containing information about the present job, along
	!	with its requisitions and lines.
	!	.lm -5
	!
	! Index:
	!	.x Query>Print
	!
	!--

	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!******************************************************************
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	03,10, "Job Number", &
			04,10, "Description", &
			05,10, "Job Type", &
			06,10, "Job Class", &
			07,10, "Open Date", &
			08,10, "Current Status", &
			09,10, "Close Date", &
			10,10, "Location", &
			11,10, "Ref #", &
			12,10, "Operator", &
			13,10, "Batch", &
			14,10, "Post Time", &
			15,10, "Post Date", &
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
		TEMP$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View "

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	^*Job Number\*
	!	.b
	!	.lm +5
	!	The ^*Job Number\* field enters a number which
	!	will reference a particular job.
	!	.b
	!	The field will accommodate ten (10) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Job Number
	!	.x Number>Job
	!
	!--
			JC_JOB::JOB = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"3;30",TEMP$, JC_JOB::JOB, &
				MFLAG, "'E", MVALUE)

		CASE 2%
	!++
	! Abstract:FLD002
	!	^*Description\*
	!	.b
	!	.lm +5
	!	The ^*Description\* field enters a description
	!	of the Job Number entered in field (01).
	!	.b
	!	The field will accommodate forty (40) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Job Description
	!
	!--
			JC_JOB::DESCR = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"4;30",	TEMP$, JC_JOB::DESCR, &
				MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	^*Job Type\*
	!	.b
	!	.lm +5
	!	The ^*Job Type\* field enters the two character job
	!	type code.
	!	.lm -5
	!
	! Index:
	!	.x Job Type
	!
	!--
			JC_JOB::TTYPE = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"5;30",	TEMP$, JC_JOB::TTYPE, &
				MFLAG, "'E", MVALUE)

		CASE 4%
	!++
	! Abstract:FLD004
	!	^*Job Class\*
	!	.b
	!	.lm +5
	!	The ^*Job Class\* field enters the job class code.
	!	.lm -5
	!
	! Index:
	!	.x Job Class
	!
	!--
			JC_JOB::CLASS = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"06;30", TEMP$, JC_JOB::CLASS, &
				MFLAG, "'E", MVALUE)

		CASE 5%
	!++
	! Abstract:FLD005
	!	^*Open Date\*
	!	.b
	!	.lm +5
	!	The ^*Open Date\* field enters the date the job began.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Open Date
	!
	!--
			JC_JOB::BDATE = ENTR_3DATE(SCOPE,  SMG_WINDOW::WNUMBER, &
				"07;30", TEMP$, JC_JOB::BDATE, &
				MFLAG, "'E", MVALUE)

		CASE 6%
	!++
	! Abstract:FLD006
	!	^*Current Status\*
	!	.b
	!	.lm +5
	!	The ^*Current Status\* field will indicate if the job is "Active"
	!	or "Closed".
	!	.b
	!	Valid codes are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	^*A\* = Active
	!	.le
	!	^*C\* = Closed
	!	.els
	!	.lm -5
	!	Pressing ^*<List Choices>\* while the cursor is located at this field
	!	provides a list of valid status codes.
	!
	! Index:
	!	.x Current Status
	!	.x Status>Current
	!
	!--
			JC_JOB::SSTATUS = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "08;30", TEMP$, &
				JC_JOB::SSTATUS, MFLAG, "!", MVALUE)

		CASE 7%
	!++
	! Abstract:FLD007
	!	^*Close Date\*
	!	.b
	!	.lm +5
	!	The ^*Close Date\* field enters the date the job ends.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!	.x Close Date
	!
	!--
			JC_JOB::EDATE = ENTR_3DATE(SCOPE,  SMG_WINDOW::WNUMBER, &
				"09;30",TEMP$, JC_JOB::EDATE, &
				MFLAG, "'E", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	^*Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field enters a
	!	location code pertaining to a selected record.
	!	.lm -5
	!
	! Index:
	!	.x Job Location
	!
	!--
			JC_JOB::LOCATION = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"10;30",TEMP$, JC_JOB::LOCATION, MFLAG, "'E", MVALUE)

		CASE 9%
	!++
	! Abstract:FLD009
	!	^*Reference _#\*
	!	.b
	!	.lm +5
	!	The ^*Reference _#\* field contains the number which posting in the
	!	General Ledger will be completed under. It is the first number referred to
	!	when dealing with this transaction. An example of a Primary Reference number
	!	is the invoice number.
	!	.lm -5
	!
	! Index:
	!	.x Reference Number
	!
	!--
			JC_JOB::REFNO = ENTR_3STRING(SCOPE,  SMG_WINDOW::WNUMBER, &
				"11;30",TEMP$, JC_JOB::REFNO, MFLAG, "'E", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	^*Operator\*
	!	.b
	!	.lm +5
	!	The ^*Operator\* field enters the code which
	!	identifies the person responsible for a particular entry.
	!	.b
	!	The field will accommodate an entry of ten (10) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!	.x Operator
	!
	!--
			JC_JOB::OPERATOR = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;30", TEMP$, &
				JC_JOB::OPERATOR, MFLAG, "'E", MVALUE)

		CASE 11%
	!++
	! Abstract:FLD011
	!	^*Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters the number
	!	of the batch in which the record is to be included.
	!	.b
	!	The field will accept two (2) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number
	!	.x Number>Batch
	!
	!--
			JC_JOB::BATCH = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;30",TEMP$, JC_JOB::BATCH, MFLAG, &
				"'E", MVALUE)

		CASE 12%
	!++
	! Abstract:FLD012
	!	^*Post Time\*
	!	.b
	!	.lm +5
	!	The ^*Post Time\* field contains the time of day the transaction
	!	for the job was posted.
	!	.lm -5
	!
	! Index:
	!	.x Post Time
	!	.x Time>Post
	!
	!--
			JC_JOB::POST_TIME = ENTR_3TIME(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;30",TEMP$, JC_JOB::POST_TIME, MFLAG, &
				"'E", MVALUE)

		CASE 13%
	!++
	! Abstract:FLD013
	!	^*Post Date\*
	!	.b
	!	.lm +5
	!	The ^*Post Date\* field contains the date the transaction was posted.
	!	.lm -5
	!
	! Index:
	!	.x Post Date
	!	.x Date>Post
	!
	!--
			JC_JOB::POST_DATE = ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;30",TEMP$, JC_JOB::POST_DATE, MFLAG, &
				"'E", MVALUE)


		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	CASE OPT_DISPLAY

		!
		! Display the description for type
		!
		IF MAIN_WINDOW(JC_MAIN_TYPE.ID, "Q0" + JC_JOB::TTYPE) <> 1%
		THEN
			JC_TYPE::DESCR = STRING$(LEN(JC_TYPE::DESCR), A"?"B)
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			JC_TYPE::DESCR, &
			5%, 40%, , SMG$M_BOLD)

		!
		! Display the description for class
		!
		JC_CLASS::DESCR = STRING$(LEN(JC_CLASS::DESCR), A"?"B) &
			IF MAIN_WINDOW(JC_MAIN_CLASS.ID, "Q0" + JC_JOB::CLASS) <> 1%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			JC_CLASS::DESCR, &
			6%, 40%, , SMG$M_BOLD)

		!
		! Display description for location
		!
		UTL_LOCATION::LOCNAME = &
			STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
					"Q0" + JC_JOB::LOCATION) <> 1%

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			UTL_LOCATION::LOCNAME, &
			10%, 40%, , SMG$M_BOLD)

	!
	! Set JC_JOB_OLD value
	!
20500	CASE OPT_SETOLD
		JC_JOB_OLD = JC_JOB

	!
	! Restore JC_JOB_OLD value
	!
	CASE OPT_RESETOLD
		JC_JOB = JC_JOB_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		JC_JOB2 = JC_JOB

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		JC_JOB = JC_JOB2

	!
	! View the Record.
	!
	CASE OPT_VIEW

		SELECT MLOOP

		CASE 1%
			MVALUE = "  Job        Description          Type Class Open Date  Status"

		CASE 2%
			MVALUE = "013,034,039,045,056"

		CASE 3%
			MVALUE = JC_JOB::JOB + " " + &
				LEFT$(JC_JOB::DESCR, 20%) + " " + &
				JC_JOB::TTYPE + "   " + &
				JC_JOB::CLASS + "  " + &
				PRNT_DATE(JC_JOB::BDATE, 8%) + " " + &
				JC_JOB::SSTATUS

		END SELECT

	!
	! Find the Order Number.
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE "J" + &
					JC_JOB::JOB, REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE "J" + &
					JC_JOB::TTYPE + &
					JC_JOB::JOB, REGARDLESS

		CASE 2%
			FIND #SMG_WINDOW::CHAN, &
				KEY #2% GE "J" + &
					JC_JOB::CLASS + &
					JC_JOB::JOB, REGARDLESS

		END SELECT
28900
	END SELECT

	EXIT FUNCTION

	%PAGE

29000	!*******************************************************************
	! Error Trapping
	!*******************************************************************

	!
	! Resume to display untrapped error
	!
	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:LINESUMMARY
	!	^*lineSummary\*
	!	.lm +5
	!	.b
	!	The ^*lineSummary\* function is used to view a
	!	summary of the job lines.  The information includes:
	!	.table 3,25
	!	.te
	!	Line _#	Item Code
	!	.te
	!	Description	Quantity Ordered
	!	.te
	!	Quantity Complete	Quantity Cancelled
	!	.te
	!	Balance
	!	.end table
	!	By accessing the ^*View\* function,
	!	additional information relative to job lines can be viewed.  The information
	!	includes:
	!	.table 3,25
	!	.te
	!	Line_#	Transaction Type
	!	.te
	!	Item Code	Start Date
	!	.te
	!	Quantity	Unit Cost
	!	.te
	!	Batch Reference
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Query>Line Summary
	!	.x Line Summary>Query
	!
	!--
	!+-+-+
	!++
	! Abstract:LINEDETAIL
	!	^*Linedetail\*
	!	.lm +5
	!	.b
	!	The ^*Linedetail\* function displays detail for each
	!	line item for a job.  The information displayed includes:
	!	.table 3,25
	!	.te
	!	Line	Record Type
	!	.te
	!	Line Type	Item Code
	!	.te
	!	Quantity	Cost
	!	.te
	!	Start Date	Complete Date
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Line Detail
	!	.x Detail>Line
	!
	!--
