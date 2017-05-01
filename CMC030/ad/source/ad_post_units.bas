1	%TITLE "Journal Posting"
	%SBTTL "AD_POST_UNITS"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
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
	! ID:AD004
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The execution of the ^*POST\* routine posts the
	!	data in a journal file into the Units Register.
	!	Upon the execution of the Posting routine, a transmittal report
	!	will be displayed on the screen at the completion of which the
	!	user has the option to either abort or continue the posting process.
	!	When the option to continue is elected, the transmittal is printed.
	!	At the completion of the posting routine the data in that batch is
	!	erased.
	!	.lm -5
	!
	! Index:
	!	.x Post >Depreciation Units Journal
	!	.x Journal>Post Depreciation Units
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_POST_UNITS
	!	$ LINK/EXE=AD_EXE: AD_POST_UNITS, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_POST_UNITS.OBJ;*
	!
	! Author:
	!
	!	06/29/89 - Aaron Redd
	!
	! Modification history:
	!
	!	08/06/91 - Craig Tanner
	!		Declare CHECK_PERIOD so that post will check the dates.
	!
	!	03/18/92 - Dan Perkins
	!		Commented out PRNT_SUMMARY variable which doesn't seem
	!		to have much reason for existence.
	!
	!	06/08/92 - Dan Perkins
	!		Kill Journal files before completing posting, if
	!		posting gets that far.
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	08/29/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/28/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	09/13/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include constants and and some functions
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[AD.OPEN]AD_UNITS.HB"
	MAP	(AD_UNITS)	AD_UNITS_CDD	AD_UNITS

	%INCLUDE "SOURCE:[AD.OPEN]AD_JOURNAL.HB"
	MAP	(AD_JOURNAL)	AD_JOURNAL_CDD	AD_JOURNAL

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[AD.OPEN]AD_REGUNIT.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	AD_TRAN_POSTAD
	EXTERNAL LONG	FUNCTION	ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION	OUTP_UNDEFCODES

	!
	! Declare internal variables
	!
	DECLARE	AD_REGUNIT_CDD		AD_REGUNIT
	DECLARE	UTL_REPORTX_CDD		UTL_REPORTX

	DECLARE	LONG			EXIT_STATUS
	DECLARE	LONG			INTR_STATUS
	DECLARE LONG			CHECK_PERIOD

	DECLARE	STRING			BATCH_NUMBER
	DECLARE	STRING			GLPERIOD
	DECLARE	STRING			AD.INTER.PERIOD
	DECLARE	STRING			POSTDATE
	DECLARE	STRING			POSTTIME
	DECLARE	STRING			TEXT
	DECLARE	STRING			TITLE(10%)

	%PAGE

	!**********************************************************************
	! Get some stuff done before we start
	!**********************************************************************
	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Title
	!
	TITLE(1%) = "UNIT  JOURNAL  POSTING  PROTOCOL"
	TITLE(2%) = "Asset Depreciation System"
	TITLE(3%) = ""

	!
	! Heading
	!
	TITLE(4%) = "."

	%PAGE

	!**********************************************************************
	! Process `from user' input
	!**********************************************************************
	!
	! Set user defined fields
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GLPERIOD = TRM$(UTL_REPORTX::OPTDEF(0%))
	!++
	! Abstract:FLD01
	!	^*(01) Period\*
	!	.b
	!	.lm +5
	!	The ^*Period\* field enters the
	!	period which will be posted.
	!	.b
	!	The format for entry is YYYYPP.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Period>Post
	!	.x Post>Period
	!
	!--

	BATCH_NO$ = TRM$(UTL_REPORTX::OPTDEF(1%))
	!++
	! Abstract:FLD02
	!	^*(02) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters a particular batch
	!	to be posted.
	!	.b
	!	Only one batch at a time may be posted.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Post>Batch #
	!	.x Batch #>Post
	!
	!--

	CHECK_DATE$ = TRM$(UTL_REPORTX::OPTDEF(2%))
	!++
	! Abstract:FLD03
	!	^*(03) Check Action Dates\*
	!	.b
	!	.lm +5
	!	The ^*Check Action Dates\* posts all action by entering
	!	a ^*N\* answer or to check the date of the action for the correct period
	!	before posting by entering a ^*Y\* answer.
	!	.lm -5
	!
	! Index:
	!	.x Check Action Dates>Post
	!	.x Post>Check Action Dates
	!
	!--

	CHECK_PERIOD = SUBOPT_NOOPT
	CHECK_PERIOD = SUBOPT_CHECK IF (CHECK_DATE$ = "Y")

310	!
	! Open unit journal file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_UNITS.UPD"
	USE
		!
		! Locked File
		!
		IF ERR = 138%
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "AD_UNITS"
		CONTINUE HelpError
	END WHEN

320	!
	! Open AD  asset description file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_JOURNAL.UPD"
	USE
		!
		! Locked File
		!
		IF ERR = 138%
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "AD_JOURNAL"
		CONTINUE HelpError
	END WHEN

	%PAGE

	!**********************************************************************
	!	1) See if the posting process has been interrupted
	!	2) If not interrupted, go on
	!	3) If interrupted, delete the superceded ledger records
	!		and then continue
	!**********************************************************************

	!**********************************************************************
	! Check if posting process has been interrupted
	!**********************************************************************
	!
	! Open up batch control file and check if interrupted
	!
	INTR_STATUS = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AD_JOURNAL", BATCH_NO$, &
		AD.INTER.PERIOD, "")

	SELECT INTR_STATUS
	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Process was interrupted
	!
	CASE CMC$_WARNING

		IF TRM$(AD.INTER.PERIOD) <> ""
		THEN
			GOTO Aborted IF &
				AD_TRAN_POSTAD(OPT_RESTART, &
				SUBOPT_NOOPT, &
				BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, &
				"", "", AD.INTER.PERIOD) <> CMC$_NORMAL
		END IF

	!
	! Abort post process
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	%PAGE

 AssignBatch:
	!**********************************************************************
	!	1) Assign a batch number
	!	2) Make sure no legitimate records in the ledger already
	!		have this batch number; if any records do have
	!		this newly assigned number, go back to (1) and
	!		get a new one.
	!**********************************************************************
	!
	! Open up batch control file and get a batch number
	!
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AD_JOURNAL", BATCH_NO$, &
		GLPERIOD, "") <> CMC$_NORMAL

	EXIT_STATUS = AD_TRAN_POSTAD(OPT_CHECK, SUBOPT_REGISTER, BATCH_NUMBER, &
		TITLE(), UTL_REPORTX, "", "", GLPERIOD)

	SELECT EXIT_STATUS
	!
	! Success; keep going
	!
	CASE CMC$_NORMAL

	!
	! Found batch number, go for new one
	!
	CASE CMC$_WARNING
		GOTO AssignBatch

	!
	! Something's definitely wrong here
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	%PAGE

	!**********************************************************************
	!	1) List the debits/credits transmittal for the user while
	!		also putting the journal data in temporary files
	!	2) If confirmation, then go on
	!**********************************************************************

	!**********************************************************************
	! Create transmittal
	!**********************************************************************
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ADDREC, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AD_JOURNAL", BATCH_NO$, "", "") <> CMC$_NORMAL

	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

 ReadParent:
	!
	! Read in a parent record from the Asset Journal file
	!
3000	WHEN ERROR IN
		GET #AD_JOURNAL.CH%
	USE
		!
		! Locked Block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE Confirm
	END WHEN

	!
	! Set some initial variable values
	!
	UNITS_KEY$ = AD_JOURNAL::DEP_OBJECT + AD_JOURNAL::ACTION_DATE
	ACTDATE$ = " "

 ProcessChildren:
	!
	! Find the first record depending from the parent record
	!
3100	WHEN ERROR IN
		FIND #AD_UNITS.CH%, KEY #0% EQ UNITS_KEY$, REGARDLESS
	USE
		CONTINUE ProcessParent IF (ERR = 155%) OR (ERR = 11%)
		FILENAME$ = "AD_UNITS"
		CONTINUE HelpError
	END WHEN

 ChildRec:
	!
	! Get the (next) "child" record
	!
3200	WHEN ERROR IN
		GET #AD_UNITS.CH%
	USE
		!
		! Locked Block
		!
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ProcessParent IF ERR = 11%
		FILENAME$ = "AD_UNITS"
		CONTINUE HelpError
	END WHEN

	!
	! Finish up the parent if we've gone through the children
	!
	GOTO ProcessParent &
		IF (AD_UNITS::DEP_OBJECT + AD_UNITS::ACTION_DATE) <> UNITS_KEY$

	!
	! Generate a REGISTER record to pass through to the post function
	!
	AD_REGUNIT::ASSET_NUM	= AD_UNITS::ASSET_NUM
	AD_REGUNIT::DEP_OBJECT	= AD_UNITS::DEP_OBJECT
	AD_REGUNIT::PERIOD	= GLPERIOD
	AD_REGUNIT::ACTION_DATE	= AD_UNITS::ACTION_DATE
	AD_REGUNIT::STATIONMAN	= AD_JOURNAL::STATIONMAN
	AD_REGUNIT::QUANTITY	= AD_UNITS::QUANTITY
	AD_REGUNIT::POST_TIME	= POSTTIME
	AD_REGUNIT::POST_DATE	= POSTDATE
	AD_REGUNIT::BATCH	= BATCH_NUMBER

	!
	! Put the record into the temporary AD file
	!
	EXIT_STATUS = AD_TRAN_POSTAD(OPT_ADDREC, SUBOPT_REGISTER+CHECK_PERIOD, &
		BATCH_NUMBER, TITLE(), UTL_REPORTX, &
		"", AD_REGUNIT, GLPERIOD)

	!
	! Check the action date; is it in the correct period?
	!
	SELECT EXIT_STATUS
	!
	! Date good; go on
	!
	CASE CMC$_NORMAL

	!
	! Out of range - set a flag and go on
	!
	CASE CMC$_DATEOUT
		ACTDATE$ = "*"

	!
	! Weird happenings
	!
	CASE ELSE
		GOTO Aborted

	END SELECT

	!
	! Was the action date out of range?
	!
	IF ACTDATE$ = "*"
	THEN
		!
		! Put the invalid stuff in TEXT to send to OUTP_UNDEFCODES
		!
		TEXT = "Production Units  " + &
			ACTDATE$ + PRNT_DATE(AD_UNITS::ACTION_DATE, 8%)

		!
		! Keep undefined codes
		!
		GOTO Aborted &
			IF OUTP_UNDEFCODES(OPT_ADDREC, TITLE(), UTL_REPORTX, &
				TEXT) <> CMC$_NORMAL

		!
		! Blank the flag
		!
		ACTDATE$ = " "

	END IF

	GOTO ChildRec

 ProcessParent:
	!
	! Process the parent record
	!

	GOTO ReadParent

 Confirm:
	!**********************************************************************
	! Confirm posting
	!**********************************************************************
	GOTO Aborted IF OUTP_UNDEFCODES(OPT_CONFIRM, TITLE(), UTL_REPORTX, "") <> CMC$_NORMAL

	%PAGE

	!**********************************************************************
	!	1) Since everything's correct and we have the user's
	!		permission, transfer all data from the temporary
	!		files into the ledger
	!**********************************************************************
	!
	! Begin posting
	!
	GOTO Interrupt IF &
		AD_TRAN_POSTAD(OPT_POSTFILE, SUBOPT_REGISTER, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", GLPERIOD) <> CMC$_NORMAL

	%PAGE

	!
	! Remove file
	!
5000	CLOSE AD_JOURNAL.CH%
	CLOSE AD_UNITS.CH%

5010 !	WHEN ERROR IN
 !		KILL AD_JOURNAL.DEV$ + "AD_JOURNAL_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE 5020
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AD_JOURNAL.DEV$ + "AD_JOURNAL_" + &
		BATCH_NO$ + ".JRL;*")

5020 !	WHEN ERROR IN
 !		KILL AD_UNITS.DEV$ + "AD_UNITS_" + BATCH_NO$ + ".JRL" &
 !			FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE Complete
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(AD_UNITS.DEV$ + "AD_UNITS_" + &
		BATCH_NO$ + ".JRL;*")

 Complete:
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AD_JOURNAL", BATCH_NO$, "", "")

	!PRNT_SUMMARY = SUBOPT_FINAL

 ExitProgram:
	!**********************************************************************
	! Exit normally
	!**********************************************************************
	!
	! Print credit and debit transmittals
	!

	!
	! Print undefined codes (if any)
	!
	TEXT = "File               ActionDate"

	EXIT_STATUS = OUTP_UNDEFCODES(OPT_SUMMARY, TITLE(), UTL_REPORTX, TEXT)

	!
	! Finish up the transmittal
	!
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 Aborted:
	!**********************************************************************
	! Abort process
	!**********************************************************************
	IF INTR_STATUS <> CMC$_WARNING
	THEN
		EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, TITLE(), &
			UTL_REPORTX, "AD_JOURNAL", &
			BATCH_NO$, "", "")
		GOTO ExitProgram
	END IF

	%PAGE

 Interrupt:
	!**********************************************************************
	! Interrupt process
	!**********************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "AD_JOURNAL", BATCH_NO$, &
		"", "")

	GOTO ExitProgram

	%PAGE

 HelpError:
	!**********************************************************************
	! Help Message for an error
	!**********************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

	%PAGE

19000	!**********************************************************************
	! Error trapping
	!**********************************************************************

	!
	! Trap untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

	%PAGE

32000	!**********************************************************************
	! End of posting program AD_POST_UNITS
	!**********************************************************************
	END
