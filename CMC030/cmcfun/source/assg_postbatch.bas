1	%TITLE "Assign Process Batch Number"
	%SBTTL "ASSG_POSTBATCH"
	%IDENT "V3.6a Calico"

	FUNCTION LONG ASSG_POSTBATCH(LONG OPT, &
		STRING BATCH_NUMBER, &
		STRING TITLE(), &
		UTL_REPORTX_CDD UTL_REPORTX, &
		STRING FILE_NAME, &
		STRING BATCHFILE, &
		STRING PERIODONE, &
		STRING PERIODTWO)

	!
	! COPYRIGHT (C) 1986 BY
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Assigns a process batch number.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	OPT
	!		modify the files.
	!	.table
	!		"I" Initilize for POST
	!
	!		"M" Modify POST record
	!
	!		"F" Finish/Cancel POST (deletes record)
	!	.endtable
	!
	!	FILE_NAME
	!		The passed file to be used.
	!
	!	Returned value
	!	.table
	!		0%	- Success
	!
	!		1%	- This process has been interrupted somehow
	!
	!		2%	- Batch control record is missing
	!
	!		4%	- Error - abort post
	!	.endtable
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:ASSG_POSTBATCH
	!	$ LIB FUNC_LIB:CMCFUN/REP ASSG_POSTBATCH
	!	$ DELETE ASSG_POSTBATCH.OBJ;*
	!
	! Author:
	!
	!	12/28/88 - Frank F. Starman
	!
	! Modification History:
	!
	!	06/30/89 - Aaron Redd
	!		Modified so that if a POST (namely, AD_POST_LEDGER)
	!		attempts to abort before a batch number has been
	!		assigned, this function won't die on the POST.
	!
	!	04/30/90 - Frank F. Starman
	!		Replace ENTR_3MESSAGE function with HELP_34MESSAGE.
	!
	!	09/11/90 - Frank F. Starman
	!		Replace HELP_34MESSAGE function with HELP_PRINTMESS.
	!
	!	10/08/91 - Frank F. Starman
	!		Print message if file is locked for modification.
	!
	!	11/14/91 - Kevin Handy
	!		Added added percent signs to key #'s.
	!
	!	02/25/92 - Kevin Handy
	!		Modified to print messages when an error occurs
	!		so that the user will see more than just Frank's
	!		"Aborted" message on the reports, which is
	!		impossible to debug.
	!
	!	05/10/92 - Frank F. Starman
	!		Check for batch number before deleting it.
	!
	!	06/16/92 - Kevin Handy
	!		Clean up (check)
	!
	!	08/14/92 - Kevin Handy
	!		Attempt to fix problem where someone changed all '#'s
	!		to tab characters (Who knows why).
	!
	!	03/24/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/28/94 - Kevin Handy
	!		Modified to use function ASSG_MAKEBATCH to generate
	!		the batch number instead of using built in code.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	02/22/96 - Kevin Handy
	!		Reformat source code.
	!
	!	04/15/96 - Kevin Handy
	!		Added about 1000 spaces to make it more readable.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/04/99 - Kevin Handy
	!		Changed field "program" to "programname"
	!
	!	03/16/99 - Kevin Handy
	!		Use WHEN ERROR
	!
	!	04/20/99 - Kevin Handy
	!		Fix unsolicited input
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_BATCH_CONTROL.HB"
	MAP (UTL_BATCH_CONTROL)	UTL_BATCH_CONTROL_CDD	UTL_BATCH_CONTROL

	COM(UTL_BATCH_CONTROL.COM) UTL_BATCH_CONTROL.CH%, &
		START_PROCESS$ = 6%, &
		START_FILE$ = 6%

	!
	! External functions
	!
	EXTERNAL STRING	FUNCTION READ_SYSLOG
	EXTERNAL STRING	FUNCTION ASSG_MAKEBATCH

	EXTERNAL LONG   OUTP_XUNSOL	! (It's really an AST routine)

	DECLARE LONG EXIT_STATUS
	DECLARE STRING PROCESS_NAME
	DECLARE STRING POSTDATE
	DECLARE STRING POSTTIME

	%PAGE

	!++
	! Error:PROCABORT
	!	^*Process Aborted\*
	!	.p
	!	^*Explanation:\*
	!	.p
	!	Process has been aborted, but no files have been changed
	!	or deleted.
	!	.p
	!	^*User Action:\*
	!	.p
	!	Check undefined codes and restart the process.
	!
	! Index:
	!	.x Process>Aborted
	!
	!--

	!
	! Assume success
	!
	EXIT_STATUS = CMC$_NORMAL

100	!
	! Open file
	!
	IF UTL_BATCH_CONTROL.CH% = 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_BATCH_CONTROL.CRE"
		USE
			FILENAME$ = "UTL_BATCH_CONTROL"
			CONTINUE HelpError
		END WHEN

	END IF

	!*******************************************************************
	! Handle whatever the user wants to do
	!*******************************************************************

	SELECT OPT

	!
	! Check if interrupted
	!
	CASE OPT_RESTART
		GOSUB 1000

	!
	! Assign batch number
	!
	CASE OPT_ASSIGN
		GOSUB 1100

	!
	! Transmittal
	!
	CASE OPT_ADDREC
		GOSUB 1200

	!
	! Open file
	!
	CASE OPT_OPENFILE
		GOSUB 1600

	!
	! Mark file
	!
	CASE OPT_MARKFILE
		GOSUB 1700

	!
	! Completed (deletes record)
	!
	CASE OPT_COMPLETE
		GOSUB 1300
		GOSUB Summary

		CALL HELP_PRINTMESS(SCOPE, "process succesfully completed", &
			"S", SCOPE::PRG_PROGRAM, "", "PROCOMPL", &
			UTL_REPORTX, TITLE(), -1%)

	!++
	! Success:PROCOMPL
	!	^*Process Completed\*
	!	.p
	!	Process has been succesfully completed.
	! Index:
	!	.x Process>Completed
	!--

	!
	! Aborted (deletes record)
	!
	CASE OPT_ABORT
		GOSUB 1300
		GOSUB Summary

		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL HELP_PRINTMESS(SCOPE, "process has been aborted", &
			"W", "ASSG_POSTBATCH", "", "PROCABORT", &
			UTL_REPORTX, TITLE(), 0%)

	!++
	! Warning:PROCABORT
	!	^*Process Aborted\*
	!	.p
	!	^*Explanation:\*
	!	.p
	!	Process has been aborted, but no files have been changed
	!	or deleted.
	!	.p
	!	^*User Action:\*
	!	.p
	!	Check undefined codes and restart the process.
	!
	! Index:
	!	.x Process>Aborted
	!
	!--
		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

	!
	! Interrupted
	!
	CASE OPT_INTERRUPT
		GOSUB 1500

	!
	! Close file
	!
	CASE OPT_CLOSEFILE
		GOSUB 1800

	!
	! Undefined OPT
	!
	CASE ELSE
		TEXT$ = "%ASSG_POSTBATCH Undefined post option"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

		EXIT_STATUS = CMC$_NOOPTION

	END SELECT

 ExitFunction:
	ASSG_POSTBATCH = EXIT_STATUS
	EXIT FUNCTION

	%PAGE

1000	!*******************************************************************
	! Check if interrupted
	!*******************************************************************

	CALL READ_DEVICE(FILE_NAME, FILE.DEV$, STAT%)
	FILE.DEV$ = "SYS$LOGIN" IF TRM$(FILE.DEV$) = ""
	PROCESS_NAME = READ_SYSLOG(FILE.DEV$) + UTL_REPORTX::REPNUM

	START_PROCESS$ = TIME_NOW

	TEXT$ = "process " + TRM$(PROCESS_NAME)
	TEXT$ = TEXT$ + ", user batch number " + BATCHFILE IF BATCHFILE <> ""

	CALL HELP_PRINTMESS(SCOPE, TEXT$, "I", &
		SCOPE::PRG_PROGRAM, "", "PROCSTART", &
		UTL_REPORTX, TITLE(), 0%)

	RESET #UTL_BATCH_CONTROL.CH%

1025	WHEN ERROR IN
		GET #UTL_BATCH_CONTROL.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 5%
			RETRY
		END IF
		CONTINUE Ret1025 IF ERR = 11% !OR ERR = 131%
		FILENAME$ = "UTL_BATCH_CONTROL"
		CONTINUE HelpError
	END WHEN

	IF UTL_BATCH_CONTROL::PROGRAMNAME = &
		LEFT(PROCESS_NAME + SPACE$(30%), 30%) AND &
		TRM$(UTL_BATCH_CONTROL::BFILE) = BATCHFILE
	THEN
		UTL_BATCH_CONTROL::DSTART	= DATE_TODAY
		UTL_BATCH_CONTROL::TSTART	= TIME_NOW
		UTL_BATCH_CONTROL::USTATUS	= "R"
		UTL_BATCH_CONTROL::DESCR	= "Restarting"

		UPDATE #UTL_BATCH_CONTROL.CH%

		BATCH_NUMBER = UTL_BATCH_CONTROL::BATCH
		PERIODONE = UTL_BATCH_CONTROL::UTLFILE
		PERIODTWO = UTL_BATCH_CONTROL::U1FILE

		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL HELP_PRINTMESS(SCOPE, &
			"restarting interrupted process " + BATCH_NUMBER, &
			"I", SCOPE::PRG_PROGRAM, "", "PROCRESTART", &
			UTL_REPORTX, TITLE(), 0%)

	!++
	! Warning:PROCRESTART
	!	^*Restarting Interrupted Process\*
	!	.b
	!	.lm +5
	!	^*Explanation\*
	!	.b
	!	This process has been interrupted and restarted again.
	!	The same batch process number is used.
	!	Any records in the process files with the same batch process
	!	number will be deleted first.
	!	.lm -5
	!
	! Index:
	!	.x Process>Restarted
	!
	!--

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

		EXIT_STATUS = CMC$_WARNING
		GOTO ExitFunction
	END IF

	GOTO 1025

 Ret1025:
	RETURN

1100	!*******************************************************************
	! Assign batch number
	!*******************************************************************

	!
	! Remove the old batch number if any
	!
	GOSUB 1300

	CALL READ_DEVICE(FILE_NAME, FILE.DEV$, STAT%)
	FILE.DEV$ = "SYS$LOGIN" IF TRM$(FILE.DEV$) = ""
	PROCESS_NAME = READ_SYSLOG(FILE.DEV$) + UTL_REPORTX::REPNUM

1110	POSTDATE = DATE_TODAY
	POSTTIME = TIME_NOW

	BATCH_NUMBER = ASSG_MAKEBATCH(POSTDATE, POSTTIME)

	UTL_BATCH_CONTROL::BATCH	= BATCH_NUMBER
	UTL_BATCH_CONTROL::PROGRAMNAME	= PROCESS_NAME
	UTL_BATCH_CONTROL::BFILE	= BATCHFILE
	UTL_BATCH_CONTROL::DSTART	= POSTDATE
	UTL_BATCH_CONTROL::TSTART	= POSTTIME
	UTL_BATCH_CONTROL::USTATUS	= "I"
	UTL_BATCH_CONTROL::DESCR	= "Initialize"
	UTL_BATCH_CONTROL::UTLFILE	= PERIODONE
	UTL_BATCH_CONTROL::U1FILE	= PERIODTWO

1150	WHEN ERROR IN
		PUT #UTL_BATCH_CONTROL.CH%
	USE
		CONTINUE 1110 IF ERR = 134%
		FILENAME$ = "UTL_BATCH_CONTROL"
		CONTINUE HelpError
	END WHEN

	RETURN

	%PAGE

1200	!*******************************************************************
	! Create transmittal
	!*******************************************************************

	WHEN ERROR IN
		GET #UTL_BATCH_CONTROL.CH%, KEY #0% EQ BATCH_NUMBER
	USE
		IF ERR = 154%
		THEN
			SLEEP 5%
			RETRY
		END IF
		FILENAME$ = "UTL_BATCH_CONTROL"
		CONTINUE HelpError
	END WHEN

	!
	! Create a header record
	!
	UTL_BATCH_CONTROL::DESCR	= "Creating transmittal"
	UTL_BATCH_CONTROL::USTATUS	= "T"
	UTL_BATCH_CONTROL::DSTART	= DATE_TODAY
	UTL_BATCH_CONTROL::TSTART	= TIME_NOW

	WHEN ERROR IN
		UPDATE #UTL_BATCH_CONTROL.CH%
	USE
		FILENAME$ = "UTL_BATCH_CONTROL"
		CONTINUE HelpError
	END WHEN

	CALL HELP_PRINTMESS(SCOPE, &
		"assigned process batch number " + BATCH_NUMBER, &
		"I", SCOPE::PRG_PROGRAM, "", "ASSGBATCH", &
		UTL_REPORTX, TITLE(), 0%)

	CALL HELP_PRINTMESS(SCOPE, "creating posting transmittal", &
		"I", SCOPE::PRG_PROGRAM, "", "CRETRAN", &
		UTL_REPORTX, TITLE(), 0%)

	RETURN

	%PAGE

1300	!*******************************************************************
	! Delete batch control record
	!*******************************************************************

	IF BATCH_NUMBER <> ""
	THEN
		WHEN ERROR IN
			GET #UTL_BATCH_CONTROL.CH%, KEY #0% EQ BATCH_NUMBER
			DELETE #UTL_BATCH_CONTROL.CH%
		USE
			IF ERR = 154%
			THEN
				SLEEP 5%
				RETRY
			END IF
			CONTINUE 1310 IF ERR = 145%
			FILENAME$ = "UTL_BATCH_CONTROL"
			CONTINUE HelpError
		END WHEN
	END IF

1310	RETURN

1500	!*******************************************************************
	! Interrupted process
	!*******************************************************************

	WHEN ERROR IN
		GET #UTL_BATCH_CONTROL.CH%, KEY #0% EQ BATCH_NUMBER

		UTL_BATCH_CONTROL::DESCR	= "Interrupted"
		UTL_BATCH_CONTROL::USTATUS	= "N"
		UTL_BATCH_CONTROL::DSTART	= DATE_TODAY
		UTL_BATCH_CONTROL::TSTART	= TIME_NOW

		UPDATE #UTL_BATCH_CONTROL.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 5%
			RETRY
		END IF
		FILENAME$ = "UTL_BATCH_CONTROL"
		CONTINUE HelpError
	END WHEN

	GOSUB Summary

	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	CALL HELP_PRINTMESS(SCOPE, "process has been interrupted", &
		"W", SCOPE::PRG_PROGRAM, "", "PROCINTERR", &
		UTL_REPORTX, TITLE(), 0%)

	!++
	! Warning:PROCINTERR
	!	^*Process Interrupted\*
	!	.p
	!	^*Explanation:\*
	!	.p
	!	Process has been interrupted, because of an unexpected error.
	!	Some files have already been changed, but not completely.
	!	.p
	!	^*User Action:\*
	!	.p
	!	Restart the process as soon as possible.
	!
	! Index:
	!	.x Process>Aborted
	!
	!--

	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, LOC(SCOPE::SMG_KBID) BY VALUE)

	RETURN

1600	!*******************************************************************
	! Open file
	!*******************************************************************

	WHEN ERROR IN
		GET #UTL_BATCH_CONTROL.CH%, KEY #0% EQ BATCH_NUMBER

		UTL_BATCH_CONTROL::USTATUS	= "W"
		UTL_BATCH_CONTROL::DSTART	= DATE_TODAY
		UTL_BATCH_CONTROL::TSTART	= TIME_NOW

		UPDATE #UTL_BATCH_CONTROL.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 5%
			RETRY
		END IF
		FILENAME$ = "UTL_BATCH_CONTROL"
		CONTINUE HelpError
	END WHEN

	START_FILE$ = TIME_NOW
	TEXT$ = SPACE$(9%) + PRNT_TIME(START_FILE$, OPT_HHMMSS) + " "
	TEXT$ = TEXT$ + "Start Time"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	RETURN

1700	!*******************************************************************
	! Mark file
	!*******************************************************************

	WHEN ERROR IN
		GET #UTL_BATCH_CONTROL.CH%, KEY #0% EQ BATCH_NUMBER

		IF UTL_BATCH_CONTROL::DESCR = FILE_NAME AND &
			UTL_BATCH_CONTROL::USTATUS = "M"
		THEN
			TEXT$ = PRNT_DATE(DATE_TODAY, OPT_MMDDYY) + " " + &
				PRNT_TIME(TIME_NOW, OPT_HHMMSS) + " "
			TEXT$ = TEXT$ + FILE_NAME + &
				" file is locked for modification"
			CALL ENTR_3MESSAGE(SCOPE, TEXT$, 1% + 16%)
			GOTO EndMarkFile
		END IF

		UTL_BATCH_CONTROL::DESCR	= FILE_NAME
		UTL_BATCH_CONTROL::USTATUS	= "M"
		UTL_BATCH_CONTROL::DSTART	= DATE_TODAY
		UTL_BATCH_CONTROL::TSTART	= TIME_NOW
		UTL_BATCH_CONTROL::UTLFILE	= PERIODONE IF PERIODONE <> ""
		UTL_BATCH_CONTROL::U1FILE	= PERIODTWO IF PERIODTWO <> ""

		UPDATE #UTL_BATCH_CONTROL.CH%
	USE
		IF ERR = 154%
		THEN
			SLEEP 5%
			RETRY
		END IF
		FILENAME$ = "UTL_BATCH_CONTROL"
		CONTINUE HelpError
	END WHEN

	TEXT$ = PRNT_DATE(UTL_BATCH_CONTROL::DSTART, OPT_MMDDYY) + " " + &
		PRNT_TIME(UTL_BATCH_CONTROL::TSTART, OPT_HHMMSS) + " "
	TEXT$ = TEXT$ + FILE_NAME
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
	CALL ENTR_3MESSAGE(SCOPE, TEXT$, 1% + 16%)

 EndMarkFile:
	RETURN

1800	!*******************************************************************
	! Close file
	!*******************************************************************

	FINISH_FILE$ = TIME_NOW
	TEXT$ = SPACE$(9%) + PRNT_TIME(FINISH_FILE$, OPT_HHMMSS) + " "
	TEXT$ = TEXT$ + "Finish Time"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	ELAPSED_TIME% = TIME_CODE(FINISH_FILE$) - TIME_CODE(START_FILE$)
	ELAPSED_TIME% = 24% * 60% * 60% + ELAPSED_TIME% &
		IF ELAPSED_TIME% < 0%
	ELAPSED_TIME$ = TIME_INVCODE(ELAPSED_TIME%)

	TEXT$ = SPACE$(9%) + PRNT_TIME(ELAPSED_TIME$, OPT_HHMMSS) + " "
	TEXT$ = TEXT$ + "Elapsed Time"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	RETURN

 Summary:
	!
	! Process summary
	!
	FINISH_PROCESS$ = TIME_NOW
	ELAPSED_TIME% = TIME_CODE(FINISH_PROCESS$) - TIME_CODE(START_PROCESS$)
	ELAPSED_TIME% = 24% * 60% * 60% + ELAPSED_TIME% &
		IF ELAPSED_TIME% < 0%
	ELAPSED_TIME$ = TIME_INVCODE(ELAPSED_TIME%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), "", 6%)
	TEXT$ = "Process summary" + STRING$(100%, A"."B)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	TEXT$ = " process name          " + UTL_BATCH_CONTROL::PROGRAMNAME
	TEXT$ = LEFT(TEXT$ + SPACE$(50%), 70%) + &
		"batch number  " + UTL_BATCH_CONTROL::BATCH
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	TEXT$ = " user batch number     " + UTL_BATCH_CONTROL::BFILE
	TEXT$ = LEFT(TEXT$ + SPACE$(50%), 70%) + &
		"start time    " + PRNT_TIME(START_PROCESS$, OPT_HHMMSS)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	TEXT$ = " period 1              " + UTL_BATCH_CONTROL::UTLFILE
	TEXT$ = LEFT(TEXT$ + SPACE$(50%), 70%) + &
		"finish time   " + PRNT_TIME(FINISH_PROCESS$, OPT_HHMMSS)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	TEXT$ = " period 2              " + UTL_BATCH_CONTROL::U1FILE
	TEXT$ = LEFT(TEXT$ + SPACE$(50%), 70%) + &
		"elapsed time  " + PRNT_TIME(ELAPSED_TIME$, OPT_HHMMSS)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)

	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), &
		UTL_REPORTX, TITLE(), 0%)

	EXIT_STATUS = CMC$_UNTERROR

	GOTO ExitFunction

32767	END FUNCTION
