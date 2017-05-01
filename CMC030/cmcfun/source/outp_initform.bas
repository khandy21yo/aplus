1	%TITLE "Initilize Forms Output Information"
	%SBTTL "OUTP_INITFORM"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OUTP_INITFORM(UTL_REPORTX_CDD UTL_REPORTX, &
		STRING REPORTNUM, STRING FIXSET)

	!
	! COPYRIGHT (C) 1992 BY
	! Computer Management Center, INC.
	! Idaho Falls, Idaho  83402
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
	!	.p
	!	This function initilizes the REPORT output functions
	!	for most forms.
	!
	!	This function was written to remove redundancies in the
	!	forms, and makes fixing some bugs in forms much easier.
	!
	! Parameters:
	!
	!	UTL_REPORTX
	!		The file used to initilize the report functions.
	!
	!	REPORTNUM
	!		The report number to look up
	!
	!	FIXSET
	!		String used to initialize UTL_REPORTX fields from
	!		0 to 9.
	!
	!	Returned value
	!		Initilizes the report output functions and other
	!		information the file has.
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_INITFORM/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OUTP_INITFORM
	!	$ DELETE OUTP_INITFORM.OBJ;*
	!
	! Author:
	!
	!	06/08/92 - Kevin Handy
	!
	! Modification history:
	!
	!	06/09/92 - Kevin Handy
	!		Fixed error trap for record locked problems.
	!
	!	06/09/92 - Kevin Handy
	!		Modified to only open REPORT and SYSREP once.
	!
	!	06/10/92 - Kevin Handy
	!		Modified to kill temp file more often.
	!
	!	06/16/92 - Kevin Handy
	!		Clean up (check)
	!
	!	08/12/92 - Kevin Handy
	!		Modified to allow input of fields in addition to
	!		the defaults (such as "RD").
	!
	!	01/27/93 - Dan Perkins
	!		Modified to handle form "DIRECT".
	!
	!	01/27/93 - Kevin Handy
	!		Fixed bug where it wouldn't set any field except 01.
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards.
	!		Lost extra parameter on calls to ASSG_FREECHANNEL.
	!
	!	05/18/95 - Kevin Handy
	!		Added "AF OF" to list of default items to include
	!		on settings screen. (After, Offset)
	!
	!	06/19/95 - Kevin Handy
	!		Remove some extra formatting, such as two
	!		"PAGE" commands in a row, and extra function
	!		definitions.
	!
	!	10/17/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/30/97 - Kevin Handy
	!		Use OUTP_3WRITESTRUCTURE version. (Shareable
	!		version)
	!
	!	05/23/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for lIB$ routines
	!
	!	06/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	09/14/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!
	!	07/09/2001 - Kevin Handy
	!		Add a lot of tests for the DIRECT flag to reduce
	!		the amount of repainting that occurs in the
	!		Point of Sale 'invprinT' option.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT) UTL_REPORT_CDD UTL_REPORT
	DECLARE UTL_REPORT_CDD UTL_REPORT_SYS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	%INCLUDE "FUNC_INCLUDE:PRINT35.INC"
	MAP (PRINTX) PRINTX_CDD PRINTX

	!
	! Common areas
	!
	COM (CH_UTL_REPORT) &
		UTL_REPORT.CH%

	COM (CH_UTL_SYSREP) &
		UTL_SYSREP.CH%

	DECLARE LONG SMG_BLANK

	%PAGE

	ON ERROR GOTO 19000

	JJ$ = READ_SYSJOB
	CALL ASSG_CHANNEL(PRNT.CH%, STAT%)

330	!
	! Open REPORT file
	!
	IF UTL_REPORT.CH% = 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.MOD"
		USE
			FILENAME$ = "UTL_REPORT"
			CONTINUE HelpError
		END WHEN
	END IF

340	!
	! Get Report
	!
	USER_REPORT% = 0%
	WHEN ERROR IN
		GET #UTL_REPORT.CH%, KEY #0% EQ REPORTNUM
	USE
		CONTINUE 350 IF ERR = 155%
		CONTINUE 345 IF ERR = 154%
		CONTINUE HelpError
	END WHEN

	UTL_REPORT_SYS = UTL_REPORT

	USER_REPORT% = -1%

	GOTO 350

345	!
	! Get Report
	!
	USER_REPORT% = 0%
	GET #UTL_REPORT.CH%, KEY #0% EQ REPORTNUM, REGARDLESS

	UTL_REPORT_SYS = UTL_REPORT

	USER_REPORT% = -2%

350	!
	! Open system file
	!
	WHEN ERROR IN
		IF UTL_SYSREP.CH% = 0%
		THEN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_SYSREP.OPN"
		END IF

		!
		! Get report from system report file
		!
		GET #UTL_SYSREP.CH%, KEY #0% EQ REPORTNUM, REGARDLESS
		CLOSE #UTL_SYSREP.CH%
		CALL ASSG_FREECHANNEL(UTL_SYSREP.CH%)
	USE
		FILENAME$ = "CMC:UTL_REPORT"
		CONTINUE HelpError
	END WHEN

360	!
	! Put or update record into report file
	!
	IF USER_REPORT%
	THEN
		FOR I% = 0% TO 9%
			UTL_REPORT::OPTDEF(I%) = UTL_REPORT_SYS::OPTDEF(I%)
			UTL_REPORT::ITEMGROUP(I%)=UTL_REPORT_SYS::ITEMGROUP(I%)
			UTL_REPORT::ITEM(I%) = UTL_REPORT_SYS::ITEM(I%)
		NEXT I%

		UTL_REPORT::DEFOUT = UTL_REPORT_SYS::DEFOUT
		UTL_REPORT::PRINTTYPE = UTL_REPORT_SYS::PRINTTYPE
		UTL_REPORT::SPOOLFORM = UTL_REPORT_SYS::SPOOLFORM
		UTL_REPORT::LASTRUNDATE = UTL_REPORT_SYS::LASTRUNDATE
		UTL_REPORT::LASTRUNTIME = UTL_REPORT_SYS::LASTRUNTIME
		UTL_REPORT::BASERUNDATE = UTL_REPORT_SYS::BASERUNDATE

		WHEN ERROR IN
			UPDATE #UTL_REPORT.CH% IF USER_REPORT% = -1%
		USE
			FILENAME$ = "UTL_REPORT"
			CONTINUE HelpError
		END WHEN
	ELSE
		WHEN ERROR IN
			PUT #UTL_REPORT.CH%
		USE
			FILENAME$ = "UTL_REPORT"
			CONTINUE HelpError
		END WHEN
	END IF


500	!******************************************************************
	! Set up the report settings screen
	!******************************************************************

	!
	! store original values for the help message
	!
	TEMP_IDENT$ = SCOPE::PRG_IDENT
	TEMP_PROGRAM$ = SCOPE::PRG_PROGRAM

	XLOOP% = 0%

510	!******************************************************************
	! Set up the report settings screen
	!******************************************************************
	XLOOP% = XLOOP% + 1%
	TEMPFILE$ = "PRNT" + JJ$ + "_" + NUM1$(XLOOP%) + ".TMP"

	IF FIND_FILEEXISTS(UTL_WORK.DEV$ + TEMPFILE$, FLAG%)
	THEN
		GOTO 510
	END IF

	CLOSE PRNT.CH%

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + TEMPFILE$ FOR OUTPUT AS FILE PRNT.CH%
	USE
		FILENAME$ = "TEMP"
		CONTINUE HelpError
	END WHEN

	SYS_STATUS% = LIB$SET_SYMBOL("CMC$REPORT", TEMPFILE$,)

	IF (SYS_STATUS% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to declare symbol for work file. " + &
			NUM1$(SYS_STATUS%), 0%)
		GOTO ExitProgram
	END IF

520	!
	! Get the report record
	!
	WHEN ERROR IN
		GET #UTL_REPORT.CH%, KEY #0% EQ REPORTNUM
	USE
		CONTINUE 525 IF ERR = 154%
		FILENAME$ = "UTL_REPORT"
		CONTINUE HelpError
	END WHEN

	GOTO 530

525	!
	! Get the report record
	!
	GET #UTL_REPORT.CH%, KEY #0% EQ REPORTNUM, REGARDLESS

	GOTO 530

530	!
	! Because this is a form we must set the program name
	! in to the program field in the utl_report record
	!
	UTL_REPORT::PRONAM = SCOPE::PRG_PROGRAM

	!
	! Set up for the window
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	UTL_REPORTX::WINDOW = 0%

	!
	! Initilize defaults from report file
	!
	CALL OUTP_INITSTRUCTURE(UTL_REPORT, UTL_REPORTX)

	TEMP$ = FIXSET

540	!
	! Load up any special values to shove into fields
	!
	SETFLG$  = ""
	DIRECTFLAG% = 0%

	WHILE TEMP$ <> ""
		I% = INSTR(1%, TEMP$, ",")
		I% = LEN(TEMP$) + 1% IF I% = 0%

		IF LEFT(TEMP$, I% - 1%) = "DIRECT"
		THEN
			DIRECTFLAG% = -1%
		ELSE
			IF INSTR(1%, "01", LEFT(TEMP$, 1%))
			THEN
				TEMP% = VAL%(LEFT(TEMP$, 2%))
				UTL_REPORTX::OPTDEF(TEMP%) = &
					SEG$(TEMP$, 3%, I% - 1%)
			ELSE
				SETFLG$ = SETFLG$ + LEFT(TEMP$, 2%) + " "
			END IF
		END IF
		TEMP$ = RIGHT(TEMP$, I% + 1%)
	NEXT

	IF DIRECTFLAG%
	THEN
		UTL_REPORTX::SPOOL = "DIRECT"
	END IF

	!
	! Ask user to change settings
	!
	CALL OUTP_SETTINGS(UTL_REPORT, UTL_REPORTX, UTL_REPORT.CH%, &
		"DD SF SP EP CP AF OF AS " + SETFLG$, "PT ")

	UNLOCK #UTL_REPORT.CH%

	!
	! Un-normal abort, exit, etc.
	!
	IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F10) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_CTRLC) OR &
		(SCOPE::SCOPE_EXIT = SMG$K_TRM_F8)
	THEN
		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(UTL_REPORTX::WINDOW) &
			IF DIRECTFLAG% = 0%

		GOSUB KillTempFIle

		GOTO ExitProgram
	END IF

	!
	! Erase option and message window
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	IF DIRECTFLAG% = 0%
	THEN
		!
		! Create a blank display window
		!
		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(20%, 132%, SMG_BLANK)

		!
		! Paste on blank display to hide the width change
		!
		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_BLANK, &
			SCOPE::SMG_PBID, 1%, 1%)

		!
		! Delete report window
		!
		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(UTL_REPORTX::WINDOW)
	END IF

	!
	! Write the data out to the ascii file
	!
	CALL OUTP_3WRITESTRUCTURE(UTL_REPORTX, PRNT.CH%, PRINTX)

	CLOSE PRNT.CH%

	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF DIRECTFLAG% = 0%
	THEN
		SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_BLANK)
	END IF

	!
	! Restore original values for the help message
	!
	SCOPE::PRG_IDENT = TEMP_IDENT$
	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$

	GOSUB KillTempFile

	OUTP_INITFORM = CMC$_NORMAL

	EXIT FUNCTION

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	GOSUB KillTempFIle

	!
	! Erase Display
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	!
	! Change the width
	!
	IF DIRECTFLAG% = 0%
	THEN
		SMG_STATUS% = &
			SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, 80%)
	END IF

	CALL ASSG_FREECHANNEL(PRNT.CH%)
	OUTP_INITFORM = CMC$_ABORT

	EXIT FUNCTION

	%PAGE

18900	!******************************************************************
	! Delete temp file to prepare for exit
	!******************************************************************
 KillTempFile:
	CLOSE PRNT.CH%

 !	WHEN ERROR IN
 !		KILL UTL_WORK.DEV$ + TEMPFILE$ FOR I% = 1% TO 10%
 !	USE
 !		CONTINUE 18910
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(UTL_WORK.DEV$ + TEMPFILE$ + ";*")

	CALL ASSG_FREECHANNEL(PRNT.CH%)

18910	RETURN

	%PAGE

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

 HelpError:
19990	!
	! This moved from inside error to outside so that errors occuring
	! at lower levels could be trapped.  Basic will not allow any
	! error to occur inside of an error no matter if it is in a
	! different module.
	!
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram


	END FUNCTION
