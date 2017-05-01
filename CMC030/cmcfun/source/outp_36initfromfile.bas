1	%TITLE "Initilize REPORT Output Information"
	%SBTTL "OUTP_36INITFROMFILE"
	%IDENT "V3.6a Calico"

	SUB OUTP_36INITFROMFILE(SCOPE_STRUCT SCOPE, &
		UTL_REPORTX_CDD UTL_REPORTX, WORD XWIDTH)

	!
	! COPYRIGHT (C) 1984 BY
	! Computer Management Center, Idaho Falls, Idaho.
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
	!	This function initilizes the REPORT output functions.
	!
	! Parameters:
	!
	!	UTL_REPORTX
	!		The file used to initilize the report functions.
	!
	!	XWIDTH
	!		The returned variable used for the report output.
	!
	!	Returned value
	!		Initilizes the report output functions and other
	!		information the file has.
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_36INITFROMFILE/LINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP OUTP_36INITFROMFILE
	!	$ DELETE OUTP_36INITFROMFILE.OBJ;*
	!
	! Author:
	!
	!	03/23/87 - Kevin Handy
	!
	! Modification history:
	!
	!	10/20/87 - Kevin Handy
	!		Modified for new print types 6, 7, and 8.
	!		(Clipboard, Word processor, file cabinet)
	!
	!	06/03/88 - Kevin Handy
	!		Modifications to make it work over DECNET.
	!		Assumed that the printer type passed through
	!		from report settings will be accurate.
	!
	!	06/16/88 - Kevin Handy
	!		Finished modifications for DECNET.  Required
	!		adding an "ORGANIZATION SEQUENTIAL" to the
	!		open statements.
	!
	!	08/12/88 - Kevin Handy
	!		Modified to allow open of set and device to
	!		occur in the functions that need them.
	!
	!	12/09/88 - Kevin Handy
	!		Modified call for KEYBOARD.OPN so that it
	!		only occured once in the entire program run,
	!		because it was consuming two event flags per
	!		call to this function.
	!
	!	06/27/89 - Kevin Handy
	!		Modified to use READ_INITIALIZE.
	!
	!	10/02/89 - Kevin Handy
	!		Changed error message to give correct function
	!		name.
	!
	!	04/03/90 - Frank F. Starman
	!		Changed more error message to give correct function
	!		name. Call "Please Wait." after initialization.
	!
	!	05/17/91 - Kevin Handy
	!		Added "ALLOW READ" to open for disk file, so that
	!		the report could be examined while it is being
	!		created.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/18/95 - Kevin Handy
	!		Modified to change the screen width only when
	!		in "display" mode.
	!
	!	06/19/95 - Kevin Handy
	!		Modified to close PRNTxxx.TMP file when done
	!		with it.
	!
	!	10/19/95 - Kevin Handy
	!		Copied from OUTP_INITFROMFILE.
	!		Assed SCOPE as a parameter.
	!		Merged in outp_readstructure.
	!		Don't call READ_INITIALIZE (Not sharable).
	!		(Should be able to be sharable now, once all the
	!		functions it calls are sharable)
	!
	!	10/17/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/16/97 - Kevin Handy
	!		Lose code for clipboard, which isn't usable.
	!		Lose code for File Cabinet, wich didn't do anything.
	!
	!	09/29/97 - Kevin Handy
	!		Increased recordsize on open of local printer
	!		and local files from 255 to 511.
	!		Tried to Add 'NOMARGIN' statements, didn't work.
	!
	!	10/13/97 - Kevin Handy
	!		Load "PT" into PRINTTYPE
	!
	!	11/11/97 - Kevin Handy
	!		Use constants instead of hard coded numbers
	!		for ::PRINTTO comparisons.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	02/05/99 - Kevin Handy
	!		Remove some duplicated code dealing with
	!		'REPWIDTH = 0'
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET fro LIB$ routines
	!
	!	12/22/99 - Kevin Handy
	!		Use 'WHEN ERROR IN' routines where possible
	!
	!	09/14/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!
	!	10/26/2000 - Kevin Handy
	!		Another attempt to fix problems with printer
	!		port and kermit. Use channel #0 when user asks
	!		for "tt:" instead of opening up another channel.
	!
	!	05/03/2001 - Kevin Handy
	!		Change how TODEVICE works. Only open output
	!		device once. It works for everything else, so
	!		why not for that.
	!
	!	06/26/2001 - Kevin Handy
	!		Move deleting of PRNT.TMP file until after it has
	!		been copied and closed, not before.
	!		Lose the chaining of programs, since it is not
	!		used anywhere, and it really bolloxes up the works
	!		everywhere.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$SSDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$DVIDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION SYS$FILESCAN

	!
	! Declare vars
	!
	DECLARE LONG SYS_STATUS, TEMPLONG
	DECLARE LONG CONSTANT DC$_TERMINAL = 66
	DECLARE LONG CONSTANT FSCN$_NAME = 6

	!
	! Create a couple of buffers
	!
	RECORD IOBUF_RECORD
		VARIANT
		CASE
			LONG IO_BUF(6%)
		CASE
			WORD IO_BUF_W(12%)
		END VARIANT
	END RECORD

	DECLARE IOBUF_RECORD IOBUF

	RECORD NAME_RECORD
		STRING NAME_BUFFER = 50%
	END RECORD

	DECLARE NAME_RECORD NAME_BUFFER

	%PAGE

	ON ERROR GOTO 19000

	!******************************************************************
	! Initilization
	!******************************************************************

600	!
	! Assume no errors
	!
	UTL_REPORTX::STAT = 0%

	!
	! Set up paging information
	!
	UTL_REPORTX::PAGENO = 0%		! Start without a page number
	UTL_REPORTX::LINENO = 0%		! Not on a line

	!
	! Open up report file and read information for this report
	!
	SYS_STATUS = LIB$GET_SYMBOL("CMC$REPORT" BY DESC, TEMPFILE$ BY DESC,,)

	IF (SYS_STATUS AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Unable to find WORK file name!", 0%)
		UTL_REPORTX::STAT = SYS_STATUS
		EXIT SUB
	END IF

	TEMPFILE$ = TRM$(TEMPFILE$)

	!
	! Allocate a channel for report
	!
	CALL ASSG_CHANNEL(UTL_REPORTX::CHAN, STAT%)

	OPEN TEMPFILE$ FOR INPUT AS FILE UTL_REPORTX::CHAN

	!
	! Assume no errors
	!
	UTL_REPORTX::STAT = 0%
	RN_FLAG% = 0%

	!
	! Set up some defaults
	!
	UTL_REPORTX::AUTOSCROLL = 0%
	UTL_REPORTX::OPTDEF(0%) = ""
	UTL_REPORTX::PRINTINIT = ""
	UTL_REPORTX::PRONAM = ""
	UTL_REPORTX::OFFSET = 0%

700	!
	! Read in one line of source file
	!
	WHEN ERROR IN
		LINPUT #UTL_REPORTX::CHAN, INLINE$
	USE
		IF ERR = 11%
		THEN
			INLINE$ = ""
			CONTINUE 750
		END IF
		CONTINUE Crash
	END WHEN

	LEFTT$ = LEFT(INLINE$, 2%)
	RIGHTT$ = RIGHT(INLINE$, 4%)

	!
	! Process one line of input
	!
	SELECT LEFTT$

	!
	! PG - Program name
	!
	CASE "PG"
		NAME_BUFFER::NAME_BUFFER = RIGHTT$

		!
		! Strip off all but the program name
		!
		IOBUF::IO_BUF_W(1%) = FSCN$_NAME
		IOBUF::IO_BUF_W(0%) = 0%
		IOBUF::IO_BUF(1%) = 0%
		IOBUF::IO_BUF(2%) = 0%
		IOBUF::IO_BUF(3%) = 0%
		SYS_STATUS% = SYS$FILESCAN( &
			NAME_BUFFER::NAME_BUFFER BY DESC, &
			IOBUF::IO_BUF() BY REF, 0%)
		TEMP_LONG% = IOBUF::IO_BUF(1%)
		TEMP1_LONG% = LOC(NAME_BUFFER::NAME_BUFFER)
		TEMP_LONG% = TEMP_LONG% - &
			TEMP1_LONG% + 1%

		UTL_REPORTX::PRONAM = MID(NAME_BUFFER::NAME_BUFFER, &
			TEMP_LONG%, IOBUF::IO_BUF_W(0%))

		UTL_REPORTX::PRODEV = LEFT(NAME_BUFFER::NAME_BUFFER, &
			TEMP_LONG% - 1%)

	!
	! LP - Lines/page
	!
	CASE "LP"
		UTL_REPORTX::PAGELEN = VAL%(RIGHTT$)

	!
	! SP - Start page
	!
	CASE "SP"
		UTL_REPORTX::STARTP = VAL%(RIGHTT$)

	!
	! EP - End page
	!
	CASE "EP"
		UTL_REPORTX::ENDP = VAL%(RIGHTT$)

	!
	! CP - Copies
	!
	CASE "CP"
		UTL_REPORTX::COPIES = VAL%(RIGHTT$)

	!
	! AF - After
	!
	CASE "AF"
		UTL_REPORTX::AFTERTIME = RIGHTT$

	!
	! BG - Background
	!
	CASE "BG"
		UTL_REPORTX::BACKGROUND = RIGHTT$

	!
	! OF - Offset
	!
	CASE "OF"
		UTL_REPORTX::OFFSET = VAL%(RIGHTT$)

	!
	! RD - Report date
	!
	CASE "RD"
		UTL_REPORTX::REPDATE = RIGHTT$

	!
	! PD - Print report date on report
	!
	CASE "PD"
		UTL_REPORTX::REPYN = RIGHTT$

	!
	! AS - Auto Scroll
	!
	CASE "AS"
		UTL_REPORTX::AUTOSCROLL = VAL%(RIGHTT$)

	!
	! SP - Spooler name
	!
	CASE "SL"
		UTL_REPORTX::SPOOL = RIGHTT$

	!
	! SF - Spooler Form name
	!
	CASE "SF"
		UTL_REPORTX::SPOOLFORM = RIGHTT$

	!
	! OD - Output device
	!
	CASE "OD"
		RN_FLAG% = -1%
		UTL_REPORTX::DEFOUT = RIGHTT$

	!
	! XX - PRINTTO definition
	!
	CASE "XX"
		UTL_REPORTX::PRINTTO = VAL%(RIGHTT$)

	!
	! TL - TOLOCAL Output to local printer
	!
	CASE "TL"
		UTL_REPORTX::TOLOCAL = RIGHTT$

	!
	! TS - TOSCREEN return control to screen
	!
	CASE "TS"
		UTL_REPORTX::TOSCREEN = RIGHTT$

	!
	! Un - User entries
	!
	CASE "U1"
		RN_FLAG% = -1%
		UTL_REPORTX::OPTDEF(0%) = RIGHTT$
	CASE "U2"
		UTL_REPORTX::OPTDEF(1%) = RIGHTT$
	CASE "U3"
		UTL_REPORTX::OPTDEF(2%) = RIGHTT$
	CASE "U4"
		UTL_REPORTX::OPTDEF(3%) = RIGHTT$
	CASE "U5"
		UTL_REPORTX::OPTDEF(4%) = RIGHTT$
	CASE "U6"
		UTL_REPORTX::OPTDEF(5%) = RIGHTT$
	CASE "U7"
		UTL_REPORTX::OPTDEF(6%) = RIGHTT$
	CASE "U8"
		UTL_REPORTX::OPTDEF(7%) = RIGHTT$
	CASE "U9"
		UTL_REPORTX::OPTDEF(8%) = RIGHTT$
	CASE "U0"
		UTL_REPORTX::OPTDEF(9%) = RIGHTT$

	!
	! RN - Next report
	!
	CASE "RN"
		GOTO 1100 IF RN_FLAG%
		UTL_REPORTX::REPNUM = RIGHTT$

	!
	! PC - Printer control string
	!
	CASE "PC"
		UTL_REPORTX::PRINTINIT = RIGHTT$

	!
	! PT - Printer Type
	!
	CASE "PT"
		UTL_REPORTX::PRINTTYPE = RIGHTT$

	!
	! ZZ - Printer control string
	!
	CASE "ZZ"
		UTL_REPORTX::PRINTFINISH = RIGHTT$

	!
	! NP - Next page control string
	!
	CASE "NP"
		UTL_REPORTX::NEXTPAGE = RIGHTT$

	END SELECT

	GOTO 700

	%PAGE

750	!
	! Finish up input file.
	!

	!
	! Set up paging information
	!
	UTL_REPORTX::PAGENO = 0%		! Start without a page number
	UTL_REPORTX::LINENO = 0%		! Not on a line

	!
	! Initilize the width variable
	!
	UTL_REPORTX::REPWIDTH = 132% IF UTL_REPORTX::REPWIDTH = 0%
	UTL_REPORTX::REPWIDTH = XWIDTH IF XWIDTH <> 0%
	UTL_REPORTX::PAGELEN  = 66%  IF UTL_REPORTX::PAGELEN  = 0%

	!
	! Keyboard open needed
	!
	IF UTL_REPORTX::PRINTTO = OUTP_TODISPLAY
	THEN
		SCOPE::SCREEN_WIDTH = UTL_REPORTX::REPWIDTH
		SCOPE::SCREEN_WIDTH = 80% IF SCOPE::SCREEN_WIDTH = 0%

		SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, &
			SCOPE::SCREEN_WIDTH)
	END IF

	CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

1000	!******************************************************************
	! Remove group from print file
	!******************************************************************

	!
	! Allocate a channel for the new copy of the file
	!
	CALL ASSG_CHANNEL(TEMPLONG, STAT%)

	IF STAT% <> 0%
	THEN
		UTL_REPORTX::STAT = STAT%
		EXIT SUB
	END IF

	!
	! If no report number (RN>) is passed through OUTP_READSTRUCTURE
	! then this is the last report to run.  No need to create a new
	! temp file.
	!
 !	GOTO 1100 IF INLINE$ = ""

	!
	! Create new copy of the settings file
	!
 !	OPEN TEMPFILE$ FOR OUTPUT AS FILE TEMPLONG
 !	REPORT.DATA$ = ""

1010	!
	! Copy over all remaining lines
	!
 ! Nxt:	PRINT #TEMPLONG, INLINE$
 !
 !	IF (LEFT(INLINE$, 3%) = "PG>") AND (REPORT.DATA$ = "")
 !	THEN
 !		REPORT.DATA$ = INLINE$
 !	END IF
 !
 !	WHEN ERROR IN
 !		LINPUT #UTL_REPORTX::CHAN, INLINE$
 !	USE
 !		CONTINUE 1100 IF ERR = 11%
 !		CONTINUE Crash
 !	END WHEN
 !
 !	GOTO Nxt

1100 !	UTL_REPORTX::NEXTRUN = RIGHT(REPORT.DATA$, 4%)
	UTL_REPORTX::NEXTRUN = ""
 !	CLOSE TEMPLONG
	CALL ASSG_FREECHANNEL(TEMPLONG)
	CLOSE UTL_REPORTX::CHAN

	!
	! Delete the old file
	!
 !	KILL TEMPFILE$
	SMG_STATUS% = LIB$DELETE_FILE(TEMPFILE$ + ";*")

	IF (SMG_STATUS% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Error deleting temp print file (" + &
			NUM1$(SMG_STATUS%) + ")", 4%)
	END IF

	!******************************************************************
	! Prepare for output of information
	!******************************************************************

	CALL WRIT_STRING(UTL_REPORTX::PRINTINIT, PRINTINIT$)
	CALL WRIT_STRING(UTL_REPORTX::TOLOCAL, TOLOCAL$)
	CALL WRIT_STRING(UTL_REPORTX::TOSCREEN, TOSCREEN$)

	!
	! Initilize the flags
	!
	SELECT UTL_REPORTX::PRINTTO

	!
	! Display
	!
	CASE OUTP_TODISPLAY
		!
		! No start/end pages
		!
		UTL_REPORTX::STARTP, UTL_REPORTX::ENDP = 0%

		!
		! Length of report is 18 lines
		!
		UTL_REPORTX::PAGELEN = 20%

		!
		! Create display for this report
		!
		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
		( &
			UTL_REPORTX::PAGELEN, &
			UTL_REPORTX::REPWIDTH * 1%, &
			UTL_REPORTX::WINDOW &
		)

		!
		! Paste on the virtual display
		!
		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
		( &
			UTL_REPORTX::WINDOW, &
			SCOPE::SMG_PBID, &
			1%, &
			1% &
		)

	!
	! To terminal
	!
	CASE OUTP_TODEVICE

 !
 ! FIXME: Why the double open, first with RECORDTYPE NONE, then
 ! without. I'm reverting this nonsense for now to see if it becomes
 ! obvious.
 !
 ! Apparently it is needed to keep from getting an extra line feed
 ! before the initilization string is sent out, which causes many
 ! printers to advance the page one line every time a report starts.
 ! Yuck.
 !
 !		OPEN UTL_REPORTX::DEFOUT AS FILE UTL_REPORTX::CHAN, &
 !			ORGANIZATION SEQUENTIAL, &
 !			ACCESS APPEND, &
 !			RECORDSIZE 511%

		OPEN UTL_REPORTX::DEFOUT AS FILE UTL_REPORTX::CHAN, &
			ORGANIZATION SEQUENTIAL, &
			ACCESS APPEND, &
			RECORDSIZE 511%, &
			RECORDTYPE NONE

		PRINT #UTL_REPORTX::CHAN, PRINTINIT$;

		CLOSE UTL_REPORTX::CHAN

		OPEN UTL_REPORTX::DEFOUT AS FILE UTL_REPORTX::CHAN, &
			ORGANIZATION SEQUENTIAL, &
			ACCESS APPEND, &
			RECORDSIZE 511%

	!
	! Local printer
	!
	CASE OUTP_TOLOCAL
		!
		! If we are going to the terminal, use the default terminal
		! channel to reduce buffering problems between SMG and
		! PRINT statements.
		!
		IF UTL_REPORTX::DEFOUT = "TT:"
		THEN
			CALL ASSG_FREECHANNEL(UTL_REPORTX::CHAN)
			UTL_REPORTX::CHAN = 0%
			MARGIN 511%
		ELSE
			!
			! Open keyboard for output
			!
			OPEN UTL_REPORTX::DEFOUT AS FILE UTL_REPORTX::CHAN, &
				ACCESS APPEND, &
				RECORDSIZE 511%
		END IF

		PRINT #UTL_REPORTX::CHAN, TOLOCAL$; &
			PRINTINIT$; &
			TOSCREEN$;

	!
	! Else a file
	!
	CASE OUTP_TOWP, OUTP_TODOCUMENT, OUTP_TO2020, OUTP_TOPL

		OPEN UTL_REPORTX::DEFOUT AS FILE UTL_REPORTX::CHAN, &
			ORGANIZATION SEQUENTIAL, &
			ACCESS APPEND, &
			RECORDSIZE 511%, &
			ALLOW READ

	!
	! Else a file
	!
	CASE ELSE
		OPEN UTL_REPORTX::DEFOUT AS FILE UTL_REPORTX::CHAN, &
			ORGANIZATION SEQUENTIAL, &
			ACCESS APPEND, &
			RECORDSIZE 511%, &
			ALLOW READ

		PRINT #UTL_REPORTX::CHAN, PRINTINIT$;

	END SELECT

	EXIT SUB

	%PAGE

 Crash:	!
	! Exit from function with an error
	!
	UTL_REPORTX::STAT = ERR

	CALL ENTR_3MESSAGE(SCOPE, "ERROR: OUTP_36INITFROMFILE (" + &
		NUM1$(ERR) + ") " + ERT$(ERR) + " at line " + NUM1$(ERL) + &
		" in " + ERN$, 4%)
	CALL ENTR_3MESSAGE(SCOPE, "ERROR: OUTP_36INITFROMFILE (" + &
		INLINE$ + ") ", 4%)

	EXIT SUB

19000	!*******************************************************************
	! Trap errors
	!

	!
	! Untrapped error
	!
	RESUME Crash

	END SUB
