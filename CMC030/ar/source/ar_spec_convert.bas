1	%TITLE "Accounts Receivable Conversion Program"
	%SBTTL "AR_SPEC_CONVERT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	!	This program converts the AR system from one release
	!	to the next.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Author:
	!
	!	03/25/88 - Aaron Redd
	!
	! Compile:
	!
	!	$  BAS AR_SOURCE:AR_SPEC_CONVERT.BAS/LINE
	!	$  LINK/EXE=AR_EXE:*.EXE AR_SPEC_CONVERT, FUNC_LIB:CMCLINK/OPTION
	!	$  DELETE AR_SPEC_CONVERT.OBJ;*
	!
	! Modification history:
	!
	!	05/09/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	06/06/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	03/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/11/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$LIBRARY instead of defining external LIB$
	!		routines.
	!
	!	04/09/99 - Kevin Handy
	!		Fix parameters for SET_BROADCAST_TRAPPING
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP (AR_CLOSED)		AR_CLOSED_CDD	AR_CLOSED

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD	AR_OPEN

	MAP (AR_O) STRING AR_O.CUSNUM = 10%, &
		STRING AR_O.INVNUM = 8%, &
		STRING AR_O.TRATYP = 2%, &
		STRING AR_O.TRADAT = 8%, &
		REAL AR_O.SALAMT, &
		REAL AR_O.DISAMT, &
		REAL AR_O.OTHCHG, &
		STRING AR_O.RECNUM = 8%, &
		STRING AR_O.CHKNUM = 6%, &
		STRING AR_O.ARACCT = 18%, &
		STRING AR_O.SUBACC = 10%, &
		STRING AR_O.DESCR = 25%, &
		STRING AR_O.BATCH = 6%, &
		STRING AR_O.UPDATED = 6%, &
		STRING AR_O.FILLER = 2%, &
		STRING AR_O.CLOSEDATE = 6%

	MAP (AR_C) STRING AR_C.CUSNUM = 10%, &
		STRING AR_C.INVNUM = 8%, &
		STRING AR_C.TRATYP = 2%, &
		STRING AR_C.TRADAT = 8%, &
		REAL AR_C.SALAMT, &
		REAL AR_C.DISAMT, &
		REAL AR_C.OTHCHG, &
		STRING AR_C.RECNUM = 8%, &
		STRING AR_C.CHKNUM = 6%, &
		STRING AR_C.ARACCT = 18%, &
		STRING AR_C.SUBACC = 10%, &
		STRING AR_C.DESCR = 25%, &
		STRING AR_C.BATCH = 6%, &
		STRING AR_C.UPDATED = 8%, &
		STRING AR_C.CLOSEDATE = 6%

	EXTERNAL LONG		READ_3BROADCAST

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize
	!*******************************************************************

	CALL READ_INITIALIZE

	CALL ASSG_CHANNEL(AR_OPEN_OLD.CH%,STAT%)
	CALL ASSG_CHANNEL(AR_CLOSED_OLD.CH%,STAT%)

	CALL READ_DEVICE("AR_OPEN",AR_OPEN.DEV$, STAT%)
	CALL READ_DEVICE("AR_CLOSED",AR_CLOSED.DEV$, STAT%)

	!*******************************************************************
	! Explain to user what is going to happen
	!*******************************************************************

	!
	! Create the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(20%, 80%, SMG_SCREEN_DATA%,,,)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, 1%, 1%)
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, &
		"This process will convert AR files from version " + &
		"3.1 to 3.2", 2%, 2%)

	YESNO$ = EDIT$(ENTR_3YESNO(SCOPE, SMG_SCREEN_DATA%, "", &
		"Confirm Process ", "N", 0%, "", ""), -1%)

	IF YESNO$ <> "Y"
	THEN
		GOTO ExitProgram
	END IF


190	!
	! Rename files to be converted.
	!
	SMG_STATUS% = SMG$DISABLE_BROADCAST_TRAPPING(SCOPE::SMG_PBID)

	CALL ENTR_3MESSAGE(SCOPE, "Renaming files.",1%+16%)

	SMG_STATUS% = LIB$SPAWN("RENAME " + AR_OPEN.DEV$ + &
		"AR_OPEN.LED " + AR_OPEN.DEV$ + &
		"AR_OPEN_OLD.LED")

	IF SMG_STATUS% <> 1% AND SMG_STATUS% <> 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Error " + NUM1$(SMG_STATUS%) + &
			" has occured",0%)
		GOTO ExitProgram
	END IF

	SMG_STATUS% = LIB$SPAWN("RENAME " + AR_CLOSED.DEV$ + &
		"AR_CLOSED.LED " + AR_CLOSED.DEV$ + &
		"AR_CLOSED_OLD.LED")

	IF SMG_STATUS% <> 1% AND SMG_STATUS% <> 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Error " + NUM1$(SMG_STATUS%) + &
			" has occured",0%)
		GOTO ExitProgram
	END IF

	SLEEP 1%

	SMG_STATUS% = SMG$SET_BROADCAST_TRAPPING(SCOPE::SMG_PBID, &
		LOC(READ_3BROADCAST) BY VALUE, LOC(SCOPE) BY VALUE)

	SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)

300	!
	! Open AR OPEN file
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.CRE"

310	!======================================================================
	! AR_OPEN file (open read only)
	!======================================================================

	AR_OPEN.NAME_OLD$ = AR_OPEN.DEV$+"AR_OPEN_OLD.LED"

	OPEN AR_OPEN.NAME_OLD$ FOR INPUT AS FILE AR_OPEN_OLD.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_O, &
		PRIMARY KEY &
		( &
			AR_O.CUSNUM, &
			AR_O.INVNUM &
		)	DUPLICATES, &
		ALTERNATE KEY &
			AR_O.BATCH &
			DUPLICATES CHANGES, &
		ACCESS READ, ALLOW MODIFY

320	!
	! Open AR CLOSED file
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.CRE"

330	!======================================================================
	! AR_CLOSED file (open read only)
	!======================================================================

	AR_CLOSED_OLD.NAME$ = AR_CLOSED.DEV$+"AR_CLOSED_OLD.LED"

	OPEN AR_CLOSED_OLD.NAME$ FOR INPUT AS FILE AR_CLOSED_OLD.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_C, &
		PRIMARY KEY &
		( &
			AR_C.CUSNUM, &
			AR_C.INVNUM &
		)	DUPLICATES, &
		ACCESS READ, ALLOW MODIFY

1000	RESET	#AR_OPEN_OLD.CH%

1020	GET	#AR_OPEN_OLD.CH%

	AR_OPEN::CUSNUM		= AR_O.CUSNUM
	AR_OPEN::INVNUM		= AR_O.INVNUM
	AR_OPEN::TRATYP		= AR_O.TRATYP
	AR_OPEN::TRADAT		= AR_O.TRADAT
	AR_OPEN::SALAMT		= AR_O.SALAMT
	AR_OPEN::DISAMT		= AR_O.DISAMT
	AR_OPEN::OTHCHG		= AR_O.OTHCHG
	AR_OPEN::RECNUM		= AR_O.RECNUM
	AR_OPEN::CHKNUM		= AR_O.CHKNUM
	AR_OPEN::ARACCT		= AR_O.ARACCT
	AR_OPEN::SUBACC		= AR_O.SUBACC
	AR_OPEN::DESCR		= AR_O.DESCR
	AR_OPEN::SALNUM		= ""
	AR_OPEN::BATCH		= AR_O.BATCH
	AR_OPEN::UPDATED	= AR_O.UPDATED
	AR_OPEN::CLOSEDATE	= AR_O.CLOSEDATE

	PUT	#AR_OPEN.CH%

	X% = X% + 1%

	PRINT X%

	GOTO 1020

1030	!

2000	CLOSE AR_OPEN.CH%, AR_OPEN_OLD.CH%
	CALL ASSG_FREECHANNEL(AR_OPEN.CH%)
	CALL ASSG_FREECHANNEL(AR_OPEN_OLD.CH%)

	X% = 0%
	RESET	#AR_CLOSED_OLD.CH%

2020	GET	#AR_CLOSED_OLD.CH%

	AR_CLOSED::CUSNUM	= AR_C.CUSNUM
	AR_CLOSED::INVNUM	= AR_C.INVNUM
	AR_CLOSED::TRATYP	= AR_C.TRATYP
	AR_CLOSED::TRADAT	= AR_C.TRADAT
	AR_CLOSED::SALAMT	= AR_C.SALAMT
	AR_CLOSED::DISAMT	= AR_C.DISAMT
	AR_CLOSED::OTHCHG	= AR_C.OTHCHG
	AR_CLOSED::RECNUM	= AR_C.RECNUM
	AR_CLOSED::CHKNUM	= AR_C.CHKNUM
	AR_CLOSED::ARACCT	= AR_C.ARACCT
	AR_CLOSED::SUBACC	= AR_C.SUBACC
	AR_CLOSED::DESCR	= AR_C.DESCR
	AR_CLOSED::SALNUM	= ""
	AR_CLOSED::BATCH	= AR_C.BATCH
	AR_CLOSED::UPDATED	= AR_C.UPDATED
	AR_CLOSED::CLOSEDATE	= AR_C.CLOSEDATE

	PUT	#AR_CLOSED.CH%

	X% = X% + 1%

	PRINT X%

	GOTO 2020

2030	!


3000	CLOSE AR_CLOSED.CH%, AR_CLOSED_OLD.CH%
	CALL ASSG_FREECHANNEL(AR_CLOSED.CH%)
	CALL ASSG_FREECHANNEL(AR_CLOSED_OLD.CH%)

15000	!
	! Kill old files
	!
	KILL AR_OPEN.DEV$ + "AR_OPEN_OLD.LED" &
		FOR I% = 1% TO 10%

15010	KILL AR_CLOSED.DEV$ + "AR_CLOSED_OLD.LED" &
		FOR I% = 1% TO 10%

 ExitProgram:
	!
	! Reset terminal and exit program
	!

	SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID,0%)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

	%Page

19000	!*******************************************************************
	! Error trARping
	!*******************************************************************

	FILENAME$ = ""
	SELECT ERL
	!
	! Cannot find close file
	!
	CASE 330%
		RESUME 1000 IF ERR = 5%
		FILENAME$ = "AR_CLOSED_OLD"

	CASE 1000%
		FILENAME$ = "AR_OPEN_OLD"

	CASE 1020%
		RESUME 1030 IF ERR = 11%
		FILENAME$ = "AR_OPEN_OLD"

	CASE 2000%
		RESUME 2030 IF ERR = 9%
		FILENAME$ = "AR_CLOSED_OLD"

	CASE 2020%
		RESUME 2030 IF ERR = 11%
		FILENAME$ = "AR_CLOSED_OLD"

	CASE 15000%
		RESUME 15010

	CASE 15010%
		RESUME ExitProgram
	END SELECT

	!
	! Untrapped error
	!
	RESUME HelpError

32767	END
