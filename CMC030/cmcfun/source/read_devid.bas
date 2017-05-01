1	%TITLE "This Routine Checks Device for Response"
	%SBTTL "READ_DEVID"
	%IDENT "V3.6a Calico"

	FUNCTION STRING READ_DEVID(DEVIC$, ESEQ$)

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
	! Computer Management Center.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This routine interrogates the given device. DEVIC$ is the
	!	device name and ESEQ$ is the string needed by that device
	!	to ask its status. This routine returns:
	!	.table
	!	Escape('27'C) for success on DEC equipment
	!	Non-Escape for failure(check non DEC equip)
	!	It maybe ERROR
	!	NORESPONSE
	!	INUSE
	!	.endtable
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	ESEQ$
	!		The passed string needed to ask for the status.
	!
	!	DEVIC$
	!		The passed device name to be interrogated.
	!
	! Example:
	!
	! Index:
	!
	!	.x Read>Device ID
	!	.x Device ID>Read
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:READ_DEVID/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP READ_DEVID
	!	$ DELETE READ_DEVID.OBJ;*
	!
	! AUTHOR:
	!
	!	12/22/86 - B. Craig Larsen
	!
	! MODIFICATION HISTORY:
	!
	!	07/06/89 - Kevin Handy
	!		Modified to remove various MAPS (CC), (TEMP),
	!		(IOSB), and (TMASK)
	!
	!	07/16/91 - Kevin Handy
	!		Unwound external definitions.
	!
	!	08/14/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/09/99 - Kevin Handy
	!		Use BASIC$STARLET for SS$ references
	!--

	%PAGE

	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "$SSDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$IODEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! External declarations
	!
	EXTERNAL	LONG	FUNCTION	SYS$ASSIGN
	EXTERNAL	LONG	FUNCTION	SYS$QIOW
	EXTERNAL	LONG	FUNCTION	SYS$DASSGN

	DECLARE		LONG	SYS_STAT, OUT_TIME, LEN_DS, XLEN1

	!
	! Set up MAPs and initialize them
	!
	RECORD IOSB_RECORD
		VARIANT
		CASE
			LONG	IO_SB(1)
		CASE
			WORD	IO_SB_W(3)
		END VARIANT
	END RECORD

	DECLARE IOSB_RECORD IOSB


	RECORD TEMP_RECORD
		BYTE	TEMP_B(15)
	END RECORD

	DECLARE TEMP_RECORD TEMP


	RECORD TMASK_RECORD
		VARIANT
		CASE
			LONG	TERM_MASK(1)
		CASE
			WORD	TERM_MASK_W(3)
		END VARIANT
	END RECORD

	DECLARE TMASK_RECORD TMASK


	RECORD CC_RECORD
		VARIANT
		CASE
			WORD	IO_CB(4)
		CASE
			LONG	CARC
		END VARIANT
	END RECORD

	DECLARE CC_RECORD CC

	%PAGE

	XLEN1 = 200
	BUFFER$ = SPACE$(XLEN1 + 10%)
	THE.CH% = 10%
	OUT_TIME = 2
	I_DEVICE$ = EDIT$(DEVIC$, 8% + 128%)
	DEV_STR$ = ""
	I% = 0%

	STRNG$ = TRM$(ESEQ$)
	SLASH$ = "\"
	SLASH$ = "/" IF INSTR(1%, STRNG$, "/")

 Loop1:
	!
	! Loop through string
	!
	I1% = INSTR(I% + 1%, STRNG$, SLASH$)

	!
	! If there is a slash, then handle it
	!
	IF I1%
	THEN
		DEV_STR$ = DEV_STR$ + SEG$(STRNG$, I% + 1%, I1% - 1%) + &
			CHR$(VAL%(MID(STRNG$, I1% + 1%, 3%)))
		I% = I1% + 3%
		GOTO Loop1
	END IF

	!
	! No slash, so must be the end of the string. Dump out rest and exit.
	!
	DEV_STR$ = DEV_STR$ + RIGHT(STRNG$, I% + 1%)
	LEN_DS = LEN(DEV_STR$)
	R% = 0%

	%PAGE

10	!
	! Assign the device
	!
	SYS_STAT = SYS$ASSIGN(I_DEVICE$ BY DESC, THE.CH% BY REF,,,)

	SELECT SYS_STAT
	CASE SS$_CONNECFAIL, SS$_DEVOFFLINE, SS$_SHUT, SS$_UNREACHABLE
		READ_DEVID = "NORESPONSE " + NUM1$(SYS_STAT)
		GOTO ExitProgram

	CASE SS$_DEVACTIVE, SS$_DEVALLOC, SS$_FILALRACC
		READ_DEVID = "INUSE      " + NUM1$(SYS_STAT)
		GOTO ExitProgram

	CASE SS$_NORMAL, SS$_REMOTE

	CASE SS$_REMRSRC
		R% = R% + 1%
		GOTO 10 IF R% < 5%
		READ_DEVID = "ERROR      " + NUM1$(SYS_STAT)
		GOTO ExitProgram

	CASE ELSE
		READ_DEVID = "ERROR      " + NUM1$(SYS_STAT)
		GOTO ExitProgram
	END SELECT

	!
	! Initialize the parameters for the write
	!
	CC::IO_CB(0%) = 0%
	CC::IO_CB(1%) = 0%
	CC::IO_CB(2%) = 141%
	CC::IO_CB(3%) = 0%

	IOSB::IO_SB(I%) = 0% FOR I% = 0% TO 1%

	SYS_STAT = SYS$QIOW &
	( &
		, &
		THE.CH% BY VALUE, &
		IO$_WRITEVBLK BY VALUE, &
		IOSB::IO_SB() BY REF,,, &
		DEV_STR$ BY REF, &
		LEN_DS BY VALUE,, &
		CC::CARC BY VALUE,, &
	)

	SELECT SYS_STAT
	CASE SS$_NORMAL

	CASE ELSE
		READ_DEVID = "ERROR      " + NUM1$(SYS_STAT)
		GOTO ExitProgram
	END SELECT

	!
	! Initialize the parameters for the read
	!
	IOSB::IO_SB(I%) = 0%	FOR I% = 0% TO 1%

	TEMP::TEMP_B(I%) = -1% FOR I% = 0% TO 3%
	TEMP::TEMP_B(I%) =  0% FOR I% = 4% TO 14%
	TEMP::TEMP_B(15%) = -128%

	TMASK::TERM_MASK(1) = LOC(TEMP::TEMP_B(0%))
	TMASK::TERM_MASK_W(1%) = 0%
	TMASK::TERM_MASK_W(0%) = 16%

	FUNC_MOD%   = (IO$_READLBLK OR IO$M_TIMED OR IO$M_ESCAPE OR &
		IO$M_PURGE OR IO$M_NOECHO OR IO$M_NOFILTR OR IO$M_TRMNOECHO)

	!
	! Get input until terminater
	!
	SYS_STAT = SYS$QIOW &
	( &
		, &
		THE.CH% BY VALUE, &
		FUNC_MOD% BY VALUE, &
		IOSB::IO_SB() BY REF,,, &
		BUFFER$ BY REF, &
		XLEN1 BY VALUE, &
		OUT_TIME BY VALUE, &
		TMASK::TERM_MASK() BY REF,, &
	)

	SELECT SYS_STAT
	CASE SS$_NORMAL

	CASE ELSE
		READ_DEVID = "ERROR      " + NUM1$(SYS_STAT)
		GOTO ExitProgram
	END SELECT

	READ_DEVID = LEFT(BUFFER$, IOSB::IO_SB_W(1%) + IOSB::IO_SB_W(3%))

 ExitProgram:
	!
	! Deassign the device
	!
	SYS_STAT = SYS$DASSGN(THE.CH% BY VALUE)

	END FUNCTION
