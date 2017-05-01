1	%TITLE "Read Structure from File and Parse"
	%SBTTL "OUTP_READSTRUCTURE"
	%IDENT "V3.6a Calico"

	SUB OUTP_READSTRUCTURE(UTL_REPORTX_CDD UTL_REPORTX, &
		WORD INPUT.CH, STRING INLINE)

	!
	!	COPYRIGHT (C) 1986.1987,1988 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
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
	! Parameters:
	!
	!	UTL_REPORTX
	!		The passed definition of the current file.
	!
	!	INPUT.CH
	!		The passed input channel for the file.
	!
	!	INLINE
	!		Passed variable to read in one line of the source file.
	!
	!
	!	Returned value
	!		Reads a structure from the file and parses it.
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_READSTRUCTURE/LINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP OUTP_READSTRUCTURE
	!	$ DELETE OUTP_READSTRUCTURE.OBJ;*
	!
	! AUTHOR:
	!
	!	03/18/87 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!	11/12/87 - Kevin Handy
	!		Modified to pass program name
	!		(UTL_REPORTX::PRONAM) for help text stuff.
	!
	!	05/17/88 - Kevin Handy
	!		Moved Spooling function internal to programs.
	!
	!	09/06/88 - Kevin Handy
	!		Added code for ZZ print control string.
	!
	!	01/27/89 - Frank Starman
	!		Fill REPORTX field in case "RN" and "PG"
	!
	!	04/06/89 - Kevin Handy
	!		Modified handlilng of escape sequences
	!
	!	07/03/89 - Kevin Handy
	!		Removed maps, made work in sharable library.
	!
	!	03/13/95 - Kevin Handy
	!		Added code for three new fields:
	!		AFTERTIME, BACKGROUND, OFFSET.
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	10/13/97 - Kevin Handy
	!		Load PT into PRINTTYPE variable.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/09/99 - Kevin Handy
	!		Use BASIC$STARLET for SS$ and DVI$
	!
	!	06/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	%INCLUDE "$SSDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$DVIDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION SYS$FILESCAN
	DECLARE LONG CONSTANT FSCN$_NAME = 6

	!
	! Declare vars
	!
	DECLARE LONG CONSTANT DC$_TERMINAL = 66

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

	!
	! Assume no errors
	!
	UTL_REPORTX::STAT = 0%
	RN.FLAG% = 0%

	!
	! Assume no auto scroll
	!
	UTL_REPORTX::AUTOSCROLL = 0%

	!
	! Assume not chaining out
	!
	UTL_REPORTX::OPTDEF(0%) = ""

	!
	! Assume no control strings
	!
	UTL_REPORTX::PRINTINIT = ""

	!
	! Assume no program name
	!
	UTL_REPORTX::PRONAM = ""

1000	!
	! Read in one line of source file
	!
	WHEN ERROR IN
		LINPUT #INPUT.CH, INLINE
	USE
		IF ERR = 11%
		THEN
			INLINE = ""
			CONTINUE 1100
		END IF
	END WHEN

	LEFTT$ = LEFT(INLINE, 2%)
	RIGHTT$ = RIGHT(INLINE, 4%)

	!
	! Mung input
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
		TEMP_LONG% = TEMP_LONG% - TEMP1_LONG% + 1%

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
		RN.FLAG% = -1%
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
		RN.FLAG% = -1%
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
		GOTO 1100 IF RN.FLAG%
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

	GOTO 1000

	%PAGE

1100	!
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
	UTL_REPORTX::PAGELEN  = 66%  IF UTL_REPORTX::PAGELEN  = 0%

	END SUB
