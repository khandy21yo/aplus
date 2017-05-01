1	%TITLE "This Routine Checks to See If QUEUE Is OK"
	%SBTTL "OUTP_TESTQUE"
	%IDENT "V3.6a Calico"

	FUNCTION INTEGER OUTP_TESTQUE(THEQUE$)

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
	!	This routine checks to see if the QUEUE is in working
	!	condition (returns -1%) or not (returns 0%). The varible
	!	THEQUE$ is the que name as a string such as "SYS$PRINT"
	!	or "SYS$BATCH".
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	THEQUE$
	!		The passed name of the que the programmer wishes to check.
	!
	!	This function returns and integer value to tell if the
	!	que is in working condition.
	!
	!	.table
	!		-1% - Que is in working condition.
	!		 0% - Que is not working.
	!	.endtable
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_TESTQUE/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OUTP_TESTQUE
	!	$ DELETE OUTP_TESTQUE.OBJ;*
	!
	! Author:
	!
	!	01/05/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	07/24/89 - Kevin Handy
	!		Modified to remove unecessary MAP statements.
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
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$, SS$ routines
	!
	!	09/15/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use 0% instead of 0.0
	!--
	%PAGE

	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	! Include the SNDJBC declarations from STARLET.MLB
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
	%INCLUDE "FUNC_INCLUDE:SYS$SNDJBC.TXT"

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "$SSDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! External functions/subroutines need
	!
	EXTERNAL LONG	FUNCTION	SYS$SNDJBCW

	!
	! Declare vars
	!
	DECLARE LONG SYS_STAT, T.CH
	DECLARE LONG CONSTANT JBC$_NORMAL = 262145

	OUTP_TESTQUE = 0%
	JJ$ = READ_SYSJOB
	SYS_STAT = LIB$GET_LUN(T.CH)

	IF (SYS_STAT AND 1%) = 0%
	THEN
 !		CALL ENTR_3MESSAGE(SCOPE, "No work channel for QUEUE test(OUTP_TESTQUE)" + &
 !			NUM1$(SYS_STAT), 0%)
		EXIT FUNCTION
	END IF

100	TEMPFILE$ = "T" + JJ$ + ".TMP"
	OPEN TEMPFILE$ FOR OUTPUT AS FILE T.CH
	CLOSE T.CH
	SYS_STAT = LIB$FREE_LUN(T.CH)

6000	RECORD	IOSB
		LONG	STAT
		LONG	ZERO
	END RECORD IOSB

	RECORD SYS_QUE_RECORD
		STRING SYS_QUE = 10
	END RECORD

	DECLARE SYS_QUE_RECORD SYS_QUE

	RECORD SYS_FILE_RECORD
		STRING SYS_FILE = 10
	END RECORD

	DECLARE SYS_FILE_RECORD SYS_FILE

	RECORD	ITEMLST
		VARIANT
		CASE
			WORD	BUFLEN
			WORD	CODE
			LONG	BUFADR
			LONG	RETLENADR
		CASE
			LONG	ENDLIST
		END VARIANT
	END RECORD ITEMLST

	DECLARE	ITEMLST	QUEA(4)
	DECLARE	IOSB	IOS

	IOS::STAT = 0%
	IOS::ZERO = 0%

	SYS_QUE::SYS_QUE = EDIT$(THEQUE$, 8% + 128%)
	SYS_FILE::SYS_FILE = TEMPFILE$ + ""

	!
	! Queue name
	!
	QUEA(0)::BUFLEN = LEN(SYS_QUE::SYS_QUE)
	QUEA(0)::CODE = SJC$_QUEUE
	QUEA(0)::BUFADR = LOC(SYS_QUE::SYS_QUE)
	QUEA(0)::RETLENADR = 0%

	!
	! File name
	!
	QUEA(1)::BUFLEN = LEN(SYS_FILE::SYS_FILE)
	QUEA(1)::CODE = SJC$_FILE_SPECIFICATION
	QUEA(1)::BUFADR = LOC(SYS_FILE::SYS_FILE)
	QUEA(1)::RETLENADR = 0%

	!
	! Delete the file on completion
	!
	QUEA(2)::BUFLEN = 0%
	QUEA(2)::CODE = SJC$_DELETE_FILE
	QUEA(2)::BUFADR = 0%
	QUEA(2)::RETLENADR = 0%

	!
	! End of list
	!
	QUEA(3)::ENDLIST = 0%

	OUTP_TESTQUE = 0%

	SYS_STAT = SYS$SNDJBCW &
	( &
		, SJC$_ENTER_FILE BY VALUE,, &
		QUEA() BY REF, &
		IOS  BY REF,, &
	)

	SELECT SYS_STAT
	CASE SS$_NORMAL
		SELECT IOS::STAT
		CASE JBC$_NORMAL
			OUTP_TESTQUE = -1%

		CASE ELSE	! Error - not normal
 !			KILL TEMPFILE$
			SMG_STATUS% = LIB$DELETE_FILE(TEMPFILE$ + ";*")
		END SELECT

	CASE ELSE	! Error - not normal
 !		KILL TEMPFILE$
		SMG_STATUS% = LIB$DELETE_FILE(TEMPFILE$ + ";*")
	END SELECT

32767	END FUNCTION
