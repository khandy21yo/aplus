1	%TITLE " Get Queue Form Info Program"
	%SBTTL "READ_QUEFORM"
	%IDENT "V3.6a Calico"

	SUB READ_QUEFORM(GQUE$, DDATA$(,), STATUS%)

	!
	! COPYRIGHT (C) 1988 BY
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This subroutine returns a 2 dimensional array DDATA$(,)
	!	containing the following information about the given queue
	!	GQUE$.
	!	.b
	!	DDATA$(0%,0%) = THE NUMBER OF SETS
	!	.table 3,25
	!	.te
	!	ONE SET CONTAINS
	!	.te
	!	DDATA$(X%,0%) = JUNK JUNK JUNK JUNK
	!	.te
	!	DDATA$(X%, 1%) = FORM NAME
	!	.te
	!	DDATA$(X%,2%) = FORM NUMBER
	!	.te
	!	DDATA$(X%,3%) = FORM STOCK
	!	.te
	!	DDATA$(X%,4%) = FORM DESCRIPTION
	!	.te
	!	DDATA$(X%,5%) = FORM LENGTH
	!	.te
	!	DDATA$(X%,6%) = FORM WIDTH
	!	.te
	!	DDATA$(X%,7%) = FORM TOP MARGIN
	!	.te
	!	DDATA$(X%,8%) = FORM BOTTOM MARGIN
	!	.te
	!	DDATA$(X%,9%) = FORM LEFT MARGIN
	!	.te
	!	DDATA$(X%,10%) = FORM RIGHT MARGIN
	!	.te
	!	DDATA$(X%,11%) = FORM SHEET FEED
	!	.te
	!	DDATA$(X%,12%) = FORM TRUNCATE
	!	.te
	!	DDATA$(X%,13%) = FORM WRAP
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	GQUE$
	!		The passed que the user wishes to use.
	!
	!	DDATA$(,)
	!		The two dimensional array returned that contains
	!		the necessary que information.
	!
	!	This subroutine gets the queue form information of the program.
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:READ_QUEFORM/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP READ_QUEFORM
	!	$ DELETE READ_QUEFORM.OBJ;*
	!
	! Author:
	!
	!	03/14/88 - B. Craig Larsen
	!
	! Modification history:
	!
	!	07/06/89 - Kevin Handy
	!		Modified to remove MAP's (A) thru (M).
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/04/96 - Kevin Handy
	!		Reformat source code.
	!
	!	10/17/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	!
	! Include files
	!
	! Include the GETQUI declarations from STARLET.MLB
	%INCLUDE "FUNC_INCLUDE:CMC_GETQUI.TXT"

	!
	! External functions/subroutines need
	!
	EXTERNAL LONG    FUNCTION	SYS$GETQUIW

	DECLARE LONG CONSTANT JBC$_NOMOREFORM = 295322
	DECLARE LONG CONSTANT JBC$_NOSUCHFORM = 295258

	!
	! Handle file name to spool
	!
6000	RECORD	IOSB
		LONG	STAT
		LONG	ZERO
	END RECORD IOSB

	RECORD STRING31_RECORD
		STRING TEXT = 31
	END RECORD

	RECORD STRING255_RECORD
		STRING TEXT = 255
	END RECORD

	RECORD LONG_RECORD
		LONG LVALUE
	END RECORD

	DECLARE STRING31_RECORD  SYS.SER_NAME
	DECLARE STRING31_RECORD  SYS.NAME
	DECLARE STRING255_RECORD SYS.DESC
	DECLARE LONG_RECORD      SYS.LENGTH
	DECLARE LONG_RECORD      SYS.BOT
	DECLARE LONG_RECORD      SYS.TOP
	DECLARE LONG_RECORD      SYS.LEFT
	DECLARE LONG_RECORD      SYS.RIGHT
	DECLARE LONG_RECORD      SYS.NUMBER
	DECLARE STRING255_RECORD SYS.STOCK
	DECLARE LONG_RECORD      SYS.FORM_FLAGS
	DECLARE LONG_RECORD      SYS.WIDTH


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

	DECLARE	ITEMLST	THE_A(15%)
	DECLARE	IOSB IOS
	DECLARE LONG SYS.COPIES

	IOS::STAT = 0%
	IOS::ZERO = 0%

	SYS.SER_NAME::TEXT = "*"

	!
	! Queue name
	!
	THE_A(0%)::BUFLEN = LEN(SYS.SER_NAME::TEXT)
	THE_A(0%)::CODE = QUI$_SEARCH_NAME
	THE_A(0%)::BUFADR = LOC(SYS.SER_NAME::TEXT)
	THE_A(0%)::RETLENADR = 0.

	!
	! Form Description
	!
	THE_A(1%)::BUFLEN = LEN(SYS.DESC::TEXT)
	THE_A(1%)::CODE = QUI$_FORM_DESCRIPTION
	THE_A(1%)::BUFADR = LOC(SYS.DESC::TEXT)
	THE_A(1%)::RETLENADR = 0.

	!
	! Form sheet feed, truncate, wrap
	!
	THE_A(2%)::BUFLEN = 4%
	THE_A(2%)::CODE = QUI$_FORM_FLAGS
	THE_A(2%)::BUFADR = LOC(SYS.FORM_FLAGS::LVALUE)
	THE_A(2%)::RETLENADR = 0.

	!
	! Form length
	!
	THE_A(3%)::BUFLEN = 4%
	THE_A(3%)::CODE = QUI$_FORM_LENGTH
	THE_A(3%)::BUFADR = LOC(SYS.LENGTH::LVALUE)
	THE_A(3%)::RETLENADR = 0.

	!
	! Form bottom margin
	!
	THE_A(4%)::BUFLEN = 4%
	THE_A(4%)::CODE = QUI$_FORM_MARGIN_BOTTOM
	THE_A(4%)::BUFADR = LOC(SYS.BOT::LVALUE)
	THE_A(4%)::RETLENADR = 0.

	!
	! Form top margin
	!
	THE_A(5%)::BUFLEN = 4%
	THE_A(5%)::CODE = QUI$_FORM_MARGIN_TOP
	THE_A(5%)::BUFADR = LOC(SYS.TOP::LVALUE)
	THE_A(5%)::RETLENADR = 0.

	!
	! Form left margin
	!
	THE_A(6%)::BUFLEN = 4%
	THE_A(6%)::CODE = QUI$_FORM_MARGIN_LEFT
	THE_A(6%)::BUFADR = LOC(SYS.LEFT::LVALUE)
	THE_A(6%)::RETLENADR = 0.

	!
	! Form right margin
	!
	THE_A(7%)::BUFLEN = 4%
	THE_A(7%)::CODE = QUI$_FORM_MARGIN_RIGHT
	THE_A(7%)::BUFADR = LOC(SYS.RIGHT::LVALUE)
	THE_A(7%)::RETLENADR = 0.

	!
	! Form name
	!
	THE_A(8%)::BUFLEN = LEN(SYS.NAME::TEXT)
	THE_A(8%)::CODE = QUI$_FORM_NAME
	THE_A(8%)::BUFADR = LOC(SYS.NAME::TEXT)
	THE_A(8%)::RETLENADR = 0.

	!
	! Form number
	!
	THE_A(9%)::BUFLEN = 4%
	THE_A(9%)::CODE = QUI$_FORM_NUMBER
	THE_A(9%)::BUFADR = LOC(SYS.NUMBER::LVALUE)
	THE_A(9%)::RETLENADR = 0.

	!
	! Form stock
	!
	THE_A(10%)::BUFLEN = LEN(SYS.STOCK::TEXT)
	THE_A(10%)::CODE = QUI$_FORM_STOCK
	THE_A(10%)::BUFADR = LOC(SYS.STOCK::TEXT)
	THE_A(10%)::RETLENADR = 0.

	!
	! Form width
	!
	THE_A(11%)::BUFLEN = 4%
	THE_A(11%)::CODE = QUI$_FORM_WIDTH
	THE_A(11%)::BUFADR = LOC(SYS.WIDTH::LVALUE)
	THE_A(11%)::RETLENADR = 0.

	!
	! End of list
	!
	THE_A(12%)::ENDLIST = 0.

	Z% = 0%

7000	SYS_STAT% = SYS$GETQUIW(,QUI$_DISPLAY_FORM BY VALUE,, THE_A() BY REF, &
		IOS  BY REF,,)

	SELECT IOS::STAT
	CASE JBC$_NOMOREFORM, JBC$_NOSUCHFORM

		GOTO ExitProgram

 !	CASE ELSE
 !
 !		CALL ENTR_3MESSAGE(SCOPE, "ERROR FROM READ_QUEFORM = " + &
 !			NUM1$(SYS_STAT%) + &
 !			" " + NUM1$(IOS::STAT), 0%) IF (SYS_STAT% AND 1%) <> 1% &
 !			OR ((IOS::STAT AND 1%) <> 1% AND (IOS::STAT > 1%))
 !
	END SELECT

	Z% = Z% + 1%

	DDATA$(Z%, 1%) = EDIT$(SYS.NAME::TEXT, 4% + 128%)
	DDATA$(Z%, 2%) = FORMAT$(SYS.NUMBER::LVALUE, "<0>#")
	DDATA$(Z%, 3%) = EDIT$(SYS.STOCK::TEXT, 4% + 128%)
	DDATA$(Z%, 4%) = EDIT$(SYS.DESC::TEXT, 4% + 128%)
	DDATA$(Z%, 5%) = FORMAT$(SYS.LENGTH::LVALUE, "<0>#")
	DDATA$(Z%, 6%) = FORMAT$(SYS.WIDTH::LVALUE, "<0>#")
	DDATA$(Z%, 7%) = FORMAT$(SYS.TOP::LVALUE, "<0>#")
	DDATA$(Z%, 8%) = FORMAT$(SYS.BOT::LVALUE, "<0>#")
	DDATA$(Z%, 9%) = FORMAT$(SYS.LEFT::LVALUE, "<0>#")
	DDATA$(Z%, 10%) = FORMAT$(SYS.RIGHT::LVALUE, "<0>#")

	IF (SYS.FORM_FLAGS::LVALUE AND QUI$M_FORM_SHEET_FEED) = QUI$M_FORM_SHEET_FEED
	THEN
		DDATA$(Z%, 11%) = "YES"
	ELSE
		DDATA$(Z%, 11%) = "NO"
	END IF

	IF (SYS.FORM_FLAGS::LVALUE AND QUI$M_FORM_TRUNCATE) = QUI$M_FORM_TRUNCATE
	THEN
		DDATA$(Z%, 12%) = "YES"
	ELSE
		DDATA$(Z%, 12%) = "NO"
	END IF

	IF (SYS.FORM_FLAGS::LVALUE AND QUI$M_FORM_WRAP) = QUI$M_FORM_WRAP
	THEN
		DDATA$(Z%, 13%) = "YES"
	ELSE
		DDATA$(Z%, 13%) = "NO"
	END IF

	GOTO 7000

 ExitProgram:
	!*********************************************************************
	! Normal exit
	!*********************************************************************
	DDATA$(0%, 0%) = NUM1$(Z%)

32767	END SUB
