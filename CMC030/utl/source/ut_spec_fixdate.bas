1	%TITLE "Fix Dates in all Systems"
	%SBTTL "UT_SPEC_FIXDATE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2003 BY
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use or
	! reliability of its software on equipment which is not supported
	! by Software Solutions, Inc.
	!
	!++
	!
	! Abstract:HELP
	!	.p
	!	Special program to search for places where the wrong
	!	year was used in a posting, and replace them with the
	!	correct one.
	!	.p
	!	This is not a GUI program, it wants to be run from the
	!	command line.
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UT_SPEC_FIXDATE/LINE
	!	$ LINK/EXECUTABLE=UTL_EXE: UT_SPEC_FIXDATE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UT_SPEC_FIXDATE.OBJ;*
	!
	! Author:
	!
	!	01/10/2003 - Kevin Handy
	!
	! Modification history:
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP	(GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP (IC_TRANSACTION)	IC_TRANSACTION_CDD	IC_TRANSACTION

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP (AP_OPEN)		AP_OPEN_CDD		AP_OPEN

	!
	! Dimension statements
	!
	DIM FILE_LIST$(1000%)

	%PAGE

1000	!*******************************************************************
	! Dialogs
	!*******************************************************************

	LINPUT "Batch Number (XXXXXX): "; BATCHNO$
	IF LEN(BATCHNO$) <> 6%
	THEN
		PRINT "Batch number must be six characters long!"
		GOTO ExitProgram
	END IF

	LINPUT "Bad Year (YYYY):       "; BAD_YEAR$
	IF LEN(BAD_YEAR$) <> 4%
	THEN
		PRINT "Bad year must be four characters long!"
		GOTO ExitProgram
	END IF

	LINPUT "Good Year (YYYY):      "; GOOD_YEAR$
	IF LEN(GOOD_YEAR$) <> 4%
	THEN
		PRINT "Good year must be four characters long!"
		GOTO ExitProgram
	END IF

5000	!*******************************************************************
	! Handle AR Open file
	!*******************************************************************

	PRINT "Trying AR_OPEN"

	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.MOD"
	USE
		PRINT "  Can't open AR_OPEN"
		CONTINUE 5900
	END WHEN

5010	WHEN ERROR IN
		FIND #AR_OPEN.CH%, KEY #1% EQ BATCHNO$
	USE
		PRINT "  Batch does not exist"
		CONTINUE 5900
	END WHEN

5100	WHEN ERROR IN
		GET #AR_OPEN.CH%
	USE
		PRINT "!"
		CONTINUE 5900
	END WHEN

	IF AR_OPEN::BATCH = BATCHNO$
	THEN
		PRINT IF CCPOS(0%) >= 50%
		DOFLAG% = 0%

		IF LEFT(AR_OPEN::TRADAT, 6%) = BAD_YEAR$ + "12"
		THEN
			AR_OPEN::TRADAT = GOOD_YEAR$ + &
				RIGHT(AR_OPEN::TRADAT, 5%)
			DOFLAG% = 1%
		END IF

		IF LEFT(AR_OPEN::DUEDATE, 6%) = BAD_YEAR$ + "12"
		THEN
			AR_OPEN::DUEDATE = GOOD_YEAR$ + &
				RIGHT(AR_OPEN::DUEDATE, 5%)
			DOFLAG% = 1%
		END IF

		IF LEFT(AR_OPEN::DISCOUNTDATE, 6%) = BAD_YEAR$ + "12"
		THEN
			AR_OPEN::DISCOUNTDATE = GOOD_YEAR$ + &
				RIGHT(AR_OPEN::DISCOUNTDATE, 5%)
			DOFLAG% = 1%
		END IF

		IF DOFLAG%
		THEN
			UPDATE #AR_OPEN.CH%
			PRINT "+";
		ELSE
			PRINT "-";
		END IF

		GOTO 5100
	END IF

5900	CLOSE #AR_OPEN.CH%

6000	!*******************************************************************
	! General Ledger Files
	!*******************************************************************

	CALL READ_DEVICE("GL_YYYY_PP", GL_YYYY_PP.DEV$, STAT%)

6010	!
	! Get information needed to open GL Period file (GL_YYYY_PP)
	!
	CALL FIND_FILE(GL_YYYY_PP.DEV$ + "GL_*.LED", FILE_LIST$(), &
		16%, "", "")

	FILE_LIST% = VAL%(FILE_LIST$(0%))

6100	FOR LOOP% = 1% TO FILE_LIST%

		YYYY_PP$ = MID(FILE_LIST$(LOOP%), 4%, 7%)

		PRINT "Trying GL_"; YYYY_PP$

		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.MOD"
		USE
			PRINT "  Can't open GL_"; YYYY_PP$
			CONTINUE 6800
		END WHEN

6110		WHEN ERROR IN
			FIND #GL_YYYY_PP.CH%, KEY #4% EQ BATCHNO$
		USE
			PRINT "  Batch does not exist"
			CONTINUE 6800
		END WHEN

6200		WHEN ERROR IN
			GET #GL_YYYY_PP.CH%
		USE
			PRINT "!"
			CONTINUE 6800
		END WHEN

		IF GL_YYYY_PP::BTHNUM = BATCHNO$
		THEN
			PRINT IF CCPOS(0%) >= 50%
			DOFLAG% = 0%

			IF LEFT(GL_YYYY_PP::TRANDAT, 6%) = BAD_YEAR$ + "12"
			THEN
				GL_YYYY_PP::TRANDAT = GOOD_YEAR$ + &
					RIGHT(GL_YYYY_PP::TRANDAT, 5%)
				DOFLAG% = 1%
			END IF

			IF DOFLAG%
			THEN
				DELETE #GL_YYYY_PP.CH%
				PUT #GL_YYYY_PP.CH%
				PRINT "+";
				GOTO 6110
			END IF

			GOTO 6200
		END IF

6800		PRINT IF CCPOS(0%) >= 1%
	NEXT LOOP%

6900	!

7000	!*******************************************************************
	! Inventory Ledger Files
	!*******************************************************************

	CALL READ_DEVICE("IC_TRANSACTION", IC_TRANSACTION.DEV$, STAT%)

7010	!
	! Get information needed to open GL Period file (GL_YYYY_PP)
	!
	CALL FIND_FILE(IC_TRANSACTION.DEV$ + "IC_TRANSACTION_*.LED", &
		FILE_LIST$(), 16%, "", "")

	FILE_LIST% = VAL%(FILE_LIST$(0%))

7100	FOR LOOP% = 1% TO FILE_LIST%

		YYYYPP$ = MID(FILE_LIST$(LOOP%), 16%, 6%)

		PRINT "Trying IC_"; YYYYPP$

		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.MOD"
		USE
			PRINT "  Can't open IC_"; YYYY_PP$
			CONTINUE 7800
		END WHEN

7110		WHEN ERROR IN
			FIND #IC_TRANSACTION.CH%, KEY #1% EQ BATCHNO$
		USE
			PRINT "  Batch does not exist"
			CONTINUE 7800
		END WHEN

7200		WHEN ERROR IN
			GET #IC_TRANSACTION.CH%
		USE
			PRINT "!"
			CONTINUE 7800
		END WHEN

		IF IC_TRANSACTION::BATCH = BATCHNO$
		THEN
			PRINT IF CCPOS(0%) >= 50%
			DOFLAG% = 0%

			IF LEFT(IC_TRANSACTION::TRANS_DATE, 6%) = BAD_YEAR$ + "12"
			THEN
				IC_TRANSACTION::TRANS_DATE = GOOD_YEAR$ + &
					RIGHT(IC_TRANSACTION::TRANS_DATE, 5%)
				DOFLAG% = 1%
			END IF

			IF DOFLAG%
			THEN
				DELETE #IC_TRANSACTION.CH%
				PUT #IC_TRANSACTION.CH%
				PRINT "+";
				GOTO 7110
			END IF

			GOTO 7200
		END IF

7800		PRINT IF CCPOS(0%) >= 1%
	NEXT LOOP%

7900	!

8000	!*******************************************************************
	! Handle AP Open file
	!*******************************************************************

	PRINT "Trying AP_OPEN"

	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.MOD"
	USE
		PRINT "  Can't open AP_OPEN"
		CONTINUE 8900
	END WHEN

8010	WHEN ERROR IN
		FIND #AP_OPEN.CH%, KEY #2% EQ BATCHNO$
	USE
		PRINT "  Batch does not exist"
		CONTINUE 8900
	END WHEN

8100	WHEN ERROR IN
		GET #AP_OPEN.CH%
	USE
		PRINT "!"
		CONTINUE 8900
	END WHEN

	IF AP_OPEN::BATCH = BATCHNO$
	THEN
		PRINT IF CCPOS(0%) >= 50%
		DOFLAG% = 0%

		IF LEFT(AP_OPEN::INVDAT, 6%) = BAD_YEAR$ + "12"
		THEN
			AP_OPEN::INVDAT = GOOD_YEAR$ + &
				RIGHT(AP_OPEN::INVDAT, 5%)
			DOFLAG% = 1%
		END IF

		IF LEFT(AP_OPEN::TRANKEY_DATE, 6%) = BAD_YEAR$ + "12"
		THEN
			AP_OPEN::TRANKEY_DATE = GOOD_YEAR$ + &
				RIGHT(AP_OPEN::TRANKEY_DATE, 5%)
			DOFLAG% = 1%
		END IF

		IF LEFT(AP_OPEN::DISCDAT, 6%) = BAD_YEAR$ + "12"
		THEN
			AP_OPEN::DISCDAT = GOOD_YEAR$ + &
				RIGHT(AP_OPEN::DISCDAT, 5%)
			DOFLAG% = 1%
		END IF

		IF LEFT(AP_OPEN::DUEDAT, 6%) = BAD_YEAR$ + "12"
		THEN
			AP_OPEN::DUEDAT = GOOD_YEAR$ + &
				RIGHT(AP_OPEN::DUEDAT, 5%)
			DOFLAG% = 1%
		END IF

		IF DOFLAG%
		THEN
			UPDATE #AP_OPEN.CH%
			PRINT "+";
		ELSE
			PRINT "-";
		END IF

		GOTO 8100
	END IF

8900	CLOSE #AP_OPEN.CH%

9000	!*******************************************************************

17999	GOTO ExitProgram

18000	!*******************************************************************
	! Exit area
	!*******************************************************************
 ExitProgram:

	GOTO 32767

19000	!*******************************************************************
	! E R R O R   T R A P P I N G   C O D E
	!*******************************************************************

	!
	! Untrapped error
	!
 !	PRINT "Untrapped error: ("; &
 !		NUM1$(ERR); ")  "; ERT$(ERR); " at line "; ERL
 !
 !	RESUME 32767

32767	END
