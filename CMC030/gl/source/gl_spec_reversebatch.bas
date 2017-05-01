1	%TITLE "Special Reversal Function"
	%SBTTL "GL_SPEC_REVERSEBATCH"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
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
	!	This program is used to reverse the amounts in one
	!	entire post to the general ledger.
	!	It manually digs into the period file and changes
	!	the amounts there. Requires a RESYNC afterwards.
	!	NOTE: Does not use any CMC functions so that the
	!	size is as small as possible so KERMIT will not take
	!	too long.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_SPEC_REVERSEBATCH/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_SPEC_REVERSEBATCH, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_SPEC_REVERSEBATCH.OBJ;*
	!
	! Author:
	!
	!	01/10/89 - Kevin Handy
	!
	! Modification history:
	!
	!	03/30/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

100	LINPUT "GL Period to reverse (YYYY_PP)"; YYYY_PP$

	IF INSTR(1%, YYYY_PP$, "_") = 0%
	THEN
		PRINT "Missing underscore!"
		GOTO 32767
	END IF

	LINPUT "Batch number to reverse (000000)"; BATCH_NO$
	BATCH_NO$ = BATCH_NO$ + SPACE$(6% - LEN(BATCH_NO$))

110	!======================================================================
	! GL_YYYY_PP file (open read/write)
	!======================================================================

	GL_YYYY_PP.CH% = 10%
	GL_YYYY_PP.DEV$ = ""

	GL_YYYY_PP.NAME$ = GL_YYYY_PP.DEV$ + "GL_" + YYYY_PP$ + ".LED"

	WHEN ERROR IN
		OPEN GL_YYYY_PP.NAME$ FOR INPUT AS FILE GL_YYYY_PP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP GL_YYYY_PP, &
			PRIMARY KEY &
			( &
				GL_YYYY_PP::ACCT, &
				GL_YYYY_PP::TRANDAT &
			)	DUPLICATES, &
			ALTERNATE KEY &
			( &
				GL_YYYY_PP::SUBACC, &
				GL_YYYY_PP::OPERATION, &
				GL_YYYY_PP::ACCT &
			)	DUPLICATES CHANGES, &
			ALTERNATE KEY &
			( &
				GL_YYYY_PP::XREFNO, &
				GL_YYYY_PP::ACCT &
			)	DUPLICATES CHANGES, &
			ALTERNATE KEY &
			( &
				GL_YYYY_PP::CKNO, &
				GL_YYYY_PP::ACCT &
			)	DUPLICATES CHANGES, &
			ALTERNATE KEY &
				GL_YYYY_PP::BTHNUM &
				DUPLICATES CHANGES, &
			ACCESS MODIFY, ALLOW NONE
	USE
		PRINT "Unable to open period file"
		CONTINUE 32767
	END WHEN

200	!
	! Search for first item with that batch number
	!
	WHEN ERROR IN
		FIND #GL_YYYY_PP.CH%, KEY #4% EQ BATCH_NO$
	USE
		PRINT "No such batch!"
		CONTINUE 32767
	END WHEN

250	!
	! Process one record
	!
	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%
	USE
		CONTINUE 300 IF ERR = 11%
		CONTINUE HelpError
	END WHEN

260	IF GL_YYYY_PP::BTHNUM == BATCH_NO$
	THEN
		GL_YYYY_PP::AMOUNT = -GL_YYYY_PP::AMOUNT

		UPDATE #GL_YYYY_PP.CH%

		PRINT ".";

		GOTO 250
	END IF

300	!
	! Done
	!
	CLOSE #GL_YYYY_PP.CH%

	GOTO 32767

 HelpError:
	PRINT "ERROR"; ERR; " AT LINE"; ERL;

32767	END
