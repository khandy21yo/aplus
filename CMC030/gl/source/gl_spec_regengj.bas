1	%TITLE "Create Folder For Beginning Balances"
	%SBTTL "GL_SPEC_REGENGJ"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2001 BY
	!
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
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Generates a journal based on a gl period/batch number.
	!	.lm -5
	!
	! Index:
	!	.x General Ledger>Generate
	!	.x Generate>General Ledger
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_SPEC_REGENGJ/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_SPEC_REGENGJ, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_SPEC_REGENGJ.OBJ;*
	!
	! Author:
	!
	!	04/18/2001 - Kevin Handy
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

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)		GL_YYYY_PP_CDD	GL_YYYY_PP

	%INCLUDE "SOURCE:[GL.OPEN]GL_GJ_LINE.HB"
	MAP (GL_GJ_LINE)	GL_GJ_LINE_CDD	GL_GJ_LINE


100	!
	! Ask questions
	!
	PRINT "Period to read (YYYY_PP) ";
	LINPUT YYYY_PP$

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"

110	PRINT "Batch number to load (xxxxxx) ";
	LINPUT BATCH$

	FIND #GL_YYYY_PP.CH%, KEY #4% EQ BATCH$

120	PRINT "Journal Number (xxxxxx) ";
	LINPUT JOURNAL$

	JRL_TYPE$ = "1"
	%INCLUDE "SOURCE:[GL.OPEN]GL_GJ_LINE.CRE"

190	ITEM% = 0%


1000	!

	GET #GL_YYYY_PP.CH%

	IF GL_YYYY_PP::BTHNUM = BATCH$
	THEN
		ITEM% = ITEM% + 1%

		GL_GJ_LINE::JOURNAL	= JOURNAL$
		GL_GJ_LINE::ITEMNUM	= FORMAT$(ITEM%, "<0>###")
		GL_GJ_LINE::ACCT	= GL_YYYY_PP::ACCT
		GL_GJ_LINE::SOURCE	= GL_YYYY_PP::SOURCE
		GL_GJ_LINE::DESCR	= GL_YYYY_PP::DESCR
		GL_GJ_LINE::TRANDAT	= GL_YYYY_PP::TRANDAT
		GL_GJ_LINE::AMOUNT	= GL_YYYY_PP::AMOUNT
		GL_GJ_LINE::CKNO	= GL_YYYY_PP::CKNO
		GL_GJ_LINE::XREFNO	= GL_YYYY_PP::XREFNO
		GL_GJ_LINE::TRANKEY	= GL_YYYY_PP::TRANKEY
		GL_GJ_LINE::SUBACC	= GL_YYYY_PP::SUBACC
		GL_GJ_LINE::OPERATION	= GL_YYYY_PP::OPERATION
		GL_GJ_LINE::UNITS	= GL_YYYY_PP::UNITS
		GL_GJ_LINE::HOURS	= GL_YYYY_PP::HOURS
		GL_GJ_LINE::POSTIM	= GL_YYYY_PP::POSTIM
		GL_GJ_LINE::POSDAT	= GL_YYYY_PP::POSDAT
		GL_GJ_LINE::BATCH	= GL_YYYY_PP::BTHNUM

		PUT #GL_GJ_LINE.CH%

		PRINT ".";
		PRINT IF CCPOS(0%) >= 50%

		GOTO 1000
	END IF

	PRINT

	END
