1	%TITLE "Convert Period to ASCII File"
	%SBTTL "GL_ASCI_YYYYPP"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 2000 BY
	!	Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	!
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	Dump GL periof file into ascii file
	!	.LM -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_ASCI_YYYYPP/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_ASCI_YYYYPP, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_ASCI_YYYYPP.OBJ;*
	!
	! Author:
	!
	!	02/04/2000 - Kevin Handy
	!
	! Modification history:
	!
	!--

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP) GL_YYYY_PP_CDD GL_YYYY_PP

1000	LINPUT "Period (yyyy_pp) "; YYYY_PP$
 !	YYYY_PP$ = "1997_12"
	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"

1010	OPEN "GL" + YYYY_PP$ + ".TXT" FOR OUTPUT AS FILE 1%, &
		RECORDSIZE 400%

	PRINT #1%, &
		LEFT(YYYY_PP$, 4%) + RIGHT(YYYY_PP$, 6%)

2000	RESET #GL_YYYY_PP.CH%

2100	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%, REGARDLESS
	USE
		CONTINUE 3000
	END WHEN

	TEXT$ = TRM$(GL_YYYY_PP::ACCT) + '	' + &
		TRM$(GL_YYYY_PP::SOURCE) + '	' + &
		TRM$(GL_YYYY_PP::REFNO) + '	' + &
		TRM$(GL_YYYY_PP::TRANDAT) + '	' + &
		TRM$(GL_YYYY_PP::DESCR) + '	' + &
		EDIT$(FORMAT$(GL_YYYY_PP::AMOUNT, "#########.##"), -1%) + '	' + &
		TRM$(GL_YYYY_PP::XREFNO) + '	' + &
		TRM$(GL_YYYY_PP::POSTIM) + '	' + &
		TRM$(GL_YYYY_PP::POSDAT) + '	' + &
		TRM$(GL_YYYY_PP::CKNO) + '	' + &
		TRM$(GL_YYYY_PP::TRANKEY) + '	' + &
		TRM$(GL_YYYY_PP::SUBACC) + '	' + &
		TRM$(GL_YYYY_PP::OPERATION) + '	' + &
		NUM1$(GL_YYYY_PP::UNITS) + '	' + &
		NUM1$(GL_YYYY_PP::HOURS) + '	' + &
		TRM$(GL_YYYY_PP::UPDSTA) + '	' + &
		TRM$(GL_YYYY_PP::BTHNUM)

	I% = INSTR(1%, TEXT$, "'")
	WHILE (I%)
		TEXT$ = LEFT(TEXT$, I% - 1%) + "^" + RIGHT(TEXT$, I% + 1%)
		I% = INSTR(1%, TEXT$, "'")
	NEXT

	PRINT #1%, TEXT$

	GOTO 2100

3000	CLOSE #1%

32767	END
