1	%TITLE "General Ledger Financial Report Writer"
	%SBTTL "GL_ASCI_CHART"
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
	!	Dump chart of accounts in to an ascii file
	!	.LM -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_ASCI_CHART/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_ASCI_CHART, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_ASCI_CHART.OBJ;*
	!
	! Author:
	!
	!	12/15/86 - Kevin Handy
	!
	! Modification history:
	!
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART) GL_CHART_CDD GL_CHART

1000	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"

1010	OUTFILE$ = "GLCHART.TXT"

	OPEN OUTFILE$ FOR OUTPUT AS FILE 1%, &
		RECORDSIZE 255%

	PRINT "Creating "; OUTFILE$

2000	RESET #GL_CHART.CH%

2100	WHEN ERROR IN
		GET #GL_CHART.CH%, REGARDLESS
	USE
		CONTINUE 3000 IF ERR = 11%
	END WHEN

	TEXT$ = TRM$(GL_CHART::ACCT) + '	' + &
		TRM$(GL_CHART::DESCR) + '	' + &
		TRM$(GL_CHART::ACCTYPE) + '	' + &
		TRM$(GL_CHART::FLOW) + '	'+ &
		TRM$(GL_CHART::WORK) + '	' + &
		TRM$(GL_CHART::FINTYPE) + '	' + &
		TRM$(GL_CHART::SUMMARY)

	PRINT #1%, TEXT$

	GOTO 2100

3000	CLOSE #1%

32767	END
