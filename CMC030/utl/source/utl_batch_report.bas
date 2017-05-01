1	%TITLE "Select Report to Print"
	%SBTTL "UTL_BATCH_REPORT"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1987, 1988 BY
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
	! Command:REPORT
	!
	! Abstract:HELP
	!	.p
	!	The ^*Select Report to Print\* option
	!	selects a report to run.
	!	.p
	!
	! Index:
	!	.x Report>Selection
	!
	! Input:
	!	UTL
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_BATCH_REPORT/LINE
	!	$ LINK/EXECUTABLE=UTL_EXE: UTL_BATCH_REPORT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_BATCH_REPORT.OBJ;*
	!
	! Author:
	!
	!	09/14/87 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	!++
	! Abstract:COMMAND
	!	^*REPORT\*
	!	.p
	!	^*Report\* extracts the specified report.
	!	.p
	!	^*Format:REPORT\*
	!	.p
	!	^*Example:\*
	!	.literal
	!	Menu Command Level> /REPORT
	!	.end literal
	!
	! Index:
	!	.x Report>Selection
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Clear core common
	!
	V$ = SYS('8'C)

	!
	! Chain into report
	!
	CHAIN "CMC:UTL_REPORT"

	END
