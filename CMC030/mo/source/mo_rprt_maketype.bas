1	%TITLE "Make Type Report"
	%SBTTL "MO_RPRT_MAKETYPE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991, BY
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
	! ID:MO0042
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Make Type\* report contains the following information:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Type
	!	.le
	!	Description
	!	.els
	!	.lm -5
	!	.b
	!	The means to print only a partial list of the Make Type is
	!	provided by use of the From Item, To Item, and Wildcard
	!	options.
	!	.lm -5
	!
	! Index:
	!	.x Make Type>Report
	!	.x Report>Make Type
	!
	! Author:
	!
	!	03/11/91 - Craig Tanner
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_RPRT_MAKETYPE
	!	$ LINK/EXECUTABLE=MO_EXE:*.EXE MO_RPRT_MAKETYPE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE MO_RPRT_MAKETYPE.OBJ;*
	!
	! Modification history:
	!
	!	10/04/91 - Dan Perkins
	!		Cleaned up program code and documentation.
	!		Checked error trapping.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/02/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/24/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKETYPE.HB"
	MAP (MO_MAKETYPE)	MO_MAKETYPE_CDD		MO_MAKETYPE

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Type\*
	!	.b
	!	.lm +5
	!	The ^*From Type\* field causes the report
	!	to begin with a particular Type _#.
	!	.b
	!	A blank field will cause the report to start with the first
	!	Type _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x From>Type
	!	.x Type>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Type\*
	!	.b
	!	.lm +5
	!	The ^*To Type\* field causes the printing
	!	to end with a particular Type _#.
	!	.b
	!	A blank field will cause the report to end with the last Type _#
	!	in the file.
	!	.lm -5
	!
	! Index:
	!	.x To>Type
	!	.x Type>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	for wildcarding technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_MAKETYPE.OPN"
	USE
		FILENAME$ = "MO_MAKETYPE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "MAKE TYPE REPORT"
	TITLE$(2%) = "Manufacturing Order System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Type   Description"
	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #MO_MAKETYPE.CH%
		ELSE
			FIND #MO_MAKETYPE.CH%, KEY #1% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "MO_MAKETYPE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #MO_MAKETYPE.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "MO_MAKETYPE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitProgram IF (MO_MAKETYPE::MTYPE > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec IF COMP_STRING &
		(EDIT$(MO_MAKETYPE::MTYPE, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	!
	! Print out one line
	!
	TEXT$ = MO_MAKETYPE::MTYPE + "     " + &
		MO_MAKETYPE::DESCR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetNextRec

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	RESUME HelpError

32767	END
