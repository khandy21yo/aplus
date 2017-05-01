1	%TITLE "Make Class Report"
	%SBTTL "MO_RPRT_MAKECLASS"
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
	! ID:MO0040
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Make Class\* report contains the following information:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Class
	!	.le
	!	Description
	!	.els
	!	.lm -5
	!	.b
	!	The means to print only a partial list of the Make Class is
	!	provided by using the From Item, To Item, and Wildcard
	!	options.
	!	.lm -5
	!
	! Index:
	!	.x Make Class>Report
	!	.x Report>Make Class
	!
	! Author:
	!
	!	02/28/91 - Craig Tanner
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_RPRT_MAKECLASS
	!	$ LINK/EXECUTABLE=MO_EXE:*.EXE MO_RPRT_MAKECLASS, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE MO_RPRT_MAKECLASS.OBJ;*
	!
	! Modification history:
	!
	!	10/03/91 - Dan Perkins
	!		Cleaned program code and documentation.
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
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE		UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKECLASS.HB"
	MAP	(MO_MAKECLASS)	MO_MAKECLASS_CDD	MO_MAKECLASS

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
	!	^*(01) From Class _#\*
	!	.b
	!	.lm +5
	!	The ^*From Class _#\* field causes the report
	!	to begin printing with a particular Class _#.
	!	.b
	!	A blank field will cause the report to start with the first
	!	Class _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x From>Class
	!	.x Class>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Class _#\*
	!	.b
	!	.lm +5
	!	The ^*To Class _#\* field causes the printing
	!	to end with a particular Class _#.
	!	.b
	!	A blank field will cause the report to end with the last Class _#
	!	in the file.
	!	.lm -5
	!
	! Index:
	!	.x To>Class
	!	.x Class>To
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

	K_NUM% = 0%

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_MAKECLASS.OPN"
	USE
		FILENAME$ = "MO_MAKECLASS"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Class Report"
	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3%) = "Class     Description"
	TITLE$(4%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #MO_MAKECLASS.CH%, KEY #K_NUM%
		ELSE
			FIND #MO_MAKECLASS.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "MO_MAKECLASS"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #MO_MAKECLASS.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "MO_MAKECLASS"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitProgram IF (MO_MAKECLASS::CLASS > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec IF COMP_STRING &
		(EDIT$(MO_MAKECLASS::CLASS, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	!
	! Print out one line
	!
	TEXT$ = MO_MAKECLASS::CLASS + "      " + &
		MO_MAKECLASS::DESCR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetNextRec

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu

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
	FILENAME$ = ""
	RESUME HelpError

32767	END
