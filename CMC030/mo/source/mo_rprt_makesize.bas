1	%TITLE "Manufacture Order Make Size Report"
	%SBTTL "MO_RPRT_MAKESIZE"
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
	! ID:MO0041
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Make Size\* report contains the following information:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Size
	!	.le
	!	Description
	!	.els
	!	.lm -5
	!	.b
	!	The means to print only a partial list of the Make Class is
	!	provided by the use of the From Item, To Item, and Wildcard
	!	options.
	!	.lm -5
	!
	! Index:
	!	.x Make Size>Report
	!	.x Report>Make Size
	!
	! Author:
	!
	!	02/28/91 - Craig Tanner
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_RPRT_MAKESIZE
	!	$ LINK/EXECUTABLE=MO_EXE:*.EXE MO_RPRT_MAKESIZE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE MO_RPRT_MAKESIZE.OBJ;*
	!
	! Modification history:
	!
	!	10/04/91 - Dan Perkins
	!		Cleaned up program code, and
	!		documentation.  Checked error
	!		trapping.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/01/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/21/2000 - Kevin Handy
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
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKESIZE.HB"
	MAP	(MO_MAKESIZE)	MO_MAKESIZE_CDD	MO_MAKESIZE

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
	!	^*(01) From Size _#\*
	!	.b
	!	.lm +5
	!	The ^*From Size _#\* field causes the report
	!	to begin with a particular Size _#.
	!	.b
	!	A blank field will cause the report to start with the first
	!	Size _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x From>Size
	!	.x Size>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Size _#\*
	!	.b
	!	.lm +5
	!	The ^*To Size _#\* field causes the printing
	!	to end with a particular Size _#.
	!	.b
	!	A blank field will cause the report to end with the last Size _#
	!	in the file.
	!	.lm -5
	!
	! Index:
	!	.x To>Size
	!	.x Size>To
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
		%INCLUDE "SOURCE:[MO.OPEN]MO_MAKESIZE.OPN"
	USE
		FILENAME$ = "MO_MAKESIZE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "MAKE SIZE REPORT"
	TITLE$(2%) = "Manufacturing Order System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Size      Description"
	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #MO_MAKESIZE.CH%, KEY #0%
		ELSE
			FIND #MO_MAKESIZE.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "MO_MAKESIZE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #MO_MAKESIZE.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "MO_MAKESIZE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitProgram IF (MO_MAKESIZE::MSIZE > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec IF COMP_STRING &
		(EDIT$(MO_MAKESIZE::MSIZE, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	!
	! Print out one line
	!
	TEXT$ = MO_MAKESIZE::MSIZE + "      " + &
		MO_MAKESIZE::DESCR

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
	FILENAME$ = ""
	RESUME HelpError

32767	END
