1	%TITLE "TK Modification Information Master File Dump"
	%SBTTL "TK_RPRT_MODINFO"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
	!
	! Computer Management Center
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
	!	.p
	!	The ^*Modification Information Dump\* option provides a
	!	report which is basically just a listing of all the
	!	information in the Modification Information file.
	!	.p
	!	The report will include the following information:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Program Name
	!	.le
	!	System the Program is from
	!	.le
	!	Programmer Name
	!	.le
	!	Modification Date
	!	.le
	!	Author/Modification Flag
	!	.le
	!	Modification Description
	!	.els
	!	.lm -15
	!
	! Index:
	!	.x Print>Modification Information
	!	.x Modification Information>Print
	!	.x Modification Information>Report
	!	.x Report>Modification Information
	!	.x Program Modification>Information Dump
	!
	! Option:
	!
	!
	! Input:
	!
	!
	! Output:
	!
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_RPRT_MODINFO/LINE
	!	$ LINK/EXE=TK_EXE: TK_RPRT_MODINFO, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_RPRT_MODINFO.OBJ;*
	!
	! Author:
	!
	!	06/05/90 - Aaron Redd
	!
	! Modification history:
	!
	!	06/05/90 - Kevin Handy
	!		Added programmer totals, and paging between
	!		programmers.
	!
	!	06/13/90 - Aaron Redd
	!		Fixed the From Item, and also modified to
	!		sort by program name.
	!
	!	03/25/91 - Kevin Handy
	!		Modified from/to dates so that they work.
	!
	!	03/13/92 - Kevin Handy
	!		Unwound error trapping (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE
	!
	!	12/05/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[TK.OPEN]TK_MODINFO.HB"
	MAP	(TK_MODINFO)	TK_MODINFO_CDD TK_MODINFO

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initialize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	%PAGE

	!
	! Set up the user defined input
	!
	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by (SY, PN, DA, PR)\*
	!	.p
	!	The ^*Sort by\* field determines which
	!	order the report will primarily be printed in:  by the
	!	system(s) the programs are from; by program name; by the
	!	author/modification date; or, by the programmer's name.
	!	.p
	!	An entry is not required in this field, as a blank will be
	!	treated as the "SY" option.  Valid codes are:
	!	.b
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	SY - System
	!	.le
	!	DA - Modification Date
	!	.le
	!	PR - Programmer's Name
	!	.le
	!	PN - Program Name
	!	.els
	!	.p
	!	.lm -10
	!
	! Index:
	!
	!--

	!
	! Define file sort key
	!
	SELECT SORT_BY$

	CASE "SY"
		K_NUM% = 1%

	CASE "DA"
		K_NUM% = 2%

	CASE "PR"
		K_NUM% = 3%

	CASE ELSE
		K_NUM% = 0%

	END SELECT


	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.p
	!	The ^*From Item\* field determines the Program
	!	system, Modification Date, or Programmer Name with which the
	!	report will begin printing.  The value must be in agreement
	!	with field (01).
	!	.p
	!	If the setting is blank, the report will begin with the first
	!	appropriate key in the file.
	!
	! Index:
	!
	!--

	!
	! If we're sorting by program name, then find From Item's system
	!
	IF (SORT_BY$ = "PN") AND (FROM_ITEM$ <> "")
	THEN

		UNDER% = INSTR(1%, FROM_ITEM$, "_")

		IF (UNDER% = 3%)
		THEN
			SYSTEM$ = LEFT(FROM_ITEM$, 2%) + "    "

			!** Temporary Addition
			SYSTEM$ = "UTL   " IF (LEFT(FROM_ITEM$, 2%) = "UT")

		ELSE
			SYSTEM$ = "CMCFUN"

			!** Temporary Addition
			SYSTEM$ = "TK    " IF (LEFT(FROM_ITEM$, 4%) = "PTK_")
			SYSTEM$ = "UTL   " IF (LEFT(FROM_ITEM$, 4%) = "UTL_")

			!** Temporary Additions (hopefully)
			SYSTEM$ = "TK    " IF (LEFT(FROM_ITEM$, 12%) = "LIB_COMPRESS")
			SYSTEM$ = "TV    " IF (LEFT(FROM_ITEM$, 14%) = "POST_TO_TVBILL")
			SYSTEM$ = "UTL   " IF (LEFT(FROM_ITEM$, 12%) = "COMPILE_READ")
			SYSTEM$ = "UTL   " IF (LEFT(FROM_ITEM$, 6%) = "SHOERR")
			SYSTEM$ = "UTL   " IF (LEFT(FROM_ITEM$, 5%) = "TOBP2")
		END IF

		FROM_ITEM$ = SYSTEM$ + FROM_ITEM$

	END IF

	!
	! Find the correct place to start, regardless of how we're sorting
	!
	IF (FROM_ITEM$ <> "")
	THEN

		SELECT SORT_BY$
		!
		! Stick an empty date on the end of the key
		!
		CASE "SY"
			FROM_ITEM$ = FROM_ITEM$ + SPACE$(8%)

		!
		! Stick an empty date on the end
		!
		CASE "PN"
			FROM_ITEM$ = FROM_ITEM$ + SPACE$(8%)

		!
		! Stick an empty date on the end of the key
		!
		CASE "PR"
			FROM_ITEM$ = FROM_ITEM$ + SPACE$(8%)

		!
		! Stick an empty program name on the end
		!
		CASE ELSE
			FROM_ITEM$ = FROM_ITEM$ + SPACE$(40%)

		END SELECT

	END IF


	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.p
	!	The ^*To item\* field determines the Program
	!	system, Modification date, or Programmer name with which the
	!	report will end.  The value must be in agreement with
	!	field (01).
	!	.p
	!	A blank setting causes the report to end with the last
	!	appropriate key in the file.
	!
	! Index:
	!
	!--

	FROM_DATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(3%))

	!++
	! Abstract:FLD04
	!	^*(04) From Date\*
	!	.p
	!	The ^*From Date\* field determines the earliest
	!	author/modification information that will be printed in the
	!	dump; that is, only modifications after this From Date will
	!	be printed.
	!	.p
	!	If the setting is blank, there will be no limitation on the
	!	earliest modification printed out.
	!
	! Index:
	!
	!--

	TO_DATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(4%))

	!++
	! Abstract:FLD05
	!	^*(05) To Date\*
	!	.p
	!	The ^*To Date\* field determines the latest
	!	author/modification information that will be printed in the
	!	dump; in other words, only modifications made before this
	!	To Date will be printed out.
	!	.p
	!	A blank setting will place no limitation on how recent of
	!	modifications will be printed out.
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field limits the
	!	information printed to only those records whose keys
	!	are similar to the Wildcard.  The value in this field
	!	must agree with what was given in (01).
	!	.p
	!	An entry is required in this field.
	!
	! Index:
	!
	!--

300	!
	! Open Modification Information Master File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[TK.OPEN]TK_MODINFO.OPN"
	USE
		FILENAME$ = "TK_MODINFO"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Modification Information Master File Dump"
	TITLE$(2%) = "TK System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "ProgramSystem ProgramName                      " + &
		"        ModificationDate Programmer"
	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #TK_MODINFO.CH%, KEY #K_NUM%
		ELSE
			FIND #TK_MODINFO.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "TK_MODINFO"
		CONTINUE HelpError
	END WHEN

	THIS_PROGRAMMER$ = ""
	THIS_PROGRAMMER% = 0%

 GetNextRec:
17050	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #TK_MODINFO.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF (ERR = 11%)
		FILENAME$ = "TK_MODINFO"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record versus TO_ITEM$
	!
	SELECT SORT_BY$

	CASE "DA"
		GOTO ExitTotal IF &
			(TK_MODINFO::MODDATE > TO_ITEM$) AND (TO_ITEM$ <> "")

	CASE "PR"
		GOTO ExitTotal IF &
			(TK_MODINFO::PROGRAMMER > TO_ITEM$) AND (TO_ITEM$ <> "")
		GOSUB ProgrammerTotal IF &
			(TK_MODINFO::PROGRAMMER <> THIS_PROGRAMMER$)

	CASE ELSE
		GOTO ExitTotal IF &
			(TK_MODINFO::SYSTEM > TO_ITEM$) AND (TO_ITEM$ <> "")

	END SELECT

	!
	! Check the current record versus FROM_DATE$ and TO_DATE$
	!
	GOTO GetNextRec IF &
		(TK_MODINFO::MODDATE < FROM_DATE$) AND (FROM_DATE$ <> "")

	GOTO GetNextRec IF &
		(TK_MODINFO::MODDATE > TO_DATE$) AND (TO_DATE$ <> "")

	!
	! Lastly, check the current key against the wildcard
	!
	SELECT SORT_BY$

	CASE "DA"
		GOTO GetNextRec IF &
			COMP_STRING(TK_MODINFO::MODDATE,WLDCRD$) = 0%

	CASE "PR"
		GOTO GetNextRec IF &
			COMP_STRING(TK_MODINFO::PROGRAMMER,WLDCRD$) = 0%

	CASE ELSE
		GOTO GetNextRec IF &
			COMP_STRING(TK_MODINFO::SYSTEM,WLDCRD$) = 0%

	END SELECT

	!
	! Initialize some variables
	!
	FLAG%, INDEX% = 0%

	!
	! Print out part of one record
	!
17100	TEXT$ = TK_MODINFO::SYSTEM + "        " + &
		TK_MODINFO::PROGNAME + " " + &
		PRNT_DATE(TK_MODINFO::MODDATE, 8%) + "       " + &
		TK_MODINFO::PROGRAMMER

	IF (TK_MODINFO::MODFLAG = "A")
	THEN
		TEXT$ = TEXT$ + " (AUTHOR)"
		FLAG% = -1%
	ELSE
		TEXT$ = TEXT$ + " (MODIFIER)"
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! If this is a modification, then print out the mod. description
	! (if any)
	!
17200	GOTO 17300 IF (TRM$(TK_MODINFO::MODDESCR(INDEX%)) = "") OR (FLAG% = -1%)

	TEXT$ = SPACE$(52%) + TK_MODINFO::MODDESCR(INDEX%)

	TEXT$ = SPACE$(25%) + "Modification Description:  " + &
		TK_MODINFO::MODDESCR(INDEX%) IF (INDEX% = 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	INDEX% = INDEX% + 1%

	GOTO 17200

	!
	! Print out a blank line
	!
17300	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	GOSUB ProgrammerTotal IF SORT_BY$ = "PR"

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

 ProgrammerTotal:
	!*******************************************************************
	! Print out totals for one programmer, and page
	!*******************************************************************

	IF THIS_PROGRAMMER% <> 0%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		TEXT$ = "   Total of " + NUM1$(THIS_PROGRAMMER%) + &
			"items for " + THIS_PROGRAMMER$
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 3000%)

	END IF

	THIS_PROGRAMMER$ = TK_MODINFO::PROGRAMMER
	THIS_PROGRAMMER% = 0%

	RETURN

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
