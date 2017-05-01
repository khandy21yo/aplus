1	%TITLE "TK Modification Information Master File Summary"
	%SBTTL "TK_RPRT_MODINFO_SUM"
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
	!	The ^*Modification Information Summary\* option provides a
	!	report which is a summary of selected information from the
	!	Modification Information file.
	!	.p
	!	Depending upon the user's choice, the report will list the
	!	number of modifications according to system, program name,
	!	programmer's name, or the date.  In addition, the report can
	!	total up author/modification information according to the
	!	modification date, by month.
	!
	! Index:
	!	.x Print>Modification Information Summary
	!	.x Modification Information>Print Summary
	!	.x Modification Information>Summary
	!	.x Report>Modification Information Summary
	!	.x Program Modification>Information Summary
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
	!	$ BAS TK_SOURCE:TK_RPRT_MODINFO_SUM/LINE
	!	$ LINK/EXE=TK_EXE: TK_RPRT_MODINFO_SUM, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_RPRT_MODINFO_SUM.OBJ;*
	!
	! Author:
	!
	!	06/05/90 - Aaron Redd
	!
	! Modification history:
	!
	!	06/06/90 - Aaron Redd
	!		Modified to summarize also by the modification date;
	!		Also added the layouts needed to send the report to
	!		a 20/20 file or other database.
	!
	!	05/24/91 - Kevin Handy
	!		Added graph to side of report.
	!
	!	05/28/91 - Kevin Handy
	!		Added "T" (total) to graph display.
	!
	!	05/29/91 - Kevin Handy
	!		Many modifications to reduce craze select statements
	!		and make it look more like one report instead
	!		of 30 seperate reports thrown into the same source
	!		code.
	!
	!	05/29/91 - Kevin Handy
	!		Added subtotals
	!
	!	05/31/91 - Kevin Handy
	!		Fixed minor bug in graph in that rel loc (0) and
	!		rel loc (1) graphed to same point.
	!
	!	07/25/91 - Kevin Handy
	!		Added code to display information about day-of-week
	!		statistics.
	!
	!	07/28/91 - Kevin Handy
	!		Modified so that by date code could work.
	!
	!	07/30/91 - Kevin Handy
	!		Modified to display decimal point in averages.
	!
	!	09/26/91 - Kevin Handy
	!		Fixed from-to date.
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

	!
	! Graphing line
	!
	DECLARE INTEGER CONSTANT MAXGRAPH = 50%
	DECLARE INTEGER CONSTANT MAXEST = 100%

	DIM BYDAY%(7%)

	%PAGE

	!*******************************************************************
	! Function to handle simple graphing problem presented by this
	! program.
	!*******************************************************************

	DEF FNGRAPH$(A%, B%)

		APOSIT% = A% * MAXGRAPH / MAXEST + 1%
		APOSIT% = MAXGRAPH IF APOSIT% > MAXGRAPH
		BPOSIT% = B% * MAXGRAPH / MAXEST + 1%
		BPOSIT% = MAXGRAPH IF BPOSIT% > MAXGRAPH
		TPOSIT% = (A% + B%) * MAXGRAPH / MAXEST + 1%
		TPOSIT% = MAXGRAPH IF TPOSIT% > MAXGRAPH

		SELECT BPOSIT%
		CASE APOSIT%
			QGRAPH$ = SPACE$(APOSIT% - 1%) + "*"

		CASE < APOSIT%
			QGRAPH$ = SPACE$(BPOSIT% - 1%) + "X" + &
				SPACE$(APOSIT% - BPOSIT% - 1%) + "+"

		CASE > APOSIT%
			QGRAPH$ = SPACE$(APOSIT% - 1%) + "+" + &
				SPACE$(BPOSIT% - APOSIT% - 1%) + "X"

		END SELECT

		IF LEN(QGRAPH$) < TPOSIT%
		THEN
			QGRAPH$ = QGRAPH$ + SPACE$(TPOSIT% - LEN(QGRAPH$) - 1%) + "T"
		END IF

		FNGRAPH$ = QGRAPH$
	FNEND

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
	!	^*(01) Sort by (SY, PN, PR, DA)\*
	!	.p
	!	The ^*Sort by\* field determines which
	!	order the summary will be printed in:  by the system(s) the
	!	programs are from; by the program name; by the programmer's
	!	name; or, by the date of authoring/modification.
	!	.p
	!	An entry is required in this field.  Valid codes are:
	!	.b
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	SY - System
	!	.le
	!	PN - Program Name
	!	.le
	!	PR - Programmer's Name
	!	.le
	!	DA - Modification Date
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

	CASE "DA"
		K_NUM% = 2%

	CASE "PR"
		K_NUM% = 3%

	CASE "PN"
		K_NUM% = 0%

	CASE ELSE
		K_NUM% = 1%

	END SELECT


	BYDATE$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) By Date (M,D,N)\*
	!	.p
	!	The ^*By Date\* field is either
	!	monthly, daily, or not at all; if it is monthly, then
	!	the report will total up all the author/modification data
	!	as the month changes, as well as when the programmer's name,
	!	program name, or system changes.  For example, in
	!	report, there will would be a line showing totals for
	!	programmer so-and-so on one date, another line for the
	!	same programmer on another date, and so on.  Similarly,
	!	the "daily" option will total the author/modification
	!	data as the date changes from daya to day.  The "N" option
	!	will have the report ignore the date and simply total up
	!	the data for each programmer, program, or system.
	!	.p
	!	Unless the user also opts to go by month, this field will
	!	be ignored if the user wants to summarize by modification
	!	date (option "DA" in field 01).
	!	.p
	!	An entry is required in this field.
	!
	! Index:
	!
	!--


	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
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


	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) From Item\*
	!	.p
	!	The ^*From Item\* field determines the Program
	!	system, Program Name, Modification Date, or Programmer's Name
	!	with which the report will begin printing.  The value must be
	!	in agreement with field (01).
	!	.p
	!	If the setting is blank, the report will begin with the first
	!	item in the file that is appropriate (according to the Sort By).
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


	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) To Item\*
	!	.p
	!	The ^*To item\* field determines the Program
	!	system, Program name, Modification date, or Programmer's name
	!	with which the report will end.  The value must be in agreement
	!	with field (01).
	!	.p
	!	A blank setting will cause the report to end with the last
	!	appropriate record in the file.
	!
	! Index:
	!
	!--

	FROM_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(5%), -1%))

	!++
	! Abstract:FLD06
	!	^*(06) From Date\*
	!	.p
	!	The ^*From Date\* field determines the earliest
	!	author/modification information that will be used in the
	!	summary; that is, only modifications after this From Date will
	!	be printed.
	!	.p
	!	If the setting is blank, there will be no limitation on the
	!	earliest modification to be used.
	!
	! Index:
	!
	!--

	TO_DATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(6%), -1%))

	!++
	! Abstract:FLD07
	!	^*(07) To Date\*
	!	.p
	!	The ^*To Date\* field determines the latest
	!	author/modification information that will be used in the
	!	used; in other words, only modifications made before this
	!	To Date will be used in totaling for the summary.
	!	.p
	!	A blank setting will place no limitation on how recent of
	!	modifications will be used.
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
	TITLE$(1%) = "Modification Information Master File Summary"

	SELECT SORT_BY$

	CASE "DA"
		TITLE$(2%) = "Summarized by Modification Date"
		TITLE$(2%) = "Summarized Monthly Activity" &
			IF BYDATE$ = "M"
		LYT_LINE$ = "DModDate:033,VAuthorTotal:046,VModTotal:056"
		TITLE$(5%) = "Date      Date     " + &
			"      Authored  Modified"

	CASE "PR"
		TITLE$(2%) = "Summarized by Programmer Name"
		TITLE$(2%) = "Summarized Daily Activity by Programmer Name" &
			IF BYDATE$ = "D"
		TITLE$(2%) = "Summarized Monthly Activity by Programmer Name" &
			IF BYDATE$ = "M"
		LYT_LINE$ = "$Programmer:041,DModDate:053," + &
			"VAuthorTotal:069,VModTotal:079"
		TITLE$(5%) = " Programmer                     " + &
			"           ModificationDate  Authored  Modified"

	CASE "PN"
		TITLE$(2%) = "Summarized by Program Name"
		TITLE$(2%) = "Summarized Daily Activity by Program Name" &
			IF BYDATE$ = "D"
		TITLE$(2%) = "Summarized Monthly Activity by Program Name" &
			IF BYDATE$ = "M"
		LYT_LINE$ = "$ProgramName:041,DModDate:053," + &
			"VAuthorTotal:069,VModTotal:079"
		TITLE$(5%) = " ProgramName                    " + &
			"           ModificationDate  Authored  Modified"

	CASE ELSE
		TITLE$(2%) = "Summarized by System"
		TITLE$(2%) = "Summarized Daily Activity by System" &
			IF BYDATE$ = "D"
		TITLE$(2%) = "Summarized Monthly Activity by System" &
			IF BYDATE$ = "M"
		LYT_LINE$ = "$System:023,DModDate:035," + &
			"VAuthorTotal:052,VModTotal:062"
		TITLE$(5%) = "System  " + &
			"ModificationDate   Authored  Modified"

	END SELECT

	TITLE$(3%) = "TK System"
	TITLE$(4%) = ""
	TITLE$(6%) = "."

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

	!
	! Set PREV_KEY$ to a flag value
	!
	PREV_KEY$ = ""
	PREV_DATE$ = ""

	A_TOTAL%, M_TOTAL% = 0%
	BYDAY%(I%) = 0% FOR I% = 1% TO 7%

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
		THIS_ITEM$ = TK_MODINFO::MODDATE

	CASE "PR"
		THIS_ITEM$ = TK_MODINFO::PROGRAMMER

	CASE "PN"
		THIS_ITEM$ = TK_MODINFO::PROGNAME

	CASE ELSE
		THIS_ITEM$ = TK_MODINFO::SYSTEM

	END SELECT

	GOTO ExitTotal IF &
		(THIS_ITEM$ > TO_ITEM$) AND (TO_ITEM$ <> "")
	GOTO GetNextRec IF &
		COMP_STRING(THIS_ITEM$, WLDCRD$) = 0%

	!
	! Check the current record versus FROM_DATE$ and TO_DATE$
	!
	GOTO GetNextRec IF &
		(TK_MODINFO::MODDATE < FROM_DATE$) AND (FROM_DATE$ <> "")

	GOTO GetNextRec IF &
		(TK_MODINFO::MODDATE > TO_DATE$) AND (TO_DATE$ <> "")


17100	!
	! Handle BYDATE function
	!
	SELECT BYDATE$

	CASE "D"
		THIS_DATE$ = TK_MODINFO::MODDATE
	CASE "M"
		THIS_DATE$ = LEFT(TK_MODINFO::MODDATE, 6%) + "00"
	CASE "N"
		THIS_DATE$ = "        "

	END SELECT

	!
	! Check the current record; is it different from the last one?
	!
	IF (THIS_ITEM$ <> PREV_KEY$) OR (THIS_DATE$ <> PREV_DATE$)
	THEN
		GOSUB PrintLine
	END IF

	!
	! Check for subtotal needed
	!
	IF (THIS_ITEM$ <> PREV_KEY$)
	THEN
		Gosub Subtotal
	END IF

	PREV_KEY$ = THIS_ITEM$
	PREV_DATE$ = THIS_DATE$

	!
	! Add this record tot totals
	!
	IF (TK_MODINFO::MODFLAG = "A")
	THEN
		A_TOTAL% = A_TOTAL% + 1%
		A_SUBTOTAL% = A_SUBTOTAL% + 1%
	ELSE
		M_TOTAL% = M_TOTAL% + 1%
		M_SUBTOTAL% = M_SUBTOTAL% + 1%
	END IF

	I% = DATE_DAYOFWEEK(DATE_DAYCODE(TK_MODINFO::MODDATE))
	BYDAY%(I%) = BYDAY%(I%) + 1%

	GOTO GetNextRec

	%PAGE

	!*******************************************************************
	! Print out subtotal
	!*******************************************************************
 Subtotal:
	IF L_SUBTOTAL% <> 0%
	THEN
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", 0%)

		!
		! Make a TEXT$ to print out shortly
		!
		TEXT$ = PREV_KEY$ + " " + &
			"Total " + FORMAT$(L_SUBTOTAL%, "####") + " " + &
			FORMAT$(A_SUBTOTAL%, "       ##,###") + &
			FORMAT$(M_SUBTOTAL%, "    ##,### ") + &
			FNGRAPH$(A_SUBTOTAL%, M_SUBTOTAL%)

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, -1%)

		TEXT$ = PREV_KEY$ + " " + &
			"Averg " + FORMAT$(L_SUBTOTAL%, "####") + " " + &
			FORMAT$(1.0 * A_SUBTOTAL% / L_SUBTOTAL%, "       ##.###") + &
			FORMAT$(1.0 * M_SUBTOTAL% / L_SUBTOTAL%, "    ##.### ") + &
			FNGRAPH$(A_SUBTOTAL% / L_SUBTOTAL%, M_SUBTOTAL% / L_SUBTOTAL%)

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, -2%)

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", 2%)

		TEXT$ = "                    Mon   Tue   Wed   Thr   Fri   Sat   Sun"

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", 3%)
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

		TEXT$ = "Total By Day     "
		TEXT$ = TEXT$ + FORMAT$(BYDAY%(I%), "######") &
			FOR I% = 1% TO 7%

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, -1%)

		TEXT$ = "Average By Day   "
		TEXT$ = TEXT$ + FORMAT$(1.0 * BYDAY%(I%) / L_SUBTOTAL%, "##.###") &
			FOR I% = 1% TO 7%

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, -2%)

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", 3000%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	END IF

	L_SUBTOTAL% = 0%
	A_SUBTOTAL% = 0%
	M_SUBTOTAL% = 0%

	BYDAY%(I%) = 0% FOR I% = 1% TO 7%

	RETURN

	%PAGE

	!*******************************************************************
	! Print out one line
	!*******************************************************************
 PrintLine:
	IF (A_TOTAL% <> 0%) OR (M_TOTAL% <> 0%)
	THEN
		!
		! Make a TEXT$ to print out shortly
		!
		TEXT$ = PREV_KEY$ + " " + &
			PRNT_DATE(PREV_DATE$, 8%) + " " + &
			FORMAT$(A_TOTAL%, "       ##,###") + &
			FORMAT$(M_TOTAL%, "    ##,### ") + &
			FNGRAPH$(A_TOTAL%, M_TOTAL%)

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		L_SUBTOTAL% = L_SUBTOTAL% + 1%
	END IF

	!
	! Reset these variables
	!
	A_TOTAL% = 0%
	M_TOTAL% = 0%

	RETURN

	%PAGE

 ExitTotal:
	!*******************************************************************
	! Print Totals
	!*******************************************************************

	GOSUB PrintLine
	GOSUB SubTotal

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
