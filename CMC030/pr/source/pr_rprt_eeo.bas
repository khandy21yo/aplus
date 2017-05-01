1	%TITLE "EEO Report"
	%SBTTL "PR_RPRT_EEO"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	! ID:PR021
	!
	! Abstract:HELP
	!	.p
	!	The ^*EEO Report\* lists statistic about employees who worked in
	!	a selected time range. Report summary shows totals of
	!	employees by sex, race and disability by location.
	!	.p
	!	Employees are selected by time range based upon their
	!	hire/termination dates, and not by actual checks
	!	being given out.
	!	.note
	!	Any employees without a location entered in the location
	!	field of the master file will not appear on this report.
	!	.en
	!
	! Index:
	!	.x EEO>Report
	!	.x Report>EEO
	!
	! Option:
	!
	! Author:
	!
	!	09/18/90 - Frank F. Starman
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_EEO
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_EEO, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_EEO.OBJ;*
	!
	! Modification history:
	!
	!	05/22/91 - Kevin Handy
	!		Modified to handle TERMDAY more consistantly.
	!
	!	03/18/92 - Kevin Handy
	!		Modified to grand total Oriental.
	!
	!	04/15/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	11/13/96 - Kevin Handy
	!		Add "A" like "O", and change title from "oriental"
	!		to "asian"
	!		Add "N" into "I".
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/18/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP	(UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!++
	!
	! Abstract:FLD01
	!	^*(01) From Date\*
	!	.p
	!	The ^*From Date\* field selects the beginning
	!	of the date range for employees to be included in this report.
	!	.p
	!	A blank field will cause the report to begin printing with
	!	any employee working before the "To Date" field.
	!
	! Index:
	!	.x From Date>EEO Report
	!	.x EEO Report>From Date
	!
	!--
	FROMDATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(0%), 132%))
	FROMDATE$ = "" IF LEN(EDIT$(FROMDATE$, -1%)) < 8%

	!++
	!
	! Abstract:FLD02
	!	^*(02) To Date\*
	!	.p
	!	The ^*To Date\* setting selects the ending of
	!	the date range for employees to be included in this report.
	!	.p
	!	A blank field will cause the report to include all employees
	!	that worked since the "From Date" field.
	!
	! Index:
	!	.x To Date>EEO Report
	!	.x EEO Report>To Date
	!
	!--
	TODATE$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(1%), 132%))
	TODATE$ = "" IF LEN(EDIT$(TODATE$, -1%)) < 8%

	!++
	!
	! Abstract:FLD03
	!	^*(03) Location\*
	!	.p
	!	The ^*Location\* field selects the location(s) to
	!	be included on this report.
	!	Wildcards may be used, or a list of locations
	!	seperated by commas.
	!
	! Index:
	!	.x Location>EEO Report
	!	.x EEO Report>Location
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

 ReportTitle:

	F.DATE$ = "BEGINNING"
	F.DATE$ = PRNT_DATE(FROMDATE$, 8%) IF EDIT$(FROMDATE$, -1%) <> ""

	T.DATE$ = "TODAY"
	T.DATE$ = PRNT_DATE(TODATE$, 8%) IF EDIT$(TODATE$, -1%) <> ""

	!
	! Title
	!
	TITLE$ = "EEO REPORT"
	TITLE$(2%) = ""

	!
	! Column headings
	!
	TITLE$(3%) = "."

	!
	! Line layouts
	!
	LYT_LINE = "$Location:004,"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #UTL_LOCATION.CH%
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

 NextLocation:
	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	GOTO NextLocation IF LOCATION$ <> "" AND &
		COMP_STRING(EDIT$(UTL_LOCATION::LOCATION, -1%), LOCATION$) = 0%

	TITLE$(1%) = TITLE$ + " FROM " + F.DATE$ + " UNTIL " + T.DATE$ + &
		" AT LOCATION " + UTL_LOCATION::LOCATION + " " + &
		UTL_LOCATION::LOCNAME

17010	WHEN ERROR IN
		FIND #PR_EMP_MASTER.CH%, &
			KEY #4% EQ UTL_LOCATION::LOCATION, &
			REGARDLESS
	USE
		CONTINUE NextLocation IF ERR = 155%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	REC.TOTAL%, OTH%, TOTAL%, DIS%, USC%, MALE%, BLACK%, HIS%, &
		ORI%, INDI%, WHITE% = 0%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE PrintSummary IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	GOTO PrintSummary IF PR_EMP_MASTER::LOCATION <> UTL_LOCATION::LOCATION

	REC.TOTAL% = REC.TOTAL% + 1%

	GOTO GetNextRec IF (PR_EMP_MASTER::TERMDAY < FROMDATE$) AND &
		(PR_EMP_MASTER::TERMDAY > "00000000") AND &
		(EDIT$(FROMDATE$, -1%) <> "")

	GOTO GetNextRec IF PR_EMP_MASTER::HIREDAY > TODATE$ AND &
		EDIT$(TODATE$, -1%) <> ""

	TOTAL% = TOTAL% + 1%
	MALE% = MALE% + 1% IF PR_EMP_MASTER::SEX = "M"
	DIS%  = DIS% + 1% IF PR_EMP_MASTER::DISABLED = "Y"
	USC%  = USC% + 1% IF PR_EMP_MASTER::USCIT = "Y"

	SELECT PR_EMP_MASTER::RACE
	CASE "B"
		BLACK% = BLACK% + 1%
	CASE "H"
		HIS% = HIS% + 1%
	CASE "O", "A"
		ORI% = ORI% + 1%
	CASE "I", "N"
		INDI% = INDI% + 1%
	CASE "W"
		WHITE% = WHITE% + 1%
	CASE ELSE
		OTH% = OTH% + 1%
	END SELECT

	!
	! Try for next record
	!
	GOTO GetNextRec

 PrintSummary:
	TEXT$ = FORMAT$(TOTAL%, "#####") + " employees " + &
		" includes  " + &
		FORMAT$(MALE%, "#####") + &
		" males and " + &
		FORMAT$(TOTAL% - MALE%, "#####") + &
		" females"

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, LIN%)
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 0%)

	LIN% = 999%
	TEXT$ = SPACE$(27%) + &
		FORMAT$(USC%, "#####") + &
		" US citizens and " + &
		FORMAT$(TOTAL% - USC%, "#####") + &
		" non citizens"

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = SPACE$(27%) + &
		FORMAT$(BLACK%, "#####") + &
		" black, " + &
		FORMAT$(HIS%, "#####") + &
		" hispanic, " + &
		FORMAT$(ORI%, "#####") + &
		" asian, " + &
		FORMAT$(INDI%, "#####") + &
		" indian and " + &
		FORMAT$(WHITE%, "#####") + &
		" white" + &
		FORMAT$(OTH%, "#####") + &
		" other"

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = SPACE$(27%) + &
		FORMAT$(DIS%, "#####") + &
		" disabled"

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 0%)


	TEXT$ = FORMAT$(REC.TOTAL%, "#####") + " is total records in location "

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	G.REC.TOTAL% = G.REC.TOTAL% + REC.TOTAL%
	G.TOTAL% = G.TOTAL% + TOTAL%
	G.MALE% = G.MALE% + MALE%
	G.USC% = G.USC% + USC%
	G.DIS% = G.DIS% + DIS%
	G.BLACK% = G.BLACK% + BLACK%
	G.HIS% = G.HIS% + HIS%
	G.ORI% = G.ORI% + ORI%
	G.INDI% = G.INDI% + INDI%
	G.WHITE% = G.WHITE% + WHITE%
	G.OTH% = G.OTH% + OTH%

	PRINT.PAGE% = PRINT.PAGE% + 1%

	GOTO NextLocation

 ExitTotal:
	!
	! Handle end of report
	!
	GOTO ExitProgram IF PRINT.PAGE% <= 1%

	TITLE$(1%) = TITLE$ + " LOCATION TOTALS"

	TEXT$ = FORMAT$(G.TOTAL%, "#####") + " employees " + &
		" includes  " + &
		FORMAT$(G.MALE%, "#####") + &
		" males and " + &
		FORMAT$(G.TOTAL% - G.MALE%, "#####") + &
		" females"

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, LIN%)
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = SPACE$(27%) + &
		FORMAT$(G.USC%, "#####") + &
		" US citizens and " + &
		FORMAT$(G.TOTAL% - G.USC%, "#####") + &
		" non citizens"

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = SPACE$(27%) + &
		FORMAT$(G.BLACK%, "#####") + &
		" black, " + &
		FORMAT$(G.HIS%, "#####") + &
		" hispanic, " + &
		FORMAT$(G.ORI%, "#####") + &
		" asian, " + &
		FORMAT$(G.INDI%, "#####") + &
		" indian and " + &
		FORMAT$(G.WHITE%, "#####") + &
		" white" + &
		FORMAT$(G.OTH%, "#####") + &
		" other"

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = SPACE$(27%) + &
		FORMAT$(G.DIS%, "#####") + &
		" disabled"

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = FORMAT$(G.REC.TOTAL%, "#####") + " is total records in all " + &
		"selected locations"

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

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
	!+-+-+
	!++
	! Abstract:FLD04
	!	^*(04) Detail/Summary (D/S)\*
	!	.p
	!	The ^*Detail/Summary\* report determines the type of report
	!	to be printed.
	!	A ^*D\* entry selects a detailed report that shows each
	!	employee,
	!	while a ^*S\* entry selects only the summary information to be printed.
	!	.note
	!	This field currently does not do anything.
	!	.en
	!
	! Index:
	!	.x Detail/Summary
	!	.x Summary/Detail
	!
	!--
