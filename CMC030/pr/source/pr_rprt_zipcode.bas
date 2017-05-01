1	%TITLE "PR Employee Zip Code Report"
	%SBTTL "PR_RPRT_ZIPCODE"
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
	! ID:PR056
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Location Zip Code report\* prints a list of where all employees live by
	!	zip code in the following columns:
	!	.table 3,25
	!	.te
	!	Location
	!	.te
	!	Location Name
	!	.te
	!	Zip Code
	!	.te
	!	Number of Employees
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Employee Zip Code Report
	!	.x Location>Zip Code>Report
	!	.x Report>Location>Zip Code
	!	.x Zip Code>Location>Report
	!	.x Report>Zip Code>Location
	!
	! Option:
	!
	! Author:
	!
	!	06/21/89 - J. Shad Rydalch
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_ZIPCODE
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_ZIPCODE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_ZIPCODE.OBJ;*
	!
	! Modification history:
	!
	!	06/19/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	05/22/91 - Kevin Handy
	!		Modified to handle TERMDAY more consistantly.
	!
	!	06/14/93 - Kevin Handy
	!		Added REGARDLESS twice to get on UTL_LOCATION.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/25/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/15/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%PAGE

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP	(UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	MAP (PR_TEMP) &
		PR_TEMP.LOCATION$ = 4%, &
		PR_TEMP.ZIPCODE$ = 10%

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Find a channel number for the temporary file
	!
	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

	!
	! Get the UTL work device
	!
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

	%PAGE

 Init:
	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Location\*
	!	.b
	!	.lm +5
	!	The ^*From Location\* field causes the printing
	!	to begin with a particular location.
	!	.b
	!	A blank field will cause the report to start with the first location in the
	!	file.
	!	.lm -5
	!
	! Index:
	!	.x From Location>Employee Zip Code Report
	!	.x Employee Zip Code Report>From Location
	!
	!--
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) To Location\*
	!	.b
	!	.lm +5
	!	The ^*To Location\* field causes the printing
	!	to end with a particular location.
	!	.b
	!	A blank field causes the report to end with the last location in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Location>Employee Zip Code Report
	!	.x Employee Zip Code Report>To Location
	!
	!--
	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* setting enables the user to print a report including selected
	!	employees only using the wildcard technique.
	!	.b
	!	Valid wildcard characters are an asterisk (_*) or a question mark (?). An
	!	asterisk (_*) indicates all employees will be considered. A question mark(?)
	!	in a field position indicates an employee with any character in that equivalent
	!	position will be considered.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Employee Zip Code Report
	!	.x Employee Zip Code Report>Wildcard
	!
	!--
	DATE_TERM_EMP$  = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(3%), 132%))
	!++
	! Abstract:FLD04
	!	^*(04) Exclude to Term Date\*
	!	.b
	!	.lm +5
	!	The ^*Exclude to Term Date\* field allows for a parameter to be set in
	!	determining which employees will be included.
	!	.b
	!	For example: if the report was to include all employees except
	!	those who were terminated before 090290, that date would be
	!	entered in the field.
	!	.lm -5
	!
	! Index:
	!	.x Exclude>to Term Date>Employee Zip Code Report
	!	.x Employee Zip Code Report>Exclude>to Term Date
	!
	!--

	FIRST$ = "Y"

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
		CONTINUE 320
	END WHEN

320	CALL ENTR_3MESSAGE(SCOPE, "Creating work file. ", 17%)

	OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT AS FILE PR_TEMP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		BUFFER 32%, &
		MAP PR_TEMP, &
		TEMPORARY, &
		PRIMARY KEY (PR_TEMP.LOCATION$,PR_TEMP.ZIPCODE$) DUPLICATES, &
		ACCESS MODIFY, ALLOW NONE

400	RESET #PR_EMP_MASTER.CH%, KEY #4%

410	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	GOTO 410 IF (PR_EMP_MASTER::TERMDAY < DATE_TERM_EMP$) AND &
		(PR_EMP_MASTER::TERMDAY > "00000000")

	GOTO 410 IF COMP_STRING(EDIT$(PR_EMP_MASTER::LOCATION, 132%), &
		WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	IF (PR_EMP_MASTER::LOCATION >= FROM_ITEM$ AND FROM_ITEM$ <> "") AND &
		(PR_EMP_MASTER::LOCATION <= TO_ITEM$ AND TO_ITEM$ <> "") OR &
		(FROM_ITEM$ = "" AND TO_ITEM$ = "")

	THEN
		PR_TEMP.ZIPCODE$ = PR_EMP_MASTER::ZIP
		PR_TEMP.LOCATION$ = PR_EMP_MASTER::LOCATION

		PUT #PR_TEMP.CH%
	END IF

	GOTO 410

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Location Zip Code Report"
	TITLE$(2%) = ""

	!
	! Column headings
	!
	TITLE$(3%) = "Location LocationName                             " + &
		"ZipCode    NumberOfEmployees"
	TITLE$(4%) = "."

	!
	! Line layouts
	!
	LYT_LINE = "$Location:004,$LocationName:049,$ZipCode:060,VNumOfEmp:078"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	RESET #PR_TEMP.CH%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_TEMP.CH%
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	LOC_TOTAL = LOC_TOTAL + 1%
	GRAND_TOTAL = GRAND_TOTAL + 1%
	NUMBER_EMP = NUMBER_EMP + 1%

	IF FIRST$ = "Y"
	THEN
		PRINT_LOC_DESCR$ = "Y"
		FIRST$ = "N"
		CHECK_LOC1$ = PR_TEMP.LOCATION$
		CHECK_LOC$ = PR_TEMP.LOCATION$
		CHECK_ZIP$ = PR_TEMP.ZIPCODE$
		NUMBER_EMP = 0%
		LOC_TOTAL = 0%
	END IF

	GOTO GetNextRec IF CHECK_LOC$ = PR_TEMP.LOCATION$ OR &
		CHECK_ZIP$ = PR_TEMP.ZIPCODE$

	!
	! Print out one line
	!
	IF PRINT_LOC_DESCR$ = "Y"
	THEN
		FILL_LINE$ = "?"
	ELSE
		FILL_LINE$ = " "
	END IF

	UTL_LOCATION::LOCNAME = STRING$(LEN(UTL_LOCATION::LOCNAME), &
		ASCII(FILL_LINE$))

17500	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, KEY #0% EQ CHECK_LOC$, REGARDLESS &
			IF PRINT_LOC_DESCR$ = "Y"
	USE
		CONTINUE 17550 IF (ERR = 155%) OR (ERR = 9%)
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

17550	TEXT$ = CHECK_LOC$ + "     " + &
		UTL_LOCATION::LOCNAME + " " + &
		CHECK_ZIP$ + SPACE$(14%) + &
		FORMAT$(NUMBER_EMP, "####")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CHECK_LOC$ = PR_TEMP.LOCATION$
	CHECK_ZIP$ = PR_TEMP.ZIPCODE$
	NUMBER_EMP = 0%
	PRINT_LOC_DESCR$ = "N"

	GOTO GetNextRec IF CHECK_LOC1$ = PR_TEMP.LOCATION$

	TEXT$ = SPACE$(46%) + "Location Total" + SPACE$(14%) + &
		FORMAT$(LOC_TOTAL, "####")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	LOC_TOTAL = 0%
	PRINT_LOC_DESCR$ = "Y"
	CHECK_LOC1$ = PR_TEMP.LOCATION$

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	IF PRINT_LOC_DESCR$ = "Y"
	THEN
		FILL_LINE$ = "?"
	ELSE
		FILL_LINE$ = " "
	END IF

	UTL_LOCATION::LOCNAME = STRING$(LEN(UTL_LOCATION::LOCNAME), &
		ASCII(FILL_LINE$))

17600	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, KEY #0% EQ CHECK_LOC$, REGARDLESS &
			IF PRINT_LOC_DESCR$ = "Y"

	USE
		CONTINUE 17550 IF (ERR = 155%) OR (ERR = 9%)
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

17650	TEXT$ = CHECK_LOC$ + "     " + &
		UTL_LOCATION::LOCNAME + " " + &
		CHECK_ZIP$ + SPACE$(14%) + &
		FORMAT$(NUMBER_EMP + 1%, "####")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(46%) + "Location Total" + SPACE$(14%) + &
		FORMAT$(LOC_TOTAL + 1%, "####")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = SPACE$(49%) + "Grand Total" + SPACE$(14%) + &
		FORMAT$(GRAND_TOTAL, "####")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Close the channel
	!
	CLOSE #PR_TEMP.CH%

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
