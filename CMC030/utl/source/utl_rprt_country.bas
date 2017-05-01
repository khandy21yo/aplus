1	%TITLE "Country, State, and County"
	%SBTTL "UTL_RPRT_COUNTRY"
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
	! ID:UT008
	!
	! Abstract:HELP
	!	.p
	!	The ^*Country, State and County\* option prints
	!	a report which contains the following categories:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Country Code
	!	.le
	!	Country
	!	.le
	!	State Code
	!	.le
	!	State
	!	.le
	!	County Code
	!	.le
	!	County
	!	.els
	!
	! Index:
	!
	! Author:
	!
	!	02/10/88 - Aaron Redd
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_RPRT_COUNTRY/LINE
	!	$ LINK/EXECUTABLE=UTL_EXE:*.EXE UTL_RPRT_COUNTRY, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_RPRT_COUNTRY.OBJ;*
	!
	! Modification history:
	!
	!	05/21/90 - Frank F. Starman
	!		Added COMMAND help message.
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
	!	07/17/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	!++
	! Abstract:COMMAND
	!	^*COUNTRY/LIST\*
	!	.p
	!	The ^*Country/List\* option prints the
	!	definition tables for country, state, and county codes.
	!	.p
	!	^*Format: COUNTRY/LIST\*
	!	.p
	!	^*Example:\*
	!	.literal
	!	Menu Command Level> /COUNTRY/LIST
	!	.end literal
	!
	! Index:
	!	.x COUNTRY/LIST
	!
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP	(UTL_COUNTRY)	UTL_COUNTRY_CDD	UTL_COUNTRY

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_STATE.HB"
	MAP	(UTL_STATE)	UTL_STATE_CDD	UTL_STATE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTY.HB"
	MAP	(UTL_COUNTY)	UTL_COUNTY_CDD	UTL_COUNTY

	MAP	(DP_OUTP_XUNSOL)		RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	OUTP_UNSOLICITED

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Country\*
	!	.p
	!	The ^*From Country\* value
	!	causes the printing to begin with the
	!	selected Country.
	!	.p
	!	A blank setting will cause the report to begin with
	!	the first country in the file.
	!
	! Index:
	!	.x From Country
	!	.x Country>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Country\*
	!	.p
	!	The ^*To Country\* value causes the
	!	printing to end with the selected country.
	!	.p
	!	A blank setting will cause the report to end with the last
	!	country in the file.
	!
	! Index:
	!	.x To Country
	!	.x Country>To
	!
	!--


300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.OPN"
	USE
		FILENAME$ = "UTL_COUNTRY"
		CONTINUE HelpError
	END WHEN

	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_STATE.OPN"
	USE
		FILENAME$ = "UTL_STATE"
		CONTINUE HelpError
	END WHEN

	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTY.OPN"
	USE
		FILENAME$ = "UTL_COUNTY"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Country/State/County File List"
	TITLE$(2%) = "List of Countries, States, and Counties"
	TITLE$(3%) = ""

	TITLE$(4%) = "CountryCode Country  "
	TITLE$(5%) = "             StateCode  State                                     FIPS"
	TITLE$(6%) = "                         CountyCode  County        "
	TITLE$(7%) = ""

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #UTL_COUNTRY.CH%
		ELSE
			FIND #UTL_COUNTRY.CH%, &
				KEY #0% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check unsolicited input
	!
	IF RRR_FLAG%
	THEN
		IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
		THEN
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitProgram
		END IF
	END IF

	!
	! Get next record (Country)
	!
	WHEN ERROR IN
		GET #UTL_COUNTRY.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal
	END WHEN

	!
	! Check current record (Country)
	!
	GOTO ExitTotal IF (UTL_COUNTRY::COUNTRY > TO_ITEM$) AND TO_ITEM$ <> ""

	!
	! Check unsolicited input
	!
	IF RRR_FLAG%
	THEN
		IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
		THEN
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitProgram
		END IF
	END IF


	!
	! Print the Country line
	!
	TEXT$ = UTL_COUNTRY::COUNTRY + SPACE$(10%) + &
		UTL_COUNTRY::DESCR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Print out States (if possible)
	!
	GOSUB Print_States

	GOTO ExitProgram IF UTL_REPORTX::STAT

17200	!
	! Try for next country
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	GOTO ExitProgram

 Print_States:
17240	!***************************************************************
	!*  Subroutine for printing out States
	!***************************************************************

	WHEN ERROR IN
		FIND #UTL_STATE.CH%, &
			KEY #0% GE UTL_COUNTRY::COUNTRY, &
			REGARDLESS
	USE
		CONTINUE LastState
	END WHEN

 Get_Next_State:
17250
	!
	! Loop starts here
	!
	GOTO Laststate IF UTL_REPORTX::STAT

	!
	! Check unsolicited input
	!
	IF RRR_FLAG%
	THEN
		IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
		THEN
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitProgram
		END IF
	END IF

	!
	! Get next record (State)
	!
	WHEN ERROR IN
		GET #UTL_STATE.CH%, REGARDLESS
	USE
		CONTINUE LastState
	END WHEN

	!
	! Check current record (State)
	!
	GOTO Laststate IF UTL_STATE::COUNTRY <> UTL_COUNTRY::COUNTRY

	!
	! Print the State line
	!
	TEXT$ = SPACE$(13%) + &
		UTL_STATE::STATE + &
		SPACE$(9%) + &
		UTL_STATE::DESCR + &
		SPACE$(2%) + &
		UTL_STATE::FIPS

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO Laststate IF UTL_REPORTX::STAT

	!
	! Print out Counties (if possible)
	!
	GOSUB Print_Counties

	!
	! Try for next State
	!
	GOTO Get_Next_State

 LastState:
	RETURN

 Print_Counties:
17290	!***************************************************************
	!*  Sub-subroutine for printing out Counties
	!***************************************************************

	WHEN ERROR IN
		FIND #UTL_COUNTY.CH%, &
			KEY #0% GE UTL_COUNTRY::COUNTRY + &
			UTL_STATE::STATE, &
			REGARDLESS
	USE
		CONTINUE LastCounty
	END WHEN

 Get_Next_County:
17300	!
	! Loop starts here
	!
	GOTO LastCounty IF UTL_REPORTX::STAT

	!
	! Check unsolicited input
	!
	IF RRR_FLAG%
	THEN
		IF OUTP_UNSOLICITED(OPT_REPORT) <> CMC$_NORMAL
		THEN
			EXIT_STATUS = CMC$_UNTERROR
			GOTO ExitProgram
		END IF
	END IF

	!
	! Get next record (County)
	!
	WHEN ERROR IN
		GET #UTL_COUNTY.CH%, REGARDLESS
	USE
		CONTINUE LastCounty
	END WHEN

	!
	! Check current record (County)
	!
	GOTO LastCounty IF (UTL_STATE::COUNTRY <> UTL_COUNTY::COUNTRY) OR &
		(UTL_STATE::STATE <> UTL_COUNTY::STATE)

	!
	! Print the County line
	!
	TEXT$ = SPACE$(25%) + &
		UTL_COUNTY::COUNTY + &
		SPACE$(10%) + &
		UTL_COUNTY::DESCR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO LastCounty IF UTL_REPORTX::STAT

	!
	! Try for next County
	!
	GOTO Get_Next_County

 LastCounty:
	RETURN

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
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
	!+-+-+
	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field enters a wildcard value which
	!	indicates the countries which will be printed.
	!
	! Index:
	!	.x Wildcard
	!
	!--
