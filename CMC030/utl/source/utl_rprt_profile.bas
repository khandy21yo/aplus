1	%TITLE "Company Structure"
	%SBTTL "UTL_RPRT_PROFILE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:UT007
	!
	! Abstract:HELP
	!	.p
	!	The ^*Company Structure\* option accesses the report which lists
	!	the company profile information relative to locations, departments and work
	!	centers.
	!	.b
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	Location Code
	!	.le
	!	Location Name
	!	.le
	!	Location Address
	!	.le
	!	Department Description
	!	.le
	!	Department Supervisor
	!	.le
	!	Work Center Description
	!	.els
	!	.lm -10
	!
	! Index:
	!	.x Report>Company Structure
	!	.x Report>Company Profile
	!	.x Company Profile>Report>Company Structure information.
	!	.x Company Structure>Report
	!	.x Report>Company Structure
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_RPRT_PROFILE/LINE
	!	$ LINK/EXE=UTL_EXE: UTL_RPRT_PROFILE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_RPRT_PROFILE.OBJ;*
	!
	! AUTHOR:
	!
	!	01/18/88 - Lance Williams
	!
	! MODIFICATION HISTORY:
	!
	!	05/21/90 - Frank F. Starman
	!		Added COMMAND help message.
	!
	!	05/28/91 - Frank F. Starman
	!		Print supervisor for department
	!
	!	06/17/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	09/18/95 - Kevin Handy
	!		Lose extra include of CONSTANTS.INC
	!		Lose extra function definitions.
	!		Reformat source closer to 80 columns.
	!
	!	09/16/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/06/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/21/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	07/22/2003 - Kevin Handy
	!		Don't die when WORKCENTER file is missing.
	!--

	!++
	! Abstract:COMMAND
	!	^*PROFILE/STRUCTURE\*
	!	.p
	!	^*Profile/Structure\* prints the company profile
	!	information relative to locations, departments and work centers.
	!	.p
	!	^*Format: PROFILE/STRUCTURE\*
	!	.p
	!	^*Example:\*
	!	.literal
	!	Menu Command Level: /PROFILE/STRUCTURE
	!	.end literal
	!
	! Index:
	!	.x PROFILE/STRUCTURE
	!
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE"FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEPARTMENT.HB"
	MAP (UTL_DEPARTMENT) UTL_DEPARTMENT_CDD UTL_DEPARTMENT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_WORKCENTER.HB"
	MAP (UTL_WORKCENTER) UTL_WORKCENTER_CDD UTL_WORKCENTER

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION) UTL_LOCATION_CDD UTL_LOCATION

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION	OUTP_UNSOLICITED

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by (NU,RG,LG)\*
	!	.p
	!	The ^*Sort by\* field selects the order the
	!	report is to be printed in.
	!	.p
	!	NU Location Number
	!	RG Location Region
	!	LG Location Group
	!
	! Index:
	!	.x Location>Number
	!	.x Location>Region
	!	.x Location>Group
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Location\*
	!	.p
	!	The ^*From Location\* field enters
	!	a selected Location from which the report will begin.
	!	.p
	!	A blank location will cause the report to start with the first
	!	Location in the file.
	!
	! Index:
	!	.x From Location
	!	.x Location>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Location\*
	!	.p
	!	The ^*To Location\* field ends
	!	the printing with a selected location.
	!	.p
	!	A blank location will cause the report to end with the last
	!	location in the file.
	!
	! Index:
	!	.x To Location
	!	.x Location>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects designated
	!	location codes to be printed by entering a "Wildcard" value.
	!
	! Index:
	!	.x Wildcard
	!
	! Required:
	!--

	SELECT SORT_BY$
	CASE "NU"
		K_NUM% = 0%
	CASE "RG"
		K_NUM% = 1%
	CASE "LG"
		K_NUM% = 2%
	END SELECT

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_STOREADD"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Department file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEPARTMENT.OPN"
	USE
		FILENAME$ = "UTL_DEPARTMENT"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Work center
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_WORKCENTER.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "UTL_WORKCENTER"
		CONTINUE HelpError
	END WHEN

330	!

    ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "COMPANY  PROFILE  REPORT"
	TITLE$(2%) = " Utility system"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		 1234567890123456789012345678901234567890

	TITLE$(4%) = "Loc  Rg LG Location Name                           " + &
		" MailingAddress                         " + &
		" City            ST ZipCode    Phone"
	TITLE$(5%) = "                                                   " + &
		" ShippingAddress"
	TITLE$(6%) = "        Department Description          " + &
		"                     Phone         Supervisor"
	TITLE$(7%) = "                    WorkCenter Description" + &
		"                              Phone"
	TITLE$(8%) = "."

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #UTL_LOCATION.CH%, KEY #K_NUM%
		ELSE
			FIND #UTL_LOCATION.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find beginning record!", 0%)
		CONTINUE ExitProgram
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!

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
	! Get next record
	!
	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$
	CASE "NU"
		GOTO ExitTotal IF (UTL_LOCATION::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(UTL_LOCATION::LOCATION, -1%), &
			WLDCRD$) = 0%

	CASE "RG"
		GOTO ExitTotal IF (UTL_LOCATION::REGION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(UTL_LOCATION::REGION, -1%), &
			WLDCRD$) = 0%

	CASE "LG"
		GOTO ExitTotal IF (UTL_LOCATION::LOCGROUP > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(UTL_LOCATION::LOCGROUP, -1%), &
			WLDCRD$) = 0%

	END SELECT

	!
	! Print out Store
	!
	TEXT$ = UTL_LOCATION::LOCATION + " " + &
		UTL_LOCATION::REGION + " " + &
		UTL_LOCATION::LOCGROUP + " " + &
		UTL_LOCATION::LOCNAME + " " + &
		LEFT(TRM$(UTL_LOCATION::ADDRESS1) + " " + &
		TRM$(UTL_LOCATION::ADDRESS2) + SPACE$(39%), 39%) + " " + &
		UTL_LOCATION::CITY + " " + &
		UTL_LOCATION::STATE + " " + &
		UTL_LOCATION::ZIP + " " + &
		UTL_LOCATION::PHONE

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = SPACE$(52%) + &
		LEFT(TRM$(UTL_LOCATION::SHPADDRESS1) + " " + &
		TRM$(UTL_LOCATION::SHPADDRESS2) + SPACE$(39%), 39%) + " " + &
		UTL_LOCATION::SHPCITY + " " + &
		UTL_LOCATION::SHPSTATE + " " + &
		UTL_LOCATION::SHPZIP + " " + &
		UTL_LOCATION::SHPPHONE

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

17100	!
	! Try to find department
	!
	WHEN ERROR IN
		FIND #UTL_DEPARTMENT.CH%, &
			KEY #0% EQ UTL_LOCATION::LOCATION, &
			REGARDLESS
	USE
		CONTINUE 17350 IF ERR = 11% OR ERR = 155%
		FILENAME$ = "UTL_DEPARTMENT"
		CONTINUE HelpError
	END WHEN

17120	!
	! Get the next department
	!
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

	WHEN ERROR IN
		GET #UTL_DEPARTMENT.CH%, REGARDLESS
	USE
		CONTINUE 17350 IF ERR = 11% OR ERR = 155%
		FILENAME$ = "UTL_DEPARTMENT"
		CONTINUE HelpError
	END WHEN

	GOTO 17350 IF UTL_DEPARTMENT::LOCATION <> UTL_LOCATION::LOCATION

	TEXT$ = "        " + &
		UTL_DEPARTMENT::DEPT_NUM + "      " + &
		UTL_DEPARTMENT::DESCRIPTION + " " + &
		UTL_DEPARTMENT::PHONE + " " + &
		UTL_DEPARTMENT::SUPERVISOR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Try to find workcenter
	!
17200	WHEN ERROR IN
		FIND #UTL_WORKCENTER.CH%, &
			KEY #0% EQ UTL_LOCATION::LOCATION + &
			UTL_DEPARTMENT::DEPT_NUM, &
			REGARDLESS
	USE
		CONTINUE 17120 IF ERR = 11% OR ERR = 155% OR ERR = 9%
		FILENAME$ = "UTL_WORDCENTER"
		CONTINUE HelpError
	END WHEN

17220	!
	! Get next work center
	!
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

	WHEN ERROR IN
		GET #UTL_WORKCENTER.CH%, REGARDLESS
	USE
		CONTINUE 17120 IF ERR = 11% OR ERR = 155%
		FILENAME$ = "UTL_WORDCENTER"
		CONTINUE HelpError
	END WHEN

	GOTO 17120 &
		IF UTL_LOCATION::LOCATION <> UTL_WORKCENTER::LOCATION OR &
		UTL_DEPARTMENT::DEPT_NUM <> UTL_WORKCENTER::DEPT_NUM

	TEXT$ = SPACE$(20%)  + &
		UTL_WORKCENTER::WORK_CENTER + "       " + &
		UTL_WORKCENTER::DESCRIPTION + " " + &
		UTL_WORKCENTER::PHONE

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO 17220

17350	!
	! Print white space and get next store
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	GOTO 17020

 ExitTotal:
17400	!
	! Handle end of report
	!

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
