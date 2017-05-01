1	%TITLE "PR Tax Profile Report"
	%SBTTL "PR_RPRT_TAX_PROFILE"
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
	! ID:PR064
	!
	! Abstract:HELP
	!	.p
	!	The ^*Profile Table\* option
	!	prints a list of the
	!	Profile Table. This list contains the following fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Authorization
	!	.le
	!	Code
	!	.le
	!	Identification Number
	!	.le
	!	Withholding Account
	!	.le
	!	Minimum Wage
	!	.le
	!	Account Numbers for relevant accounts
	!	.els
	!
	! Index:
	!	.x Profile Table>Report
	!	.x Report>Profile Table
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TAX_PROFILE
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_TAX_PROFILE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TAX_PROFILE.OBJ;*
	!
	! Author:
	!
	!	12/07/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	08/12/89 - Kevin Handy
	!		Fixed problem where it was showing SUI
	!		percentage where it should have been showing
	!		minimum wage.
	!
	!	03/24/92 - Kevin Handy
	!		Modified to print SUTA account number.
	!
	!	06/14/93 - Kevin Handy
	!		Added REGARDLESS to find on PR_TAX_PROFILE.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/30/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/30/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP	(PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_S.HB"
	MAP	(PR_TAX_PROFILE_F)	PR_TAX_PROFILE_S_CDD	PR_TAX_PROFILE_S

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_C.HB"
	MAP	(PR_TAX_PROFILE_F)	PR_TAX_PROFILE_C_CDD	PR_TAX_PROFILE_C

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_E.HB"
	MAP	(PR_TAX_PROFILE_F)	PR_TAX_PROFILE_E_CDD	PR_TAX_PROFILE_E

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_D.HB"
	MAP	(PR_TAX_PROFILE_F)	PR_TAX_PROFILE_D_CDD	PR_TAX_PROFILE_D

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	K_NUM% = 0%

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"
	USE
		FILENAME$ = "PR_TAX_PROFILE_F"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "TAX PROFILE REPORT"
	TITLE$(2%) = ""
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE1$ = SPACE$(37%) + "FEDERAL"
	TITLE2$ = "     Auth          Code          " + &
		"Identification #              MinWage"

	TITLE3$ = "  WH Account         FICA Expense Acct  " + &
		"FICA Lia Employr   FICA Lia Employe " + &
		"  Cash Account       Accrual Account"

	TITLE4$ = "  FUI Expense Acct   FUI Lia Account    " + &
		"FUI   %    FUI Max "

	TITLE5$ = SPACE$(37%) + "STATE"

	TITLE6$ = "     Auth   Code   Identification#        " + &
			"WH Account           MinWage"

	TITLE7$ = "            SUI      Lia Account        " + &
		"Expense Account      Percent    Maximun  SUTA Account"

	TITLE8$ = "            OST      Lia Account        " + &
		"Expense Account      Percent    Maximun "


	TITLE9$ = SPACE$(40%) + "CITY"

	TITLE10$ = SPACE$(40%) + "SCHOOL"

	TITLE11$ = SPACE$(40%) + "COUNTY"

	TITLE12$ = "     Auth   Code   Identification #       " + &
		"WH Account"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		FIND  #PR_TAX_PROFILE.CH%, KEY #K_NUM% EQ "F  ", REGARDLESS
	USE
		FILENAME$ = "PR_TAX_PROFILE_F"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_TAX_PROFILE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	!GOTO ExitTotal IF (PR_TAX_PROFILE_F::AUTH+PR_TAX_PROFILE_F::CODE > &
	!	TO.ITEM$) AND TO.ITEM$ <> ""

	!
	! Print out one line
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE1$, 13%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE2$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = SPACE$(6%) + &
		PR_TAX_PROFILE_F::AUTH + SPACE$(13%) + &
		PR_TAX_PROFILE_F::CODE + SPACE$(11%) + &
		PR_TAX_PROFILE_F::REPNO + SPACE$(11%) + &
		FORMAT$(PR_TAX_PROFILE_F::MIN_WAGE, "##.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE3$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "  " + PR_TAX_PROFILE_F::WH_ACCT + " " + &
		PR_TAX_PROFILE_F::FICA_EX_ACCT + " " + &
		PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPR + " " + &
		PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPE + " " + &
		PR_TAX_PROFILE_F::CASH_ACCT + " " + &
		PR_TAX_PROFILE_F::PR_ACCRUAL_ACCT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE4$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "  " + PR_TAX_PROFILE_F::FUI_EX_ACCT + " " + &
		PR_TAX_PROFILE_F::FUI_LIA_ACCT + " " + &
		FORMAT$(PR_TAX_PROFILE_F::FUI_PCT, "##.###%") + " " + &
		FORMAT$(PR_TAX_PROFILE_F::FUI_MAX, "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 StateTop:
17040	!
	! State loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		FIND #PR_TAX_PROFILE.CH%, KEY #0% EQ "S", REGARDLESS
	USE
		CONTINUE CityTop
	END WHEN

	STATE_FLAG% = -1%

 StateNext:
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_TAX_PROFILE.CH%, REGARDLESS
	USE
		CONTINUE CityTop
	END WHEN

	GOTO CityTop IF PR_TAX_PROFILE_S::AUTH <> "S"

	IF STATE_FLAG%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE5$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
		STATE_FLAG% = 0%
	END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE6$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	TEXT$ = SPACE$(6%) + &
		PR_TAX_PROFILE_S::AUTH		+ "      " + &
		PR_TAX_PROFILE_S::CODE		+ "    " + &
		PR_TAX_PROFILE_S::REPNO		+ "   " + &
		PR_TAX_PROFILE_S::WH_ACCT	+ "    " + &
		FORMAT$(PR_TAX_PROFILE_S::MIN_WAGE, "##.##") + "   " + &
		PR_TAX_PROFILE_S::SUTANO
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE7$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = SPACE$(21%) + &
		PR_TAX_PROFILE_S::SUI_LIA_ACCT	+ " " + &
		PR_TAX_PROFILE_S::SUI_EX_ACCT	+ "   " + &
		FORMAT$(PR_TAX_PROFILE_S::SUI_PCT, "##.###%") + "   " + &
		FORMAT$(PR_TAX_PROFILE_S::SUI_MAX, "########")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE8$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = SPACE$(21%) + &
		PR_TAX_PROFILE_S::OST_LIA_ACCT	+ " " + &
		PR_TAX_PROFILE_S::OST_EX_ACCT	+ "   " + &
		FORMAT$(PR_TAX_PROFILE_S::OST_PCT, "##.###%") + "   " + &
		FORMAT$(PR_TAX_PROFILE_S::OST_MAX, "########")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO StateNext

 CityTop:
17080	!
	! City loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		FIND #PR_TAX_PROFILE.CH%, KEY #0% EQ "C", REGARDLESS
	USE
		CONTINUE SchoolTop
	END WHEN

	CITY_FLAG% = -1%

 CityNext:
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_TAX_PROFILE.CH%, REGARDLESS
	USE
		CONTINUE SchoolTop
	END WHEN

	GOTO SchoolTop IF PR_TAX_PROFILE_C::AUTH <> "C"

	IF CITY_FLAG%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE9$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE12$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
		CITY_FLAG% = 0%
	END IF

	TEXT$ = "      " + PR_TAX_PROFILE_C::AUTH + "      " + &
		PR_TAX_PROFILE_C::CODE + "    " + &
		PR_TAX_PROFILE_C::REPNO + "   " + &
		PR_TAX_PROFILE_C::CITY_LIA_ACCT
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO CityNext

 SchoolTop:
17120	!
	! School loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		FIND #PR_TAX_PROFILE.CH%, KEY #0% EQ "E", REGARDLESS
	USE
		CONTINUE CountyTop
	END WHEN

	SCHOOL_FLAG% = -1%

 SchoolNext:
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_TAX_PROFILE.CH%, REGARDLESS
	USE
		CONTINUE CountyTop
	END WHEN

	GOTO CountyTop IF PR_TAX_PROFILE_E::AUTH <> "E"

	IF SCHOOL_FLAG%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE10$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE12$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
		SCHOOL_FLAG% = 0%
	END IF

	TEXT$ = "      " + PR_TAX_PROFILE_E::AUTH + "      " + &
		PR_TAX_PROFILE_E::CODE + "    " + &
		PR_TAX_PROFILE_E::REPNO + "   " + &
		PR_TAX_PROFILE_E::SCH_LIA_ACCT
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO SchoolNext

 CountyTop:
17160	!
	! County loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		FIND #PR_TAX_PROFILE.CH%, KEY #0% EQ "D", REGARDLESS
	USE
		CONTINUE LongEnd
	END WHEN

	COUNTY_FLAG% = -1%

 CountyNext:
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_TAX_PROFILE.CH%, REGARDLESS
	USE
		CONTINUE LongEnd
	END WHEN

	GOTO LongEnd IF PR_TAX_PROFILE_D::AUTH <> "D"

	IF COUNTY_FLAG%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE11$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE12$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
		COUNTY_FLAG% = 0%
	END IF

	TEXT$ = "      " + PR_TAX_PROFILE_D::AUTH + "      " + &
		PR_TAX_PROFILE_D::CODE + "    " + &
		PR_TAX_PROFILE_D::REPNO + "   " + &
		PR_TAX_PROFILE_D::COU_LIA_ACCT
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO CountyNext

 LongEnd:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE15$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

 ExitTotal:
	!
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
