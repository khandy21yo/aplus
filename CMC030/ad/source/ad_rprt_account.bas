1	%TITLE "Asset Account Table"
	%SBTTL "AD_RPRT_ACCOUNT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986 BY
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
	! ID:AD015
	!
	! Abstract:HELP
	!	.B
	!	.LM +5
	!	The ^*Asset Accounts\* option causes the printing
	!	of the Asset Account Table. This print out contains the following fields:
	!	.table 3,25
	!	.te
	!	Location
	!	.te
	!	Name
	!	.te
	!	Type
	!	.te
	!	Description
	!	.te
	!	Account
	!	.te
	!	Description
	!	.end table
	!	.LM -5
	!
	! Index:
	!	.x Asset Accounts>Report
	!	.x Report>Asset Accounts
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_ACCOUNT/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_ACCOUNT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_ACCOUNT.OBJ;*
	!
	! AUTHOR:
	!
	!	12/07/87 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	08/28/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/19/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/15/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/27/2000 - Kevin Handy
	!		Use A"x"B
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

	%INCLUDE "SOURCE:[AD.OPEN]AD_ACCOUNT.HB"
	MAP (AD_ACCOUNT) AD_ACCOUNT_CDD AD_ACCOUNT

	%INCLUDE "SOURCE:[AD.OPEN]AD_ASSTYPE.HB"
	MAP (AD_ASSTYPE)	AD_ASSTYPE_CDD	AD_ASSTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)	GL_CHART_CDD	GL_CHART

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
	!	^*(01) From Location\*
	!	.B
	!	.LM +5
	!	The ^*From Location\* field causes the
	!	printing to begin with the selected location.
	!	.B
	!	A blank field will cause the report to start with the first
	!	location record in the file.
	!	.LM -5
	!
	! Index:
	!	.x From Location>Asset Accounts
	!	.x Asset Accounts>From Location
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Location\*
	!	.b
	!	.lm +5
	!	The ^*To Location\* field causes the
	!	printing to end with the selected location.
	!	.b
	!	A blank field will cause the report to end with the last
	!	location record in file.
	!	.lm -5
	!
	! Index:
	!	.x To Location>Asset Accounts
	!	.x Asset Accounts>To Location
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* setting enables the user to print a
	!	report including selected Locations only using the
	!	wildcarding technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Asset Account Table
	!	.x Asset Account Table>Wildcard
	!
	!--


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_ACCOUNT.OPN"
	USE
		FILENAME$ = "AD_ACCOUNT"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_ASSTYPE.OPN"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "AD_ASSTYPE"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelPError
	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "ASSET  ACCOUNTS  TABLE"
	TITLE$(2%) = "Asset depreciation system"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		 123456789012345678901234567890123456789
	TITLE$(4%) = "Loc  Name                              " + &
		"       Type Description          Account" + &
		"            Description"
	TITLE$(5%) = "."

	LYT_LINE$ = "$Loc:005,$Name:046,$Type:051,$Description:072," + &
			"$Account:091,$Description:103"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AD_ACCOUNT.CH%
		ELSE
			FIND #AD_ACCOUNT.CH%, &
				KEY #0% GE FROM_ITEM$, &
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
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AD_ACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AD_ACCOUNT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (AD_ACCOUNT::LOCATION > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(AD_ACCOUNT::LOCATION, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	GOSUB 18500

17300	!
	! Print out one line
	!
	ACCOUNT$ = AD_ACCOUNT::ASS_ACCT
	GOSUB 18550

	TEXT$ = AD_ACCOUNT::LOCATION + " " + &
		UTL_LOCATION::LOCNAME + " " + &
		AD_ACCOUNT::ASSET_TYPE + "   " + &
		AD_ASSTYPE::DESCRIPTION + " " + &
		AD_ACCOUNT::ASS_ACCT + " " + &
		GL_CHART::DESCR

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	ACCOUNT$ = AD_ACCOUNT::DEP_ACCT
	GOSUB 18550

	TEXT$ = AD_ACCOUNT::LOCATION + &
		SPACE$(68%) + &
		AD_ACCOUNT::DEP_ACCT + " " + &
		GL_CHART::DESCR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	ACCOUNT$ = AD_ACCOUNT::EXP_ACCT
	GOSUB 18550

	TEXT$ = AD_ACCOUNT::LOCATION+ &
		SPACE$(68%) + &
		AD_ACCOUNT::EXP_ACCT + " " + &
		GL_CHART::DESCR

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO GetNextRec

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

18500	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, &
			KEY #0% EQ AD_ACCOUNT::LOCATION, &
			REGARDLESS
	USE
		UTL_LOCATION::LOCNAME = &
			STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B)
		CONTINUE 18510 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

18510	WHEN ERROR IN
		GET #AD_ASSTYPE.CH%, &
			KEY #0% EQ AD_ACCOUNT::ASSET_TYPE, &
			REGARDLESS
	USE
		AD_ASSTYPE::DESCRIPTION = &
			STRING$(LEN(AD_ASSTYPE::DESCRIPTION), A"?"B)
		CONTINUE AssetType IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AD_ASSTYPE"
		CONTINUE HelpError
	END WHEN

 AssetType:
	RETURN

18550	WHEN ERROR IN
		GET #GL_CHART.CH%, KEY #0% EQ ACCOUNT$, REGARDLESS
	USE
		GL_CHART::DESCR = &
			STRING$(LEN(GL_CHART::DESCR), A"?"B)
		CONTINUE Account IF ERR = 155% OR ERR = 9%
		FILENAME$ = "GL_CHART"
		CONTINUE HelpError
	END WHEN

 Account:
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
	FILENAME$ = ""
	RESUME HelpError

32767	END
