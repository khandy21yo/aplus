1	%TITLE "Product Account Table"
	%SBTTL "PD_RPRT_ACCOUNT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	! ID:PD007
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints out a list of the Product types
	!	account table. This list contains the following fields:
	!	.table 3,25
	!	.te
	!	Location
	!	.te
	!	Location Name
	!	.te
	!	Type
	!	.te
	!	Type Description
	!	.te
	!	Inv Number
	!	.te
	!	Inv Account Description
	!	.te
	!	Sale Account Number
	!	.te
	!	Sale Account Description
	!	.te
	!	COS Account Number
	!	.te
	!	COS Account Description
	!	.te
	!	Disc Account Number
	!	.te
	!	Disc Account Description
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Product Account Table>List
	!	.x List>Product Account Table
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PD_SOURCE:PD_RPRT_ACCOUNT/LINE
	!	$ LINK/EXE=PD_EXE: PD_RPRT_ACCOUNT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PD_RPRT_ACCOUNT.OBJ;*
	!
	! Author:
	!
	!	07/29/87 - Frank F. Starman
	!
	! Modification History:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Lose optdef 10 from utl_print (required sortby
	!		never looked at in this program) (ROBSON)
	!
	!	06/09/2000 - Kevin Handy
	!		Add in all the other accounts that were not
	!		being printed
	!
	!	11/01/2000 - Kevin Handy
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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	MAP	(PD_ACCOUNT)	PD_ACCOUNT_CDD		PD_ACCOUNT

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODTYPE.HB"
	MAP	(PD_PRODTYPE)	PD_PRODTYPE_CDD		PD_PRODTYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP	(UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP	(GL_CHART)	GL_CHART_CDD		GL_CHART

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
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field
	!	begins printing
	!	with a selected item.
	!	.b
	!	A blank field will cause the report to begin with
	!	the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Product Account Table
	!	.x Product Account Table>From Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters a selected
	!	item number with which the report will end printing.
	!	.b
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Product Account Table
	!	.x Product Account Table>To Item
	!	.x Item>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)
	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field prints
	!	selected items on the report by using the "wildcard"
	!	technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Product Account Table
	!	.x Product Account Table>Wildcard
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.OPN"
	USE
		FILENAME$ = "PD_ACCOUNT"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODTYPE.OPN"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "PD_PRODTYPE"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
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
	TITLE$(1%) = "PRODUCT  TYPE  ACCOUNT  LIST  "
	TITLE$(2%) = "Inventory Control System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Loc   Name                              " + &
		"       Type Description          InvAcc" + &
		"t#           Description"
	TITLE$(5%) = " "

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_ACCOUNT.CH%,KEY #0%
		ELSE
			FIND #PD_ACCOUNT.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
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
		GET #PD_ACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PD_ACCOUNT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (PD_ACCOUNT::PRODTYPE > TO_ITEM$) &
		AND TO_ITEM$ <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(PD_ACCOUNT::PRODTYPE, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

17200	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, &
			KEY #0% EQ PD_ACCOUNT::LOCATION, &
			REGARDLESS
	USE
		UTL_LOCATION::LOCNAME = &
			STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B)
		CONTINUE 17210 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

17210	WHEN ERROR IN
		GET #PD_PRODTYPE.CH%, KEY #0% EQ PD_ACCOUNT::PRODTYPE, REGARDLESS
	USE
		PD_PRODTYPE::DESCRIPTION = &
			STRING$(LEN(PD_PRODTYPE::DESCRIPTION), A"?"B)
		CONTINUE 17250 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PD_PRODTYPE"
		CONTINUE HelpError
	END WHEN

17250	WHEN ERROR IN
		GET #GL_CHART.CH%, KEY #0% EQ PD_ACCOUNT::INVACCT, REGARDLESS
		DESCR_INVACCT$ = GL_CHART::DESCR
	USE
		DESCR_INVACCT$ = &
			STRING$(LEN(GL_CHART::DESCR), A"?"B)
	END WHEN

17300	!
	! Print out one line
	!
	TEXT$ = PD_ACCOUNT::LOCATION + "  " + &
		UTL_LOCATION::LOCNAME + " " + &
		PD_ACCOUNT::PRODTYPE + "   " + &
		PD_PRODTYPE::DESCRIPTION + " " + &
		"Invent " + &
		PD_ACCOUNT::INVACCT + " " + &
		DESCR_INVACCT$

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 6%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF PD_ACCOUNT::WIPACCT <> ""
	THEN
		WHEN ERROR IN
			GET #GL_CHART.CH%, &
				KEY #0% EQ PD_ACCOUNT::WIPACCT, REGARDLESS
			DESCR_WIPACCT$ = GL_CHART::DESCR
		USE
			DESCR_WIPACCT$ = &
				STRING$(LEN(GL_CHART::DESCR), A"?"B)
		END WHEN

		TEXT$ = "    " + "  " + &
			"                              " + " " + &
			"  " + "   " + &
			"                              " + " " + &
			"WIP    " + &
			PD_ACCOUNT::WIPACCT + " " + &
			DESCR_WIPACCT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	IF PD_ACCOUNT::COSACCT <> ""
	THEN
		WHEN ERROR IN
			GET #GL_CHART.CH%, &
				KEY #0% EQ PD_ACCOUNT::COSACCT, &
				REGARDLESS
			DESCR_COSACCT$ = GL_CHART::DESCR
		USE
			DESCR_COSACCT$ = &
				STRING$(LEN(GL_CHART::DESCR), A"?"B)
		END WHEN

		TEXT$ = "    " + "  " + &
			"                              " + " " + &
			"  " + "   " + &
			"                              " + " " + &
			"COS    " + &
			PD_ACCOUNT::COSACCT + " " + &
			DESCR_COSACCT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	IF PD_ACCOUNT::DISCACCT <> ""
	THEN
		WHEN ERROR IN
			GET #GL_CHART.CH%, &
				KEY #0% EQ PD_ACCOUNT::DISCACCT, &
				REGARDLESS
			DESCR_DISCACCT$ = GL_CHART::DESCR
		USE
			DESCR_DISCACCT$ = &
				STRING$(LEN(GL_CHART::DESCR), A"?"B)
		END WHEN

		TEXT$ = "    " + "  " + &
			"                              " + " " + &
			"  " + "   " + &
			"                              " + " " + &
			"Discnt " + &
			PD_ACCOUNT::DISCACCT + " " + &
			DESCR_DISCACCT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	IF PD_ACCOUNT::MISCHACCT <> ""
	THEN
		WHEN ERROR IN
			GET #GL_CHART.CH%, &
				KEY #0% EQ PD_ACCOUNT::MISCHACCT, &
				REGARDLESS
			DESCR_MISCHACCT$ = GL_CHART::DESCR
		USE
			DESCR_MISCHACCT$ = &
				STRING$(LEN(GL_CHART::DESCR), A"?"B)
		END WHEN

		TEXT$ = "    " + "  " + &
			"                              " + " " + &
			"  " + "   " + &
			"                              " + " " + &
			"Misch  " + &
			PD_ACCOUNT::MISCHACCT + " " + &
			DESCR_MISCHACCT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	IF PD_ACCOUNT::PRICEVARACCT <> ""
	THEN
		WHEN ERROR IN
			GET #GL_CHART.CH%, &
				KEY #0% EQ PD_ACCOUNT::PRICEVARACCT, &
				REGARDLESS
			DESCR_PRICEVARACCT$ = GL_CHART::DESCR
		USE
			DESCR_PRICEVARACCT$ = &
				STRING$(LEN(GL_CHART::DESCR), A"?"B)
		END WHEN

		TEXT$ = "    " + "  " + &
			"                              " + " " + &
			"  " + "   " + &
			"                              " + " " + &
			"Varian " + &
			PD_ACCOUNT::PRICEVARACCT + " " + &
			DESCR_PRICEVARACCT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	IF PD_ACCOUNT::MISCH2ACCT <> ""
	THEN
		WHEN ERROR IN
			GET #GL_CHART.CH%, &
				KEY #0% EQ PD_ACCOUNT::MISCH2ACCT, &
				REGARDLESS
			DESCR_MISCH2ACCT$ = GL_CHART::DESCR
		USE
			DESCR_MISCH2ACCT$ = &
				STRING$(LEN(GL_CHART::DESCR), A"?"B)
		END WHEN

		TEXT$ = "    " + "  " + &
			"                              " + " " + &
			"  " + "   " + &
			"                              " + " " + &
			"Misch2 " + &
			PD_ACCOUNT::MISCH2ACCT + " " + &
			DESCR_MISCH2ACCT$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

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
