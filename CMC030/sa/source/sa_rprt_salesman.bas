1	%TITLE "Salesman's Address Report"
	%SBTTL "SA_RPRT_SALESMAN"
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
	! ID:SA0003
	!
	! Abstract:HELP
	!	.p
	!	The ^*Salesman's Address Report\* option prints a list of
	!	the Salesman's address master file.  The following information will appear.
	!	.b
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	Salesman Code
	!	.le
	!	Salesman Name
	!	.le
	!	Salesman Type
	!	.le
	!	Salesman Class
	!	.le
	!	Start Date
	!	.le
	!	Status Code
	!	.le
	!	Termination Date
	!	.le
	!	Address 1
	!	.le
	!	Address 2
	!	.le
	!	City
	!	.le
	!	State
	!	.le
	!	Zip Code
	!	.le
	!	Country
	!	.le
	!	Telephone
	!	.le
	!	Initials
	!	.le
	!	Region
	!	.le
	!	Commission %
	!	.els
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_RPRT_SALESMAN/LINE
	!	$ LINK/EXECUTABLE=SA_EXE: SA_RPRT_SALESMAN, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_SALESMAN.OBJ;*
	!
	! Author:
	!
	! Modification history:
	!
	!	08/16/90 - Craig Tanner
	!		Cleaned up error trapping
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/07/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/20/97 - Kevin Handy
	!		Don't allocate channel for report
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excess %PAGE's
	!
	!	12/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!******************************************************************
	! External modules needed
	!******************************************************************

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs)
	!******************************************************************

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Initialization:
	!******************************************************************
	! Get ready to begin
	!******************************************************************

	!
	! Initialize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by (S,C,T)\*
	!	.p
	!	The ^*Sort by\* field determines the order
	!	to print in.
	!	.p
	!	Valid settings are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	S - Salesman
	!	.le
	!	C - Class
	!	.le
	!	T - Type
	!	.els
	!	.lm -10
	!	.p
	!	A setting is required in this field.  No other settings are
	!	valid.
	!
	! Index:
	!	.x Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02)From Order\*
	!	.p
	!	The ^*From Order\* field enters the
	!	order with which to begin with.
	!	.p
	!	A blank field causes the report to begin with the first
	!	order in the file.
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Order\*
	!	.p
	!	The ^*To Order\* field specifies the order
	!	with which to end with.
	!	.p
	!	A blank field causes the report to end with the last
	!	order in the file.
	!
	! Index:
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	using the Wildcarding Technique.
	!
	! Index:
	!
	!--

300	!
	! Open Subaccount file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(2%) = "Sales Analysis System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Salesman   Name                     " + &
		"Type    Class StartDate   Status  TermDate   " + &
		"Address 1                 Address 2"
	TITLE$(5%) = "             City            State ZIP        " + &
		"Country  Phone      Initials Region    Comm%"
	TITLE$(6%) = "."

	SELECT SORTBY$
	CASE "S"
		K_NUM% = 0%
		TITLE$(1%) = "SALESMAN MASTER FILE REPORT BY SALESMAN"
	CASE "C"
		K_NUM% = 2%
		TITLE$(1%) = "SALESMAN MASTER FILE REPORT BY CLASS"
	CASE "T"
		K_NUM% = 1%
		TITLE$(1%) = "SALESMAN MASTER FILE REPORT BY TYPE"
	END SELECT

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #SB_SUBACCOUNT.CH%, KEY #K_NUM%
		ELSE
			FIND #SB_SUBACCOUNT.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17100	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetNextRec IF SA_SALESMAN::SUBJECT <> "S"

	SELECT SORTBY$
	CASE "C"
		GOTO ExitTotal IF (SA_SALESMAN::CLASS > TO_ITEM$) AND &
			TO_ITEM$ <> ""
		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(SA_SALESMAN::CLASS, -1%), &
			WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "S"

		GOTO ExitTotal &
			IF (SA_SALESMAN::SALESMAN > TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(SA_SALESMAN::SALESMAN, -1%), &
			WLDCRD$) = 0% AND WLDCRD$ <> ""

	CASE "T"

		GOTO ExitTotal &
			IF (SA_SALESMAN::TTYPE > TO_ITEM$) AND TO_ITEM$ <> ""

		GOTO GetNextRec &
			IF COMP_STRING(EDIT$(SA_SALESMAN::TTYPE, -1%), &
			WLDCRD$) = 0% AND WLDCRD$ <> ""

	END SELECT

	!
	! Print out one line
	!
	TEXT$ =  SA_SALESMAN::SALESMAN + " " + &
		LEFT$(SA_SALESMAN::DESCR, 24%) + " " + &
		SA_SALESMAN::TTYPE + "      " + &
		SA_SALESMAN::CLASS + "  " + &
		PRNT_DATE(SA_SALESMAN::BDATE, 8%) + "  " + &
		SA_SALESMAN::SSTATUS + "        " + &
		PRNT_DATE(SA_SALESMAN::EDATE, 8%) + " " + &
		SA_SALESMAN::ADD1 + " " + &
		SA_SALESMAN::ADD2


	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ =  "             " + SA_SALESMAN::CITY + " " + &
		SA_SALESMAN::STATE + "    " + &
		SA_SALESMAN::ZIP + " " + &
		SA_SALESMAN::COUNTRY + "       " + &
		SA_SALESMAN::PHONE + " " + &
		SA_SALESMAN::INITIALS + "      " + &
		SA_SALESMAN::REGION + " " + &
		FORMAT$(SA_SALESMAN::COMMPER, "#,###,###.##")


	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Print out totals
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
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report SA_RPRT_SALESMAN
	!******************************************************************
	END
