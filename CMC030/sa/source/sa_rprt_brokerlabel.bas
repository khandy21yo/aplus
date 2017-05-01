1	%TITLE "Sales Analysis Broker Label Writer"
	%SBTTL "SA_RPRT_BROKERLABEL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! ID:SA0007
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Name Address Labels\* routine prints
	!	labels for customers.
	!	.b
	!	The print setting screen provides fields in which entries may be
	!	made to print only a partial listing of the file and also provides
	!	a setting in which the labels can be printed in customer number order,
	!	customer name order, or alphabetical order.
	!	.lm -5
	!
	! Index:
	!	.x Labels>Customer
	!	.x Customer>Labels
	!	.x Print>Customer Labels
	!
	! Option:
	!
	! Author:
	!
	!	12/23/91 - Dan Perkins
	!
	! Compile:
	!
	!	$ BAS SA_SOURCE:SA_RPRT_BROKERLABEL/LINE
	!	$ LINK/EXECUTABLE=SA_EXE:*.EXE SA_RPRT_BROKERLABEL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SA_RPRT_BROKERLABEL.OBJ;*
	!
	! Modification history:
	!
	!	02/04/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/07/96 - Kevin Handy
	!		Reformat source code.
	!
	!	09/13/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/21/2000 - Kevin Handy
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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP (UTL_COUNTRY)	UTL_COUNTRY_CDD		UTL_COUNTRY

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field
	!	begins printing with the selected Item _#.
	!	The value must be in agreement with field
	!	(03) Sort by.
	!	.b
	!	A blank setting will cause the report to begin with the first
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Broker Labels
	!	.x Broker>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* value
	!	ends printing with the selected Item _#. The value must be
	!	in agreement with field (03) Sort by.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Broker Labels
	!	.x Broker Labels>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*S\* - Salesman
	!	.te
	!	^*C\* - Class
	!	.te
	!	^*T\* - Type
	!	.end table
	!	A setting is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort by>Broker Labels
	!	.x Broker Labels>Sort by
	!
	!--

	SELECT SORTBY$
	CASE "S"
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(SA_SALESMAN::SALESMAN))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(SA_SALESMAN::SALESMAN))

	CASE "T"
		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(SA_SALESMAN::TTYPE))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(SA_SALESMAN::TTYPE))

	CASE "C"
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(SA_SALESMAN::CLASS))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(SA_SALESMAN::CLASS))

	END SELECT

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	USE
		FILENAME$ = "SA_SALESMAN"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.OPN"
	USE
		CONTINUE 17000 IF ERR = 5%
		FILENAME$ = "UTL_COUNTRY"
		CONTINUE HelpError
	END WHEN

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
		FILENAME$ = "SA_SALESMAN"
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
		GET #SB_SUBACCOUNT.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "SA_SALESMAN"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec IF SA_SALESMAN::SUBJECT <> "S"

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "S"
		GOTO ExitTotal IF (SA_SALESMAN::SALESMAN > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "T"
		GOTO ExitTotal IF (SA_SALESMAN::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "C"
		GOTO ExitTotal IF (SA_SALESMAN::CLASS > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	END SELECT

17100	WHEN ERROR IN
		GET #UTL_COUNTRY.CH%, &
			KEY #0% EQ SA_SALESMAN::COUNTRY, &
			REGARDLESS
	USE
		IF ERR = 155% OR ERR = 9%
		THEN
			UTL_COUNTRY::DESCR = ""
			CONTINUE 17110
		END IF
		FILENAME$ = "UTL_COUNTRY"
		CONTINUE HelpError
	END WHEN

17110	IF SA_SALESMAN::COUNTRY = "US" OR SA_SALESMAN::COUNTRY = ""
	THEN
		UTL_COUNTRY::DESCR = ""

	!
	! Print out one line
	!
	IF UTL_REPORTX::PRINTTO <> 1%
	THEN
		L_COUNT% = 0%
		CALL OUTP_LINENOTITLE("", UTL_REPORTX, SA_SALESMAN::DESCR, 0%)
		L_COUNT% = L_COUNT% + 1%
		GOTO 17340 IF L_COUNT% >= UTL_REPORTX::PAGELEN

		TEXT$ = SA_SALESMAN::ADD1

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEXT$, 0%)
		L_COUNT% = L_COUNT% + 1%
		GOTO 17340 IF L_COUNT% >= UTL_REPORTX::PAGELEN

		TEXT$ = SA_SALESMAN::ADD2

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEXT$, 0%)
		L_COUNT% = L_COUNT% + 1%
		GOTO 17340 IF L_COUNT% >= UTL_REPORTX::PAGELEN

		IF UTL_COUNTRY::DESCR = ""
		THEN
			TEXT$ = SA_SALESMAN::CITY + "  " + &
				SA_SALESMAN::STATE + "  " + &
				SA_SALESMAN::ZIP
		ELSE
			TEXT$ = SA_SALESMAN::CITY + "  " + UTL_COUNTRY::DESCR
		END IF


		CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEXT$, 0%)
		L_COUNT% = L_COUNT% + 1%
		GOTO 17340 IF L_COUNT% >= UTL_REPORTX::PAGELEN

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, SPACE$(28%) + "(" + &
			SA_SALESMAN::SALESMAN + ")", 0%)
		L_COUNT% = L_COUNT% + 1%
		GOTO 17340 IF L_COUNT% >= UTL_REPORTX::PAGELEN

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
			FOR I% = L_COUNT% + 1% TO UTL_REPORTX::PAGELEN
	ELSE
		CALL OUTP_LINENOTITLE("", UTL_REPORTX, SA_SALESMAN::DESCR, 0%)

		TEXT$ = SA_SALESMAN::ADD1

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEXT$, 0%)

		TEXT$ = SA_SALESMAN::ADD2

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEXT$, 0%)

		IF UTL_COUNTRY::DESCR = ""
		THEN
			TEXT$ = SA_SALESMAN::CITY + "  " + &
				SA_SALESMAN::STATE + "  " + &
				SA_SALESMAN::ZIP
		ELSE
			TEXT$ = SA_SALESMAN::CITY + "  " + UTL_COUNTRY::DESCR
		END IF

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEXT$, 0%)


		CALL OUTP_LINENOTITLE("", UTL_REPORTX, SPACE$(28%) + "(" + &
			SA_SALESMAN::SALESMAN + ")", 0%)

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%)
	END IF

17340	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!

 ExitProgram:
	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

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
