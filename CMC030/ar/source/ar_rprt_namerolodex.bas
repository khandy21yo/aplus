1	%TITLE "Accounts Receivable Vendor Rolodex"
	%SBTTL "AR_RPRT_NAMEROLODEX"
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
	! ID:AR054
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Name Address Rolodex\* routine prints
	!	Rolodex cards for customers.
	!	.b
	!	The print setting screen provides fields in which entries may be
	!	made to print only a partial listing of the file and also provides
	!	a field in which the cards can be printed in customer number order,
	!	customer name order, or alphabetical order.
	!	.lm -5
	!
	! Index:
	!	.x Customer>Rolodex Cards
	!	.x Rolodex Cards>Customer
	!	.x Print>Rolodex Cards
	!
	! Option:
	!
	!
	! Author:
	!
	!	03/11/88 - Aaron Redd
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_NAMEROLODEX/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_RPRT_NAMEROLODEX, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_NAMEROLODEX.OBJ;*
	!
	! Modification history:
	!
	!	06/15/93 - Kevin Handy
	!		Added REGARDLESS to UTL_COUNTRY.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	07/14/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/26/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

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
	!	The ^*From Item\* field causes the printing
	!	to begin with the selected Item _#.
	!	The value must be in agreement with field
	!	(03) Sort by.
	!	.b
	!	A blank setting causes the report to begin with the first
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Name/Address Rolodex
	!	.x Name/Address Rolodex>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the printing
	!	to end with a particular Item _#.
	!	The value must be in agreement with field
	!	(03) Sort by.
	!	.b
	!	A blank setting causes the report to end with the last
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Name/Address Rolodex
	!	.x Name/Address Rolodex>To Item
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
	!	.te
	!	.table 3,25
	!	^*N\* - Number
	!	.te
	!	^*T\* - Type
	!	.te
	!	^*C\* - Category
	!	.te
	!	^*A\* - Alphabetical
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort by>Name/Address Rolodex
	!	.x Name/Address Rolodex>Sort by
	!
	!--

	SELECT SORTBY$
	CASE "N"
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::CUSNUM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::CUSNUM))

	CASE "T"
		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::TTYPE))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::TTYPE))

	CASE "C"
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::CATEGORY))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::CATEGORY))

	CASE "A"
		K_NUM% = 3%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::ALPSRT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::ALPSRT))

	END SELECT

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.OPN"
	USE
		FILENAME$ = "UTL_COUNTRY"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_35CUSTOM.CH%, KEY #K_NUM%
		ELSE
			FIND #AR_35CUSTOM.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "AR_35CUSTOM"
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
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "N"
		GOTO ExitTotal IF (AR_35CUSTOM::CUSNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "T"
		GOTO ExitTotal IF (AR_35CUSTOM::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "C"
		GOTO ExitTotal IF (AR_35CUSTOM::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "A"
		GOTO ExitTotal IF (AR_35CUSTOM::ALPSRT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

17100	WHEN ERROR IN
		GET #UTL_COUNTRY.CH%, &
			KEY #0% EQ AR_35CUSTOM::COUNTRY, &
			REGARDLESS
	USE
		UTL_COUNTRY::DESCR = ""
		CONTINUE 17110
	END WHEN

17110	IF AR_35CUSTOM::COUNTRY = "US" OR AR_35CUSTOM::COUNTRY = ""
	THEN
		UTL_COUNTRY::DESCR = ""
	END IF

	!
	! Print out one line
	!
	L_COUNT% = 0%

	IF UTL_REPORTX::PRINTTO <> 1%
	THEN
		CALL OUTP_LINENOTITLE("", UTL_REPORTX, AR_35CUSTOM::CUSNUM, 0%)
		L_COUNT% = L_COUNT% + 1%
		GOTO 17340 IF L_COUNT% >= UTL_REPORTX::PAGELEN

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, AR_35CUSTOM::CUSNAM, 0%)
		L_COUNT% = L_COUNT% + 1%
		GOTO 17340 IF L_COUNT% >= UTL_REPORTX::PAGELEN

		TEMP$ = AR_35CUSTOM::ADD1

		IF TEMP$ <> ""
		THEN
			CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEMP$, 0%)
			L_COUNT% = L_COUNT% + 1%
			GOTO 17340 IF L_COUNT% >= UTL_REPORTX::PAGELEN
		END IF

		TEMP$ = AR_35CUSTOM::ADD2

		IF TEMP$ <> ""
		THEN
			CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEMP$, 0%)
			L_COUNT% = L_COUNT% + 1%
			GOTO 17340 IF L_COUNT% >= UTL_REPORTX::PAGELEN
		END IF

		TEMP$ = AR_35CUSTOM::ADD3

		IF TEMP$ <> ""
		THEN
			CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEMP$, 0%)
			L_COUNT% = L_COUNT% + 1%
			GOTO 17340 IF L_COUNT% >= UTL_REPORTX::PAGELEN
		END IF

		IF UTL_COUNTRY::DESCR = ""
		THEN
			TEMP$ = AR_35CUSTOM::CITY + "  " + &
				AR_35CUSTOM::STATE + "  " + &
				AR_35CUSTOM::ZIP
		ELSE
			TEMP$ = AR_35CUSTOM::CITY + "  " + UTL_COUNTRY::DESCR
		END IF

		IF TEMP$ <> ""
		THEN
			CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEMP$, 0%)
			L_COUNT% = L_COUNT% + 1%
			GOTO 17340 IF L_COUNT% >= UTL_REPORTX::PAGELEN
		END IF

		TEMP$ = AR_35CUSTOM::PHONE

		IF TEMP$ <> ""
		THEN
			CALL OUTP_LINENOTITLE("", UTL_REPORTX, &
				PRNT_PHONE(TEMP$, 0%), 0%)
			L_COUNT% = L_COUNT% + 1%
			GOTO 17340 IF L_COUNT% >= UTL_REPORTX::PAGELEN
		END IF

		IF AR_35CUSTOM::ALPSRT <> ""
		THEN
			CALL OUTP_LINENOTITLE("", UTL_REPORTX, &
				AR_35CUSTOM::ALPSRT, 0%)
			L_COUNT% = L_COUNT% + 1%
			GOTO 17340 IF L_COUNT% >= UTL_REPORTX::PAGELEN
		END IF

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
			FOR I% = L_COUNT% + 1% TO UTL_REPORTX::PAGELEN
	ELSE
		CALL OUTP_LINENOTITLE("", UTL_REPORTX, AR_35CUSTOM::CUSNUM, 0%)
		CALL OUTP_LINENOTITLE("", UTL_REPORTX, AR_35CUSTOM::CUSNAM, 0%)

		TEMP$ = AR_35CUSTOM::ADD1

		IF TEMP$ <> ""
		THEN
			CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEMP$, 0%)
		END IF

		TEMP$ = AR_35CUSTOM::ADD2

		IF TEMP$ <> ""
		THEN
			CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEMP$, 0%)
		END IF

		TEMP$ = AR_35CUSTOM::ADD3

		IF TEMP$ <> ""
		THEN
			CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEMP$, 0%)
		END IF

		IF UTL_COUNTRY::DESCR = ""
		THEN
			TEMP$ = AR_35CUSTOM::CITY + " " + AR_35CUSTOM::STATE + "  " + &
				AR_35CUSTOM::ZIP
		ELSE
			TEMP$ = AR_35CUSTOM::CITY + " " + UTL_COUNTRY::DESCR
		END IF
		IF TEMP$ <> ""
		THEN
			CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEMP$, 0%)
		END IF

		TEMP$ = AR_35CUSTOM::PHONE

		IF TEMP$ <> ""
		THEN
			CALL OUTP_LINENOTITLE("", UTL_REPORTX, &
				PRNT_PHONE(TEMP$, 0%), 0%)
		END IF

		IF AR_35CUSTOM::ALPSRT <> ""
		THEN
			CALL OUTP_LINENOTITLE("", UTL_REPORTX, AR_35CUSTOM::ALPSRT, 0%)
		END IF

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
