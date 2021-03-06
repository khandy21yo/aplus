1	%TITLE "AR Summary of Payments"
	%SBTTL "AR_RPRT_SUMPAY"
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
	! ID:AR0151
	!
	! Abstract:HELP
	!	.p
	!
	! Index:
	!	.x Report>Summary of Payments
	!	.x Summary of Payments>Report
	!
	! Option:
	!
	!
	! Input:
	!
	!
	! Output:
	!
	!
	! Example:
	!
	!
	! Author:
	!
	!	05/10/91 - Craig P. Tanner
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_SUMPAY
	!	$ LINK/EXE=AR_EXE:*.EXE AR_RPRT_SUMPAY, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_SUMPAY.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/30/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!		Use "WHEN ERROR IN"
	!
	!	10/30/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN_DIST.HB"
	MAP	(AR_OPEN_DIST)	AR_OPEN_DIST_CDD	AR_OPEN_DIST

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP	(AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM

	MAP (AR_TEMP) &
		AR_TEMP.CLIENT$ = 10%, &
		AR_TEMP.DATE$ = 8%, &
		AR_TEMP.MATTER$ = 10%, &
		AR_TEMP.FAMOUNT, &
		AR_TEMP.CAMOUNT, &
		AR_TEMP.EAMOUNT

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	PERIOD$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Period\*
	!	.b
	!	.lm +5
	!	The ^*Period\* field in the Detail Payments Report Setting
	!	Screen enters an accounting period for which
	!	the report will print.
	!	.b
	!	The format for entry is YYYYPP.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Period>Report>Detail Payments
	!	.x Report>Period>Detail Payments
	!
	!--


	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN_DIST.OPN"
	USE
		FILENAME$ = "AR_OPEN_DIST"
		CONTINUE HelpError
	END WHEN

330	!
	! Open file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		CONTINUE 350 IF ERR =  5%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

350	CALL ENTR_3MESSAGE(SCOPE, &
		"Creating temporary file.  Reading work files.", 1%)

	CALL ASSG_CHANNEL(AR_TEMP.CH%, STAT%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "AR_TEMP.TMP" FOR OUTPUT AS FILE #AR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP AR_TEMP, &
			PRIMARY KEY (AR_TEMP.DATE$, &
				AR_TEMP.CLIENT$, AR_TEMP.MATTER$ &
				) DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "AR_TEMP"
		CONTINUE HelpError
	END WHEN

400	!*******************************************************************
	! Start putting stuff into AR_TEMP file
	!*******************************************************************

	WHEN ERROR IN
		FIND #AR_OPEN_DIST.CH%, KEY #1% GE PERIOD$, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "AR_OPEN_DIST"
		CONTINUE HelpError
	END WHEN

410
 GetFromHistory:
	WHEN ERROR IN
		GET #AR_OPEN_DIST.CH%, REGARDLESS
	USE
		FAILED = 1%
		CONTINUE AddRecord IF ERR = 11%
		FILENAME$ = "AR_OPEN_DIST"
		CONTINUE HelpError
	END WHEN

	IF AR_OPEN_DIST::POST_DATE <> AR_TEMP.DATE$
	THEN
		GOSUB AddRecord
	END IF

	GOTO ReportTitle IF AR_OPEN_DIST::UPDATED <> PERIOD$

	GOTO GetFromHistory &
		IF COMP_STRING(EDIT$(AR_OPEN_DIST::POST_DATE, -1%), WLDCRD$) = 0% &
		AND (WLDCRD$ <> "")

	GOTO GetFromHistory IF AR_OPEN_DIST::LTYPE <> "2"

	!
	! Build Temporary File now
	!
	AR_TEMP.CLIENT$	= AR_OPEN_DIST::CUSNUM
	AR_TEMP.DATE$	= AR_OPEN_DIST::POST_DATE
	AR_TEMP.MATTER$	= AR_OPEN_DIST::SUBACCT

	IF AR_OPEN_DIST::TAXTYP = "F"
	THEN
		AR_TEMP.FAMOUNT = AR_OPEN_DIST::AMOUNT + AR_TEMP.FAMOUNT
	END IF

	IF AR_OPEN_DIST::TAXTYP = "C"
	THEN
		AR_TEMP.CAMOUNT = AR_OPEN_DIST::AMOUNT + AR_TEMP.CAMOUNT
	END IF

	IF AR_OPEN_DIST::TAXTYP = "E"
	THEN
		AR_TEMP.EAMOUNT = AR_OPEN_DIST::AMOUNT + AR_TEMP.EAMOUNT
	END IF

	GOTO GetFromHistory

450
 AddRecord:
	RETURN IF (AR_TEMP.CLIENT$ = AR_OPEN_DIST::SUBACCT) AND &
		(AR_TEMP.MATTER$ = AR_OPEN_DIST::SUBACCT)
	RETURN IF AR_TEMP.DATE$ + AR_TEMP.CLIENT$ + AR_TEMP.MATTER$ = ""
	RETURN IF (AR_TEMP.FAMOUNT = 0.0) AND (AR_TEMP.CAMOUNT = 0.0) AND &
		(AR_TEMP.EAMOUNT = 0.0) AND (FAILED <> 1%)
	PUT #AR_TEMP.CH%
	AR_TEMP.FAMOUNT = 0.0
	AR_TEMP.CAMOUNT = 0.0
	AR_TEMP.EAMOUNT = 0.0
	RETURN IF FAILED <> 1%

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "LEGAL BILLING AR SUMMARY PAYMENT REPORT"
	TITLE$(2%) = "For Period " + PERIOD$
	TITLE$(3%) = ""

	!
	! Heading
	!		 1234567890123456789012345678901234567890
	TITLE$(4%) = "Client#    ClientName              " + &
		"                           Matter#         " + &
		"Total       Fees      Costs      Other"

	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	RESET #AR_TEMP.CH%, KEY #0%

 GetNextRec:
17020	WHEN ERROR IN
		GET #AR_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AR_TEMP"
		CONTINUE HelpError
	END WHEN

	GOSUB Client IF LAST_CUST$ <> AR_TEMP.CLIENT$

	GOSUB PrintLine &
		IF AR_TEMP.CLIENT$ + AR_TEMP.MATTER$ <> &
		LAST_CUST$ + LAST_MATTER$

	GOSUB DayTotal IF (LAST_DATE$ <> AR_TEMP.DATE$) AND (LAST_DATE$ <> "")

	GOSUB Day IF LAST_DATE$ <> AR_TEMP.DATE$

	LAST_MATTER$ = AR_TEMP.MATTER$
	LAST_CUST$ = AR_TEMP.CLIENT$
	LAST_DATE$ = AR_TEMP.DATE$
	LAST_NAME$ = AR_35CUSTOM::CUSNAM

	CUST_FTOTAL = CUST_FTOTAL + AR_TEMP.FAMOUNT
	CUST_ETOTAL = CUST_ETOTAL + AR_TEMP.EAMOUNT
	CUST_CTOTAL = CUST_CTOTAL + AR_TEMP.CAMOUNT

	DAY_FTOTAL = DAY_FTOTAL + AR_TEMP.FAMOUNT
	DAY_ETOTAL = DAY_ETOTAL + AR_TEMP.EAMOUNT
	DAY_CTOTAL = DAY_CTOTAL + AR_TEMP.CAMOUNT

	OVER_FTOTAL = OVER_FTOTAL + AR_TEMP.FAMOUNT
	OVER_ETOTAL = OVER_ETOTAL + AR_TEMP.EAMOUNT
	OVER_CTOTAL = OVER_CTOTAL + AR_TEMP.CAMOUNT

	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	!
	GOSUB DayTotal

	!
	! Do grand total here
	!
	TEXT$ = SPACE$(72%)  + STRING$(44%, 61%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TOTAL = OVER_FTOTAL + OVER_ETOTAL + OVER_CTOTAL
	TEXT$ = SPACE$(73%) + FORMAT$(TOTAL, "###,###.##") + " " + &
		FORMAT$(ABS(OVER_FTOTAL), "###,###.##") + " " + &
		FORMAT$(ABS(OVER_CTOTAL), "###,###.##") + " " + &
		FORMAT$(ABS(OVER_ETOTAL), "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitProgram:
17510	CALL OUTP_FINISH(UTL_REPORTX)

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

18000
 Client:
	AR_35CUSTOM::CUSNAM = STRING$(LEN(AR_35CUSTOM::CUSNAM), A"?"B)

	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, KEY #0% EQ AR_TEMP.CLIENT$, REGARDLESS
	USE
		CONTINUE 18010 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

18010	RETURN

 Day:
	TEXT$ = PRNT_DATE(AR_TEMP.DATE$, 8%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	RETURN

 DayTotal:

	!
	! Sub-total Day here
	!
	TEXT$ = SPACE$(72%)  + STRING$(44%, 95%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TOTAL = DAY_FTOTAL + DAY_ETOTAL + DAY_CTOTAL
	TEXT$ = SPACE$(73%) + FORMAT$(ABS(TOTAL), "###,###.##") + " " + &
		FORMAT$(ABS(DAY_FTOTAL), "###,###.##") + " " + &
		FORMAT$(ABS(DAY_CTOTAL), "###,###.##") + " " + &
		FORMAT$(ABS(DAY_ETOTAL), "###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	DAY_FTOTAL, DAY_CTOTAL, DAY_ETOTAL = 0.0

	RETURN

 PrintLine:

	!
	! Print Line for customer, matter
	!
	TOTAL = CUST_FTOTAL + CUST_ETOTAL + CUST_CTOTAL
	TEXT$ = LAST_CUST$ + " " + &
		LAST_NAME$ + " " + &
		LAST_MATTER$ + " " + &
		FORMAT$(ABS(TOTAL), "###,###.##") + " " + &
		FORMAT$(ABS(CUST_FTOTAL), "###,###.##") + " " + &
		FORMAT$(ABS(CUST_ETOTAL), "###,###.##") + " " + &
		FORMAT$(ABS(CUST_FTOTAL), "###,###.##")
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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


 !	SELECT ERR
 !	CASE 154%
		! Locked record
 !		SLEEP 1%
 !		RESUME
 !	END SELECT

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
