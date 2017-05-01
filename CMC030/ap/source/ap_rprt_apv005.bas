1	%TITLE "Accounts Payable Vendor Dump"
	%SBTTL "AP_RPRT_APV005"
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
	! ID:APV005
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Vendor File Dump\* report prints ^&all\&
	!	information in the Vendor Master File, i.e., the contents of every
	!	field in every record is displayed. These fields consist of the following:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Vendor Number
	!	.le
	!	Vendor Name
	!	.le
	!	Address
	!	.le
	!	Remittance Address
	!	.le
	!	Purchase Order Address
	!	.le
	!	Federal Identification Number
	!	.le
	!	1099
	!	.le
	!	Due Date
	!	.le
	!	Due Days
	!	.le
	!	Discount Days
	!	.le
	!	Discount Date
	!	.le
	!	Percent Discount
	!	.le
	!	Purge
	!	.le
	!	Sort Key
	!	.els
	!
	! Index:
	!	.x Vendor File Dump>Reports
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_APV005/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_APV005, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_APV005.OBJ;*
	!
	! Author:
	!
	!	08/03/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	05/17/88 - Lance Williams
	!		Modified the header
	!
	!	03/12/92 - Kevin Handy
	!		Unrolled error trapping (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/27/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to assign channel for report
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	10/23/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
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
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

	%PAGE

	!******************************************************************
	! Take care of anything else before staring the report
	!******************************************************************

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

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field selects a
	!	record number from which the report will begin. If the
	!	report is to begin with the first record in the file, the
	!	setting should be left blank.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Vendor Dump
	!	.x Vendor Dump>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* determines the last record number
	!	to be printed. If the report is to end with the last record
	!	in the file the setting should be left blank.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Vendor Dump
	!	.x Vendor Dump>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Order (NU/NA/S)\*
	!	.b
	!	.lm +5
	!	The ^*Order (NU/NA/S)\* determines the order in which the file will be
	!	sorted.
	!	.lm 15
	!	.LS "*"
	!	.LE
	!	^*NU\* = Vendor Number
	!	.LE
	!	^*NA\* = Vendor Name
	!	.LE
	!	^*S\* = Alpha Sort
	!	.ELS
	!	.lm -5
	!	A blank in this field will cause the report to be printed in
	!	Vendor Number order.
	!
	! Index:
	!	.x Order>Vendor Dump
	!	.x Vendor Dump>Order
	!
	!--


	SELECT SORTBY$

	CASE "NU"

		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::VENNUM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::VENNUM))

	CASE "NA"

		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::VENNAM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::VENNAM))

	CASE ELSE

		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AP_VENDOR::ALPSRT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AP_VENDOR::ALPSRT))

	END SELECT


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Vendor Master File Dump"
	TITLE$(2%) = ""
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE1$ = "VendorNum  VendorName                               "
	TITLE2$ = SPACE$(31%) + "Remittance Address" + SPACE$(30%) + &
		"Purchase Order Address"
	TITLE3$ = "Address"
	TITLE4$ = "Address (Cont.)"
	TITLE5$ = "City, State Country Zip"
	TITLE6$ = "Phone Number"
	TITLE7$ = "Fed ID #      1099  DueDays  DueDate  DisDays  " + &
		"DisDate  %Discount  Purge  SortKey"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AP_VENDOR.CH%, KEY #K_NUM%
		ELSE
			FIND #AP_VENDOR.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "AP_VENDOR"
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
		GET #AP_VENDOR.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF (ERR = 11%)
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	SELECT SORTBY$

	CASE "NU"

		GOTO ExitTotal IF (AP_VENDOR::VENNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "NA"

		GOTO ExitTotal IF (AP_VENDOR::VENNAM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE ELSE

		GOTO ExitTotal IF (AP_VENDOR::ALPSRT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	END SELECT

	!
	! Print out the record
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE1$, 12%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = AP_VENDOR::VENNUM + " " + &
		AP_VENDOR::VENNAM

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE2$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = TITLE3$ + SPACE$(24%) + AP_VENDOR::ADD1   + &
		SPACE$(23%) + AP_VENDOR::POADD1 &

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT


	TEXT$ = TITLE4$ + SPACE$(16%) + AP_VENDOR::ADD2   + &
		SPACE$(27%) + AP_VENDOR::POADD2 &

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT


	TEXT$ = TITLE5$ + SPACE$(8%) + AP_VENDOR::CITY + ", " + &
		AP_VENDOR::STATE + " " + AP_VENDOR::COUNTRY + " " + &
		AP_VENDOR::ZIP + SPACE$(9%) + AP_VENDOR::POCITY + &
		", " + AP_VENDOR::POSTATE + " " + AP_VENDOR::POCOUNTRY + &
		" " + AP_VENDOR::POZIP

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT


	TEXT$ = TITLE6$ + SPACE$(19%) + PRNT_PHONE(AP_VENDOR::PHONE, 0%) + &
		SPACE$(35%) + PRNT_PHONE(AP_VENDOR::POPHONE, 0%) &

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT


	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT


	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE7$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		TESTDATE% = VAL%(AP_VENDOR::DUEDATE)
		TESTDATE$ = " "
	USE
		TESTDATE$ = "*"
		CONTINUE
	END WHEN

	WHEN ERROR IN
		TESTDIS% = VAL%(AP_VENDOR::DISDATE)
		TESTDIS$ = " "
	USE
		TESTDIS$ = "*"
		CONTINUE
	END WHEN

	TEXT$ = AP_VENDOR::FEDID + " " + &
		AP_VENDOR::FLG1099 + "        " + &
		FORMAT$(AP_VENDOR::DUEDAYS, "####") + "       " + &
		AP_VENDOR::DUEDATE + TESTDATE$ + "    " + &
		FORMAT$(AP_VENDOR::DISDAYS, "####") + "       " + &
		AP_VENDOR::DISDATE + TESTDIS$ + "   " + &
		FORMAT$(AP_VENDOR::DISCPER, "###.##%") + "  " + &
		AP_VENDOR::PURGE + "      " + &
		AP_VENDOR::ALPSRT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT


	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), STRING$(132%, A"="B), 0%)
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
	!
	! Finish up the report
	!
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
	! End of report AP_RPRT_APV005
	!******************************************************************
	END
