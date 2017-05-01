1	%TITLE "Accounts Payable Vendor Label Writer"
	%SBTTL "AP_RPRT_APV003"
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
	! ID:APV003
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Vendor Address Labels\* report
	!	prints vendor address labels on continuous form
	!	pressure-sensitive labels.
	!	.lm -5
	!
	! Index:
	!	.x Reports>Accounts Payable>Vendor Address Labels
	!	.x Accounts Payable>Vendor Address Labels
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_APV003/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_APV003, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_APV003.OBJ;*
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
	!	10/06/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to assign channel for report
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose an excessive number of %PAGE's
	!
	!	10/23/2000 - Kevin Handy
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
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

	!
	! Declare some variables
	!
	DECLARE	LONG	LINE_COUNTER

	%PAGE

	!******************************************************************
	! Take care of anything else before starting the report
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
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
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
	!	.x From Item>Print Vendor Labels
	!	.x Print Vendor Labels>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* determins the last record number
	!	to be printed. If the report is to end with the last record
	!	in the file the setting should be left blank.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Print Vendor Labels
	!	.x Print Vendor Labels>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Order (NU/NA/S)\*
	!	.b
	!	.lm +5
	!	The ^*Order (NU/NA/S)\* determines the order in which the file is to be
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
	!	.x Order>Print Vendor Labels
	!	.x Print Vendor Labels>Order
	!
	!--

	RP_ADD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Remit/PO Address (R,P)\*
	!	.b
	!	.lm +5
	!	The ^*Remit/PO Address\* setting determines whether the Remittance
	!	Address of the Purhase Order Address will be printed on the labels.
	!	.b
	!	.lm 15
	!	^*R\* = Remittance Address
	!	.b
	!	^*P\* = Purchase Order Address
	!	.lm -10
	!
	! Index:
	!	.x Remit/PO Address>Print Vendor Labels
	!	.x Print Vendor Labels>Remit/PO Address
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
	! No titles are set up
	!

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
	! Print out one line
	!
	IF UTL_REPORTX::PRINTTO <> 1%
	THEN
		!
		! Print out the Vendor Name
		!
		CALL OUTP_LINENOTITLE("", UTL_REPORTX, AP_VENDOR::VENNAM, 0%)
		GOTO 17200 IF (UTL_REPORTX::PAGELEN <= 1%)
		LINE_COUNTER = 2%

		!
		! Start printing out the Vendor's address
		!
		TEMP$ = AP_VENDOR::ADD1
		TEMP$ = AP_VENDOR::POADD1 &
			IF (RP_ADD$ = "P") AND (AP_VENDOR::POADD1 <> "")

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEMP$, 0%)
		GOTO 17200 IF (UTL_REPORTX::PAGELEN <= LINE_COUNTER)
		LINE_COUNTER = LINE_COUNTER + 1%

		!
		! Finish printing out the Vendor's address
		!
		TEMP$ = AP_VENDOR::ADD2
		TEMP$ = AP_VENDOR::POADD2 &
			IF (RP_ADD$ = "P") AND (AP_VENDOR::POADD2 <> "")

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEMP$, 0%)
		GOTO 17200 IF (UTL_REPORTX::PAGELEN <= LINE_COUNTER)
		LINE_COUNTER = LINE_COUNTER + 1%

		!
		! Print out the Vendor's City, State, and Zip Code
		!
		TEMP$ = AP_VENDOR::CITY + "  " + AP_VENDOR::STATE + "  " + &
			AP_VENDOR::ZIP
		TEMP$ = AP_VENDOR::POCITY + "  " + AP_VENDOR::POSTATE + "  " + &
			AP_VENDOR::POZIP &
			IF (TRM$(AP_VENDOR::POCITY + AP_VENDOR::POSTATE + &
				AP_VENDOR::POZIP) <> "") AND (RP_ADD$ = "P")

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEMP$, 0%)
		GOTO 17200 IF (UTL_REPORTX::PAGELEN <= LINE_COUNTER)
		LINE_COUNTER = LINE_COUNTER + 1%

		!
		! Print out the Vendor's country
		!
		TEMP$ = AP_VENDOR::COUNTRY
		TEMP$ = AP_VENDOR::POCOUNTRY &
			IF (AP_VENDOR::POCOUNTRY <> "") AND (RP_ADD$ = "P")

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEMP$, 0%)
		GOTO 17200 IF (UTL_REPORTX::PAGELEN <= LINE_COUNTER)
		LINE_COUNTER = LINE_COUNTER + 1%

		!
		! Print out the Vendor Number
		!
		CALL OUTP_LINENOTITLE("", UTL_REPORTX, SPACE$(28%) + "(" + &
			AP_VENDOR::VENNUM + ")", 0%)
		GOTO 17200 IF (UTL_REPORTX::PAGELEN <= LINE_COUNTER)
		LINE_COUNTER = LINE_COUNTER + 1%

		!
		! Fill up the rest of the label with white space
		!
		CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
			FOR I% = LINE_COUNTER to UTL_REPORTX::PAGELEN

	ELSE
		CALL OUTP_LINENOTITLE("", UTL_REPORTX, AP_VENDOR::VENNAM, 0%)

		TEMP$ = AP_VENDOR::ADD1
		TEMP$ = AP_VENDOR::POADD1 &
			IF (AP_VENDOR::POADD1 <> "") AND (RP_ADD$ = "P")

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEMP$, 0%)

		TEMP$ = AP_VENDOR::ADD2
		TEMP$ = AP_VENDOR::POADD2 &
			IF (AP_VENDOR::POADD2 <> "") AND (RP_ADD$ = "P")

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEMP$, 0%)

		TEMP$ = AP_VENDOR::CITY + "  " + AP_VENDOR::STATE + "  " + &
			AP_VENDOR::ZIP
		TEMP$ = AP_VENDOR::POCITY + "  " + AP_VENDOR::POSTATE + "  " + &
			AP_VENDOR::POZIP &
			IF (TRM$(AP_VENDOR::POCITY + AP_VENDOR::POSTATE + &
				AP_VENDOR::POZIP) <> "") AND (RP_ADD$ = "P")

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEMP$, 0%)

		TEMP$ = AP_VENDOR::COUNTRY
		TEMP$ = AP_VENDOR::POCOUNTRY &
			IF (AP_VENDOR::POCOUNTRY <> "") AND (RP_ADD$ = "P")

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, TEMP$, 0%)

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, SPACE$(28%) + "(" + &
			AP_VENDOR::VENNUM + ")", 0%)

		CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%)

	END IF

	!
	! Check status
	!
17200	GOTO ExitProgram IF UTL_REPORTX::STAT

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
	! Finish up report
	!
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
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report AP_RPRT_APV003
	!******************************************************************
	END
