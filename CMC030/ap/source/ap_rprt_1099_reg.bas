1	%TITLE "Accounts Payable 1099 Register"
	%SBTTL "AP_RPRT_1099_REG"
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
	! ID:AP109R
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*1099 Register Report\* option prints
	!	and reviews a subject report for a specified calendar year.
	!	.b
	!	The 1099 Register Report prints in vendor number order and
	!	includes information on all disbursements made to all vendors for
	!	the calendar year.
	!	.b
	!	This report contains the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Vendor Number
	!	.le
	!	Summary Flag
	!	.le
	!	Vendor Name
	!	.le
	!	Code
	!	.le
	!	Description
	!	.le
	!	Transaction Number
	!	.le
	!	Invoice Number
	!	.le
	!	Invoice Date
	!	.le
	!	Check Number
	!	.le
	!	Check Date
	!	.le
	!	Check Amount
	!	.le
	!	1099 Amount
	!	.els
	!	.lm -5
	!	.note
	!	Vendors who do not meet the minimum defined in the 1099 table
	!	(AP MAST TABLE) will still print on this register, but will
	!	not print in the 1099 form.
	!	.end note
	!
	! Index:
	!	.x 1099 Register Report
	!	.x 1099 Register Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_1099_REG/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_1099_REG, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_1099_REG.OBJ;*
	!
	! Author:
	!
	!	07/31/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	01/10/91 - Craig Tanner
	!		Changed where FILENAME$ = "AP_1099_YYYY" to =
	!		"AP_1099_" + YEAR_1099$ in error handler.
	!
	!	06/05/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/27/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to assign channel for report.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose an excessive number of %PAGE's
	!
	!	07/27/2000 - Kevin Handy
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

	%PAGE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_1099_YYYY.HB"
	MAP	(AP_1099_YYYY)	AP_1099_YYYY_CDD	AP_1099_YYYY

	%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.HB"
	MAP	(AP_1099_TABLE)	AP_1099_TABLE_CDD	AP_1099_TABLE

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD		AP_VENDOR

	!
	! Dimension arrays
	!
	DIM	REAL	TOTAL_1099(100%, 2%)
	DIM	STRING	CD_1099(100%)

	%PAGE

	!******************************************************************
	! Take care of anything else before starting the report
	!******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

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
	!	The ^*From Item\* field determines the vendor number
	!	with which the report will begin. If the setting is blank
	!	the report will begin with the first vendor number in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>1099 Register
	!	.x 1099 Register>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field determines the vendor number
	!	with which the report will end. If this setting is blank the report
	!	will end with the last vendor in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>1099 Register
	!	.x 1099 Register>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort Order (NU,S)\*
	!	.b
	!	.lm +5
	!	The ^*Sort Order (NU,S)\* field determines if the
	!	Accounts Payable Register report is to be printed in vendor number
	!	order or in alphabetical order.
	!	.b
	!	.lm 15
	!	.LIST "*"
	!	.LIST ELEMENT
	!	^*NU#\*=#Vendor Number order
	!	.LIST ELEMENT
	!	^*S\*##=#Alphabetical order
	!	.LIST ELEMENT
	!	^*Blank\* #A blank setting will cause the report to print in numerical
	!	order.
	!	.END LIST
	!	.lm -5
	!	.b
	!	There are no other valid values for this field.
	!
	! Index:
	!	.x Sort Order>1099 Register Report
	!	.x 1099 Register Report>Sort Order
	!
	!--
	YEAR_1099$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Year (YYYY)\*
	!	.b
	!	.lm +5
	!	The ^*Year (YYYY)\* field determines the calendar
	!	year for which the 1099 Register will be printed.
	!	.lm -5
	!
	! Index:
	!	.x Year>1099 Register
	!	.x 1099 Register>Year
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

	%PAGE

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.OPN"
	USE
		FILENAME$ = "AP_1099_" + YEAR_1099$
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Payable Vendor file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Payable 1099 Year file
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_1099_YYYY.OPN"
	USE
		FILENAME$ = "AP_1099_TABLE"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Vendor 1099 Register for " + YEAR_1099$
	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3%) = "VendorNum  Fl VendorName              CD " + &
		"Description       Trans# InvNum          InvDate  " + &
		"CkNum  CkDate       CheckAmt      1099Amt"
	TITLE$(4%) = ""

	!
	! Layouts for printed lines
	!
	LYT_LINE$ = "$VendorNum:010,$1099Flag:012,$VendorName:037," + &
		"$Code:040,$CodeDescr:058,$TranKey:065,$Invoice:081," + &
		"DInvDate:090,$CheckNum:097,DCheckDate:106,VChkAmt:119," + &
		"V1099Amt:132"

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
		CONTINUE 17900 IF ERR = 11%
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
	! Find the AP 1099 Year record
	!
17200	WHEN ERROR IN
		FIND #AP_1099_YYYY.CH%, KEY #0% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		CONTINUE 17500 IF ERR = 155%
		FILENAME$ = "AP_1099_" + YEAR_1099$
		CONTINUE HelpError
	END WHEN

	!
	! Initialize some values before starting sub-loop
	!
	TEMP_TEXT$ = AP_VENDOR::VENNUM + " " + &
		AP_VENDOR::FLG1099 + "  " + &
		LEFT(AP_VENDOR::VENNAM, 23%) + " "
	CD$ = "1234567890"

	CD_1099_TOTAL(LOOP%),VENDOR_TOTAL(LOOP%) = 0.0 &
		FOR LOOP% = 1% TO 2%

	!
	! Report sub-loop starts here
	!
17300	WHEN ERROR IN
		GET #AP_1099_YYYY.CH%, REGARDLESS
	USE
		CONTINUE 17400 IF ERR = 11%
		FILENAME$ = "AP_1099_" + YEAR_1099$
		CONTINUE HelpError
	END WHEN

	!
	! Check this record
	!
	GOTO 17400 IF AP_VENDOR::VENNUM <> AP_1099_YYYY::VENNUM

	!
	! Print out this record
	!
	GOSUB CodeTotal &
		IF (CD$ <> AP_1099_YYYY::CODE)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get the Code Description from the 1099 Table file
	!
	AP_1099_TABLE::DESCR = STRING$(LEN(AP_1099_TABLE::DESCR), 63%)

17310	WHEN ERROR IN
		GET #AP_1099_TABLE.CH%, &
			KEY #0% EQ AP_1099_YYYY::CODE, &
			REGARDLESS
	USE
		CONTINUE 17320 IF ERR = 155%
		FILENAME$ = "AP_1099_TABLE"
		CONTINUE HelpError
	END WHEN

17320	TEXT$ = TEMP_TEXT$ + SPACE$(38% - LEN(TEMP_TEXT$)) + &
		AP_1099_YYYY::CODE + " " + &
		LEFT(AP_1099_TABLE::DESCR, 17%) + " " + &
		AP_1099_YYYY::TRANKEY + " " + &
		AP_1099_YYYY::INVNUM + " " + &
		PRNT_DATE(AP_1099_YYYY::INVDAT, 6%) + " " + &
		AP_1099_YYYY::CKNUM + " " + &
		PRNT_DATE(AP_1099_YYYY::CKDAT, 6%) + " " + &
		FORMAT$(AP_1099_YYYY::CKAMT, "#,###,###.##") + &
		FORMAT$(AP_1099_YYYY::AMT1099, " #,###,###.##")

	CD_1099_TOTAL(1%) = CD_1099_TOTAL(1%) + AP_1099_YYYY::CKAMT
	CD_1099_TOTAL(2%) = CD_1099_TOTAL(2%) + AP_1099_YYYY::AMT1099
	VENDOR_TOTAL(1%) = VENDOR_TOTAL(1%) + AP_1099_YYYY::CKAMT
	VENDOR_TOTAL(2%) = VENDOR_TOTAL(2%) + AP_1099_YYYY::AMT1099

	GOTO 17330 IF AP_1099_YYYY::CODE = CD_1099(LOOP%) &
		FOR LOOP% = 1% TO CD_1099_LOOP%

	CD_1099_LOOP%, LOOP% = CD_1099_LOOP% + 1%
	CD_1099(LOOP%) = AP_1099_YYYY::CODE

17330	TOTAL_1099(LOOP%, 1%) = TOTAL_1099(LOOP%, 1%) + AP_1099_YYYY::CKAMT
	TOTAL_1099(LOOP%, 2%) = TOTAL_1099(LOOP%, 2%) + AP_1099_YYYY::AMT1099

	TEMP_TEXT$ = ""

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! End of sub-loop
	!
	GOTO 17300

17400	GOSUB CodeTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOSUB VendorTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT

17500	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

17900	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Print out totals
	!
	TOTAL_1099(0%, 1%), TOTAL_1099(0%, 2%) = 0.0

	!
	! Loop to print out the totals
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FOR LOOP% = 1% TO CD_1099_LOOP%

		AP_1099_TABLE::DESCR = STRING$(LEN(AP_1099_TABLE::DESCR), 63%)

17910		WHEN ERROR IN
			GET #AP_1099_TABLE.CH%, &
				KEY #0% EQ CD_1099(LOOP%), &
				REGARDLESS
		USE
			CONTINUE 17920 IF ERR = 155%
			FILENAME$ = "AP_1099_TABLE"
			CONTINUE HelpError
		END WHEN

17920		TEXT$ = SPACE$(18%) + &
			"Total for           " + &
			CD_1099(LOOP%) + ":  " + &
			LEFT(AP_1099_TABLE::DESCR, 20%) + &
			SPACE$(43%) + &
			FORMAT$(TOTAL_1099(LOOP%, 1%), " #,###,###.##") + &
			FORMAT$(TOTAL_1099(LOOP%, 2%), " #,###,###.##")

		TOTAL_1099(0%, 1%) = TOTAL_1099(0%, 1%) + TOTAL_1099(LOOP%, 1%)
		TOTAL_1099(0%, 2%) = TOTAL_1099(0%, 2%) + TOTAL_1099(LOOP%, 2%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	NEXT LOOP%

	!
	! Print out grand totals
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = SPACE$(18%) + "Grand Total" + SPACE$(78%) + &
		FORMAT$(TOTAL_1099(0%, 1%), "#,###,###.## ") + &
		FORMAT$(TOTAL_1099(0%, 2%), "#,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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

 CodeTotal:
	!******************************************************************
	! Subroutine to print the 1099 Code total
	!******************************************************************
	IF CD$ <> "1234567890"
	THEN
		TEXT$ = SPACE$(41%) + "1099 Total" + SPACE$(56%) + &
			FORMAT$(CD_1099_TOTAL(1%), "#,###,###.##") + &
			FORMAT$(CD_1099_TOTAL(2%), " #,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	CD_1099_TOTAL(LOOP%) = 0.0 FOR LOOP% = 1% TO 2%

	CD$ = AP_1099_YYYY::CODE

	RETURN

	%PAGE

 VendorTotal:
	!******************************************************************
	! Subroutine to print Vendor total
	!******************************************************************

	IF (VENDOR_TOTAL(1%) <> 0.0) OR (VENDOR_TOTAL(2%) <> 0.0)
	THEN
		TEXT$ = SPACE$(41%) + "Vendor Total" + SPACE$(54%) + &
			FORMAT$(VENDOR_TOTAL(1%), "#,###,###.##") + &
			FORMAT$(VENDOR_TOTAL(2%), " #,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	VENDOR_TOTAL(1%) = 0.0
	VENDOR_TOTAL(2%) = 0.0

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
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report AP_RPRT_1099_REG
	!******************************************************************
	END
