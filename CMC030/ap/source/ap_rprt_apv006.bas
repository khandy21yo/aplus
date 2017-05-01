1	%TITLE "Accounts Payable Telephone Directory Report"
	%SBTTL "AP_RPRT_APV006"
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
	! ID:APV006
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Vendor Telephone Directory\* report
	!	prints an alphabetical listing of vendors with telephone
	!	numbers. This listing contains the following fields:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Vendor Name
	!	.le
	!	Address
	!	.le
	!	Phone Number
	!	.els
	!
	! Index:
	!	.x Reports>Vendor Telephone Directory
	!	.x Vendor Telephone Directory>Reports
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_APV006/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_APV006, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_APV006.OBJ;*
	!
	! Author:
	!
	!	07/31/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	03/12/92 - Kevin Handy
	!		Unrolled error trapping
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
	!	.x From Item>Telephone Directory
	!	.x Telephone Directory>From Item
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
	!	.x To Item>Telephone Directory
	!	.x Telephone Directory>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Order (NU/NA/S)\*
	!	.b
	!	.lm +5
	!	The ^*Order (NU/NA/S)\* determines the order in which the report is to
	!	be printed.
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
	!	.x Order>Telephone Directory
	!	.x Telephone Directory>Order
	!
	!--

	RP_ADD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Remit/PO Address (R,P)\*
	!	.b
	!	.lm +5
	!	The ^*Remit/PO Address\* setting determines whether the Remittance
	!	Address of the Purhase Order Address will be printed in the directory.
	!	.b
	!	.lm 15
	!	^*R\* = Remittance Address
	!	.b
	!	^*P\* = Purchase Order Address
	!	.lm -5
	!
	! Index:
	!	.x Remit/PO Address>Telephone Directory
	!	.x Telephone Directory>Remit/PO Address
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
	TITLE$(1%) = "Vendor Telephone Directory"
	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3%) = "VendorName                                " + &
		"Address                                           " + &
		"                           PhoneNumber  "
	TITLE$(4%) = ""

	!
	! Layouts for printed lines
	!
	LYT_LINE$ = "$VendorName:040,$Address:118,PPhoneNum:132"

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
	! Build the line to print out
	!
	TEMP_ADD1$ = AP_VENDOR::ADD1
	TEMP_ADD2$ = AP_VENDOR::ADD2
	TEMP_CITY$ = AP_VENDOR::CITY
	TEMP_STAT$ = AP_VENDOR::STATE
	TEMP_ZPCD$ = AP_VENDOR::ZIP
	TEMP_FONE$ = AP_VENDOR::PHONE

	IF (RP_ADD$ = "P")
	THEN
		TEMP_ADD1$ = AP_VENDOR::POADD1 &
			IF (AP_VENDOR::POADD1 <> "")
		TEMP_ADD2$ = AP_VENDOR::POADD2 &
			IF (AP_VENDOR::POADD2 <> "")
		TEMP_CITY$ = AP_VENDOR::POCITY &
			IF (AP_VENDOR::POCITY <> "")
		TEMP_STAT$ = AP_VENDOR::POSTATE &
			IF (AP_VENDOR::POSTATE <> "")
		TEMP_ZPCD$ = AP_VENDOR::POZIP &
			IF (AP_VENDOR::POZIP <> "")
		TEMP_FONE$ = AP_VENDOR::POPHONE &
			IF (AP_VENDOR::POPHONE <> "")
	END IF

	!
	! Print out one line
	!
	TEXT$ = AP_VENDOR::VENNAM + "  "

	TEXT$ = TEXT$ + TRM$(TEMP_ADD1$) + "  " &
		IF TRM$(TEMP_ADD1$) <> ""

	TEXT$ = TEXT$ + TRM$(TEMP_ADD2$) + "  " &
		IF TRM$(TEMP_ADD2$) <> ""

	TEXT$ = TEXT$ + TRM$(TEMP_CITY$) + ", " &
		IF TRM$(TEMP_CITY$) <> ""

	TEXT$ = TEXT$ + TRM$(TEMP_STAT$) + "  " &
		IF TRM$(TEMP_STAT$) <> ""

	TEXT$ = TEXT$ + TRM$(TEMP_ZPCD$) + "  " &
		IF TRM$(TEMP_ZPCD$) <> ""

	TEXT$ = LEFT(TEXT$, 115%) + STRING$(118% - LEN(TEXT$), A"."B) + &
		" " + PRNT_PHONE(TEMP_FONE$, 0%)

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

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
	! End of report AP_RPRT_APV006
	!******************************************************************
	END
