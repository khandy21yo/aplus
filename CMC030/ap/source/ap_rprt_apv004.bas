1	%TITLE "Accounts Payable Vendor 1099 Report"
	%SBTTL "AP_RPRT_APV004"
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
	! ID:APV004
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Vendor 1099 Listing\* report includes a list of ^*all\*
	!	vendors, the 1099 flags, and the Federal Tax ID numbers
	!	from the Vendor Master File.
	!	.b
	!	A report setting prints either a short or
	!	long form. The short form column headings include:
	!	.b
	!	.lm 15
	!	.LS 0,"*"
	!	.LE
	!	Vendor Number
	!	.LE
	!	Vendor Name
	!	.LE
	!	1099 Flag
	!	.LE
	!	Federal Tax ID
	!	.ELS
	!	.lm -5
	!	The long form report column headings include columns for the
	!	remittance address as well as the column headings which are included
	!	in the short form report.
	!
	! Index:
	!	.x Reports>Vendor 1099 Listing
	!	.x Vendor 1099 Listing>Reports
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_APV004/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_APV004, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_APV004.OBJ;*
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

	%PAGE

	!******************************************************************
	! Take care of any other items before starting the report
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
	!	.x From Item>Vendor 1099 Listing
	!	.x Venfor 1099 Listing>From Item
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
	!	in the file, the setting should be left blank.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Vendor 1099 Listing
	!	.x Vendor 1099 Listing>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Order (NU/NA/S)\*
	!	.b
	!	.lm +5
	!	The ^*Order (NU/NA/S)\* determines the order in which the report will
	!	be printed.
	!	.b
	!	Valid entries are:
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
	!	.x Order>Vendor 1099 Listing
	!	.x Vendor 1099 Listing>Order
	!
	!--

	FORMTYPE$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Form Type (L/S)\*
	!	.b
	!	.lm +5
	!	The ^*Form Type (L/S)\* setting determines whether the Vendor
	!	Master File List will be printed in "long" or "short" form.
	!	.b
	!	The long form includes the following columns of information:
	!	.lm 15
	!	.LS "*"
	!	.LE
	!	Vendor Number
	!	.LE
	!	Vendor Name
	!	.LE
	!	Address Line 1
	!	.LE
	!	Address Line 2
	!	.LE
	!	City
	!	.LE
	!	State
	!	.LE
	!	Zip
	!	.LE
	!	Country
	!	.LE
	!	Telephone Number
	!	.ELS
	!	.lm -5
	!	The short form does not print address and telephone information.
	!	.b
	!	If the field is left blank the short form will be printed.
	!	.lm -5
	!
	! Index:
	!	.x Form Type>Vendor 1099 Listing
	!	.x Vendor 1099 Listing>Form Type
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
	TITLE$(1%) = "Vendor File 1099 Report"
	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3%) = "VendorNum  VendorName                       " + &
		"        1099 FedTaxID"
	TITLE$(3%) = "VendorNum  VendorName           Address     " + &
		"         Address              City            ST Zip" + &
		"        Country 1099 FedTaxID" &
		IF (FORMTYPE$ = "L")
	TITLE$(4%) = ""

	!
	! Layouts for printed lines
	!
	LYT_LINE$ = "$VendorNum:010,$VendorName:051,$1099Flag:053," + &
		"$FederalTaxID:080"

	LYT_LINE1$ = "$VendorNum:010,$VendorName:031,$Address1:052," + &
		"$Address2:073,$City:089,$State:092,$ZipCode:103," + &
		"$Country:111,$1099Flag:114,$FederalTaxID:130"

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
	! Print out one line
	!
	TEXT$ = AP_VENDOR::VENNUM + " " + &
		AP_VENDOR::VENNAM + " " + &
		AP_VENDOR::FLG1099 + "    " + &
		AP_VENDOR::FEDID

	IF (FORMTYPE$ = "L")
	THEN
		TEXT$ = AP_VENDOR::VENNUM + " " + &
			LEFT(AP_VENDOR::VENNAM, 20%) + " " + &
			LEFT(AP_VENDOR::ADD1, 20%) + " " + &
			LEFT(AP_VENDOR::ADD2, 20%) + " " + &
			AP_VENDOR::CITY   + " " + &
			AP_VENDOR::STATE  + " " + &
			AP_VENDOR::ZIP    + " " + &
			LEFT(AP_VENDOR::COUNTRY, 7%) + " " + &
			AP_VENDOR::FLG1099 + "    " + &
			AP_VENDOR::FEDID

		LYT_LINE$ = LYT_LINE1$

	END IF

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
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
	! Finish up report
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
	! End of report AP_RPRT_APV004
	!******************************************************************
	END
