1	%TITLE "Vendor File"
	%SBTTL "AP_RPRT_APV001"
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
	! ID:APV001
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Vendor File\* can be printed in "short" or "long" form.
	!	The short form includes only two columns:
	!	.b
	!	.lm 15
	!	.List 0,"*"
	!	.le
	!	Vendor Number
	!	.LE
	!	Vendor Name
	!	.ELS
	!	.lm 10
	!	The long form includes the following columns:
	!	.b
	!	.lm 15
	!	.List 0,"*"
	!	.le
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
	!	If a separate Purchase Order address and telephone number is
	!	included in any record, that information will be printed on the
	!	report.
	!	.b
	!	The file can be sorted and printed in either Vendor Number or
	!	alphabetical order.
	!
	! Index:
	!	.x Vendor>Report
	!	.x Report>Vendor
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_APV001/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_APV001, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_APV001.OBJ;*
	!
	! Author:
	!
	!	07/31/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	05/17/88 - Lance Williams
	!		Modified the header
	!
	!	07/19/89 - Frank Starman
	!		Error trapping for AP_CONTACT
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/27/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/20/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to assign channel for report
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Remove an excessive number of %PAGE's
	!
	!	10/23/3000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

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

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTACT.HB"
	MAP	(AP_CONTACT)		AP_CONTACT_CDD	AP_CONTACT

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
	!	The ^*From Item\* selects a
	!	record number from which the report will begin. If the
	!	report is to begin with the first record in the file, the
	!	setting should be left blank.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Vendor Listing
	!	.x Vendor Listing>From Item
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
	!	in the file, the setting should be left blank.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Vendor Listing
	!	.x Vendor Listing>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Order (NU/NA/S)\*
	!	.b
	!	.lm +5
	!	The ^*Order (NU/NA/S)\* determines the KEY to be used in
	!	sorting the file.
	!	.lm 15
	!	.LS "*"
	!	.LE
	!	^*NU\* = Vendor Number
	!	.LE
	!	^*NA\* = Vendor Name
	!	.LE
	!	^*S\* = Alpha Sort
	!	.ELS
	!	.b
	!	.lm -5
	!	A blank in this field will cause the report to be printed in
	!	Vendor Number order.
	!	.lm -5
	!
	! Index:
	!	.x Order>Vendor Listing
	!	.x Vendor Listing>Order
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
	!	The long form includes the following columns:
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
	!	.b
	!	.lm -5
	!	The short form does not print address and telephone information.
	!	.b
	!	A blank in this setting will cause a short form to be printed.
	!	.lm -5
	!
	! Index:
	!	.x Form Type>Vendor Listing
	!	.x Vendor Listing>Form Type
	!
	!--

	FINDCUSTOM$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Contact\*
	!	.b
	!	.lm +5
	!	The ^*Contact\* field refers to whether the vendor has a contact person.
	!	A ^*Y\* would indicate a contact person, while a ^*N\* would indicate no
	!	contact person.
	!	.lm -5
	!
	! Index:
	!	.x Contact>Vendor Listing
	!	.x Vendor Listing>Contact
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

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CONTACT.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "AP_CONTACT"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Vendor Master File Report"
	TITLE$(2%) = ""

	!
	! Heading
	!
	TITLE$(3%) = "VendorNum  VendorName                              "
	TITLE$(3%) = "VendorNum  VendorName           Address     " + &
		"              Address               City            " + &
		"ST Zip        Country  Phone" &
		IF FORMTYPE$ = "L"
	TITLE$(4%) = "."

	IF  (FORMTYPE$ = "L") AND (FINDCUSTOM$ = "Y")
	THEN
		TITLE$(4%) = "           Contact Name                   Title              " + &
				"  Phone          Extension "
		TITLE$(5%) = "."
	END IF

	!
	! Layouts for lines printed
	!
	LYT_LINE$ = "$VendorNum:010,$VendorName:051"
	LYT_LINE$ = "$VendorNum:010,$VendorName:031,$Address1:057," + &
		"$Address2:079,$City:095,$State:098,$ZipCode:109," + &
		"$Country:118,$PhoneNumber:132" &
			IF FORMTYPE$ = "L"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	IF FROM_ITEM$ = ""
	THEN
		RESET #AP_VENDOR.CH%, KEY #K_NUM%
	ELSE
		FIND #AP_VENDOR.CH%, KEY #K_NUM% GE FROM_ITEM$, REGARDLESS
	END IF

 GetNextRec:
17020	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AP_VENDOR.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
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

17100	WHEN ERROR IN
		GET #AP_CONTACT.CH%, KEY #0% EQ AP_VENDOR::VENNUM, REGARDLESS
	USE
		AP_CONTACT::CUSNUM = ""
		AP_CONTACT::CONTACT_NAME = ""
		AP_CONTACT::TITLE = ""
		AP_CONTACT::PHONE = ""
		AP_CONTACT::EXTENSION = ""

		CONTINUE PrintLine IF ERR = 155% OR ERR = 9%
		CONTINUE BlankLine IF ERR = 11%
		FILENAME$ = "AP_CONTACT"
		CONTINUE HelpError
	END WHEN

 PrintLine:
	!
	! Print out one line
	!
	TEXT$ = AP_VENDOR::VENNUM + " " + &
		AP_VENDOR::VENNAM

	IF FORMTYPE$ = "L"
	THEN
		TEXT$ = AP_VENDOR::VENNUM + " " + &
			LEFT(AP_VENDOR::VENNAM, 20%) + " " + &
			AP_VENDOR::ADD1 + " " + &
			AP_VENDOR::ADD2 + " " + &
			AP_VENDOR::CITY + " " + &
			AP_VENDOR::STATE + " " + &
			AP_VENDOR::ZIP + " " + &
			AP_VENDOR::COUNTRY + " " + &
			PRNT_PHONE(AP_VENDOR::PHONE, 0%)

		IF (TRM$(AP_VENDOR::POADD1) <> "") OR &
			(TRM$(AP_VENDOR::POCITY) <> "") OR &
			(TRM$(AP_VENDOR::POSTATE) <> "") OR &
			(TRM$(AP_VENDOR::POZIP) <> "") OR &
			(TRM$(AP_VENDOR::POCOUNTRY) <> "") OR &
			(TRM$(AP_VENDOR::POPHONE) <> "")
		THEN
			CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), &
				TEXT$, 0%)
			GOTO ExitProgram IF UTL_REPORTX::STAT

			TEXT$ = SPACE$(15%) + "   PO Address    " + &
				AP_VENDOR::POADD1 + " " + &
				AP_VENDOR::POADD2 + " " + &
				AP_VENDOR::POCITY + " " + &
				AP_VENDOR::POSTATE + " " + &
				AP_VENDOR::POZIP + " " + &
				AP_VENDOR::POCOUNTRY + " " + &
				PRNT_PHONE(AP_VENDOR::POPHONE, 0%) &
		END IF

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		IF (AP_CONTACT::CUSNUM <> "") AND (FINDCUSTOM$ = "Y")
		THEN
			FOR LOOP% = 1% TO 100%
				TEXT$ = "           " + &
					AP_CONTACT::CONTACT_NAME + " " + &
					AP_CONTACT::TITLE + " " + &
					PRNT_PHONE(AP_CONTACT::PHONE, 0%) + "  " + &
					AP_CONTACT::EXTENSION

				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
				GET #AP_CONTACT.CH%, REGARDLESS
				GOTO BlankLine &
					IF AP_CONTACT::CUSNUM <> AP_VENDOR::VENNUM
			NEXT LOOP%
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		END IF
	ELSE

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	!
	! Try for next record
	!
	GOTO GetNextRec

 BlankLine:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
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
	! End of report AP_RPRT_APV001
	!******************************************************************
	END
