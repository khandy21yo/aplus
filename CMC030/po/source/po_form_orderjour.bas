1	%TITLE "Print Form"
	%SBTTL "PO_FORM_ORDERJOUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	! ID:POFROM
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Form\* option
	!	prints a hard copy of the Purchase Order forms.
	!	The format is user defined.   (See Forms Controlling in the Utility
	!	Section.)
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_FORM_ORDERJOUR/LINE
	!	$ LINK/EXECUTABLE=PO_EXE:*.EXE PO_FORM_ORDERJOUR, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_FORM_ORDERJOUR.OBJ;*
	!
	! Author:
	!
	!	06/28/90 - Kevin Handy, J. Shad Rydalch
	!
	! Modification history:
	!
	!	07/09/90 - Kevin Handy
	!		Modifications to make it work correctly.
	!
	!	10/19/90 - Kevin Handy
	!		Added FOB code information, fixed bugs in printing
	!		other codes.
	!
	!	06/06/91 - J. Shad Rydalch
	!		Added form fields for address that will keep
	!		post office happy.
	!
	!	06/19/91 - Craig Tanner
	!		Added section to update UTL_REPORT from master.
	!
	!	12/04/91 - Kevin Handy
	!		Removed garbage characters so program could
	!		compile.
	!
	!	02/17/92 - Dan Perkins
	!		Modified to accomodate file layout changes
	!		in ORDERJOUR, ORDERLINE, and ORDERSLINE files.
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	03/12/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	03/13/92 - Kevin Handy
	!		Removed duplicate error trap (check)
	!
	!	05/19/92 - Kevin Handy
	!		Modified to use OUTP_NEWPAGE instead of loop.
	!
	!	05/20/92 - Dan Perkins
	!		Rehacked form output to allow printing line info
	!		after sub_line info is obtained.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/05/92 - Dan Perkins
	!		Trap opening of UTL_FOB file.  Try to open
	!		PO_ACKNOWLEDGE file.
	!
	!	10/07/92 - Dan Perkins
	!		Added IC_BINMAP junk so we can get the bin map
	!		location.
	!
	!	01/07/93 - Dan Perkins
	!		Added option to print dates in six character format
	!		or in the regular eight character format.
	!
	!	04/09/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/10/94 - Kevin Handy
	!		Added PO_PARTCROSS access.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/08/97 - Kevin Handy
	!		Use OUTP_INITFORM function.
	!
	!	09/09/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/26/98 - Kevin Handy
	!		Don't erase SMG_SCREEN_DATA%, which is
	!		never created
	!
	!	11/01/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Define maps
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERJOUR.HB"
	MAP (PO_ORDERJOUR)	PO_ORDERJOUR_CDD	PO_ORDERJOUR

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERLINE.HB"
	MAP (PO_ORDERLINE)	PO_ORDERLINE_CDD	PO_ORDERLINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERSLINE.HB"
	MAP (PO_ORDERSLINE)	PO_ORDERSLINE_CDD	PO_ORDERSLINE

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	COM (AP_VENDOR)		AP_VENDOR_CDD		AP_VENDOR_EXAM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	COM (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION_EXAM

	%INCLUDE "SOURCE:[PO.OPEN]PO_ACKNOWLEDGE.HB"
	MAP (PO_ACKNOWLEDGE)	PO_ACKNOWLEDGE_CDD	PO_ACKNOWLEDGE

	%INCLUDE "SOURCE:[PO.OPEN]PO_CATEGORY.HB"
	MAP (PO_CATEGORY)	PO_CATEGORY_CDD		PO_CATEGORY

	%INCLUDE "SOURCE:[PO.OPEN]PO_TYPE.HB"
	MAP (PO_TYPE)		PO_TYPE_CDD		PO_TYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.HB"
	MAP (UTL_CARRIER)	UTL_CARRIER_CDD		UTL_CARRIER

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS)		UTL_TERMS_CDD		UTL_TERMS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_FOB.HB"
	MAP (UTL_FOB)		UTL_FOB_CDD		UTL_FOB

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM			FORM_GROUP_CDD		FORM_GROUP(10%)

	%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.HB"
	MAP (PO_PARTCROSS)	PO_PARTCROSS_CDD	PO_PARTCROSS

	MAP (JOUR_FORM) &
		ORDER_TOTAL, &
		LINE_TOTAL, &
		PAGE_NUMBER%, &
		LAST_PAGE%, &
		AP_VENDOR.ADDLINE$(3%) = 50%, &
		AP_VENDOR.POADDLINE$(3%) = 50%, &
		PO_ORDERJOUR.TOADDLINE$(3%) = 50%, &
		UTL_LOCATION.ADDLINE$(3%) = 50%, &
		UTL_LOCATION.SHPADDLINE$(3%) = 50%

	COM (PO_ACKNOWLEDGE_CH) PO_ACKNOWLEDGE.CH%
	COM (PO_CATEGORY_CH)	PO_CATEGORY.CH%
	COM (PO_TYPE_CH)	PO_TYPE.CH%
	COM (UTL_CARRIER_CH)	UTL_CARRIER.CH%
	COM (UTL_TERMS_CH)	UTL_TERMS.CH%
	COM (UTL_FOB_CH)	UTL_FOB.CH%
	COM (IC_BINMAP_CH)	IC_BINMAP.CH%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OUTP_INITFORM
	EXTERNAL LONG	FUNCTION OUTP_FORMINIT
	EXTERNAL LONG   FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG   FUNCTION AP_EXAM_VENDOR
	EXTERNAL LONG   FUNCTION UTL_EXAM_LOCATION

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$ = "POFORM"

	!
	! Look up device
	!
	CALL  READ_DEVICE("PO_FORM", PO_FORM.DEV$, STAT%)

500	!******************************************************************
	! Set up the report settings screen
	!******************************************************************

	!
	! store original values for the help message
	!
	TEMP_IDENT$ = SCOPE::PRG_IDENT
	TEMP_PROGRAM$ = SCOPE::PRG_PROGRAM

510	!******************************************************************
	! Set up the report settings screen
	!******************************************************************

	!
	! Ask user to change settings
	!
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field allows for entry of a selected
	!	batch number. Each Journal File is assigned
	!	a user batch number consisting of two (2) alphanumeric
	!	characters.
	!	.b
	!	Only one batch at a time may be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Purchase Order Form
	!	.x Purchase Order Form>Batch Number
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field enters a selected sort order
	!	in which the report is to print.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*N\* - Purchase Order Number
	!	.te
	!	^*T\* - Purchase Order Type
	!	.te
	!	^*V\* - Vendor Number
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort by>Purchase Order Form
	!	.x Purchase Order Form>Sort by
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the
	!	printing to begin with a particular
	!	item. The value must be in agreement with the value
	!	entered in field (01) Sort by.
	!	.b
	!	A blank field will cause the form to start with
	!	the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Purchase Order Form
	!	.x Purchase Order Form>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the
	!	printing to end with a particular
	!	item in the file. The value must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field causes the form to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Purchase Order Form
	!	.x Purchase Order Form>To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	items to be printed by entering a "wildcard" for Wildcarding Technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Purchase Order Form
	!	.x Purchase Order Form>Wildcard
	!
	!--

	REPORT$ = REPORT$ + "$" + TRM$(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) Form\*
	!	.b
	!	.lm +5
	!	The ^*Forms\* option enters the form number to be used for
	!	printing.  (See Forms Controlling
	!	under Utility Section.)
	!	.lm -5
	!
	! Index:
	!	.x Form>Purchase Order Form
	!	.x Purchase Order Form>Form
	!
	!--

	SELECT SORTBY$
	CASE "N"
		K_NUM% = 0%

		!
		! Routine to load left justified zeros into FROM_ITEM$
		! and TO_ITEM$ if any order numbers are entered as ranges
		!
		FROM_ITEM$ = STRING$(LEN(PO_ORDERJOUR::PO) - &
			LEN(FROM_ITEM$), A"0"B) + &
			FROM_ITEM$ IF FROM_ITEM$ <> ""

		TO_ITEM$ = STRING$(LEN(PO_ORDERJOUR::PO) - &
			LEN(TO_ITEM$), A"0"B) + &
			TO_ITEM$ IF TO_ITEM$ <> ""

	CASE "V"
		K_NUM% = 1%

	CASE "T"
		K_NUM% = 2%
	END SELECT

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!***************************************************************
	! Open all other files
	!***************************************************************

600	!
	! Open purchase order journal header file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERJOUR.MOD"
	USE
		FILENAME$ = "PO_ORDERJOUR_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

610	!
	! Open purchase order journal line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERLINE.OPN"
	USE
		FILENAME$ = "PO_ORDERLINE_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

620	!
	! Open purchase order journal subline file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERSLINE.OPN"
	USE
		FILENAME$ = "PO_ORDERSLINE_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

630	!
	! Open po acknowledge file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_ACKNOWLEDGE.OPN"
	USE
		CONTINUE 640 IF ERR = 5%
		FILENAME$ = "PO_ACKNOWLEDGE"
		CONTINUE HelpError
	END WHEN

640	!
	! Open po type file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_TYPE.OPN"
	USE
		CONTINUE 650 IF ERR = 5%
		FILENAME$ = "PO_TYPE"
		CONTINUE HelpError
	END WHEN

650	!
	! Open carrier file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.OPN"
	USE
		CONTINUE 660 IF ERR = 5%
		FILENAME$ = "UTL_CARRIER"
		CONTINUE HelpError
	END WHEN

660	!
	! Open terms file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.OPN"
	USE
		CONTINUE 670 IF ERR = 5%
		FILENAME$ = "UTL_TERMS"
		CONTINUE HelpError
	END WHEN

670	!
	! Open FOB file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_FOB.OPN"
	USE
		CONTINUE 680 IF ERR = 5%
		FILENAME$ = "UTL_FOB"
		CONTINUE HelpError
	END WHEN

680	!
	! Open binmap file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.OPN"
	USE
		CONTINUE 690 IF ERR = 5%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

690	!
	! Open Vendor Product Cross reference file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.OPN"
	USE
		CONTINUE EndOpen IF ERR = 5%
		FILENAME$ = "PO_PARTCROSS"
		CONTINUE HelpError
	END WHEN

 EndOpen:
	!
	! Restore original values for the help message
	!
	SCOPE::PRG_IDENT = TEMP_IDENT$
	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$

	!
	! Load In The Form
	!
	GOSUB LoadForm

	!
	! Go check out aligment routine
	!
	GOSUB Alignment

	%PAGE

2000	!*******************************************************************
	! Read through header file
	!*******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PO_ORDERJOUR.CH%, KEY #K_NUM%
		ELSE
			FIND #PO_ORDERJOUR.CH%, KEY #K_NUM% GE FROM_ITEM$
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PO_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
2010	WHEN ERROR IN
		GET #PO_ORDERJOUR.CH%
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PO_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

	!
	! Check for end item
	!
	SELECT SORTBY$

	CASE "N"
		GOTO ExitProgram IF PO_ORDERJOUR::PO > TO_ITEM$ &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PO_ORDERJOUR::PO, -1%), &
			WLDCRD$) = 0%

	CASE "V"
		GOTO ExitProgram IF PO_ORDERJOUR::VENDOR > TO_ITEM$ &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PO_ORDERJOUR::VENDOR, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO ExitProgram IF PO_ORDERJOUR::POTYPE > TO_ITEM$ &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(PO_ORDERJOUR::POTYPE, -1%), &
			WLDCRD$) = 0%

	END SELECT

	!
	! Skip if non-printing
	!
	GOTO GetNextRec IF PO_ORDERJOUR::PRINTFORM = "Y"

2050	!
	! Set the Order Journal Print Flag to "Y"
	!
	IF PO_ORDERJOUR::PRINTFORM <> "Y"
	THEN
		PO_ORDERJOUR::PRINTFORM = "Y"

		WHEN ERROR IN
			UPDATE #PO_ORDERJOUR.CH%
		USE
			FILENAME$ = "PO_ORDERJOUR"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Create an address line format that reduces white space
	!
	I% = 0%

	IF EDIT$(PO_ORDERJOUR::TOADD1, -1%) <> ""
	THEN
		I% = I% + 1%
		PO_ORDERJOUR.TOADDLINE$(I%) = &
			EDIT$(PO_ORDERJOUR::TOADD1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(PO_ORDERJOUR::TOADD2, -1%) <> ""
	THEN
		I% = I% + 1%
		PO_ORDERJOUR.TOADDLINE$(I%) = &
			EDIT$(PO_ORDERJOUR::TOADD2, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%

	PO_ORDERJOUR.TOADDLINE$(I%) = &
		EDIT$(EDIT$(PO_ORDERJOUR::TOCITY, 128%) + ", " + &
		PO_ORDERJOUR::TOSTATE + " " + PO_ORDERJOUR::TOZIP + " " + &
		PO_ORDERJOUR::TOCOUNTRY, 8% + 16% + 32% + 128%)

	PO_ORDERJOUR.TOADDLINE$(LOOP%) = "" &
		FOR LOOP% = I% + 1% TO 3%	! Blank 'em out

	!
	! Get the records that match in the tables
	! Accounts Payable Vendor File
	!
	V% = AP_EXAM_VENDOR(PO_ORDERJOUR::VENDOR, AP_VENDOR_EXAM)

	!
	! Create an address line format that reduces white space
	!
	I% = 0%

	IF EDIT$(AP_VENDOR_EXAM::ADD1, -1%) <> ""
	THEN
		I% = I% + 1%
		AP_VENDOR.ADDLINE$(I%) = &
			EDIT$(AP_VENDOR_EXAM::ADD1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(AP_VENDOR_EXAM::ADD2, -1%) <> ""
	THEN
		I% = I% + 1%
		AP_VENDOR.ADDLINE$(I%) = &
			EDIT$(AP_VENDOR_EXAM::ADD2, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%

	AP_VENDOR.ADDLINE$(I%) = &
		EDIT$(EDIT$(AP_VENDOR_EXAM::CITY, 128%) + ", " + &
		AP_VENDOR_EXAM::STATE + " " + AP_VENDOR_EXAM::ZIP + " " + &
		AP_VENDOR_EXAM::COUNTRY, 8% + 16% + 32% + 128%)

	AP_VENDOR.ADDLINE$(LOOP%) = "" &
		FOR LOOP% = I% + 1% TO 3%	! Blank 'em out

	I% = 0%

	IF EDIT$(AP_VENDOR_EXAM::POADD1, -1%) <> ""
	THEN
		I% = I% + 1%
		AP_VENDOR.POADDLINE$(I%) = &
			EDIT$(AP_VENDOR_EXAM::POADD1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(AP_VENDOR_EXAM::POADD2, -1%) <> ""
	THEN
		I% = I% + 1%
		AP_VENDOR.POADDLINE$(I%) = &
			EDIT$(AP_VENDOR_EXAM::POADD2, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%

	AP_VENDOR.POADDLINE$(I%) = &
		EDIT$(EDIT$(AP_VENDOR_EXAM::POCITY, 128%) + ", " + &
		AP_VENDOR_EXAM::POSTATE + " " + AP_VENDOR_EXAM::POZIP + " " + &
		AP_VENDOR_EXAM::POCOUNTRY, 8% + 16% + 32% + 128%)

	AP_VENDOR.POADDLINE$(LOOP%) = "" &
		FOR LOOP% = I% + 1% TO 3%	! Blank 'em out

	!
	! Utility Location File
	!
 GetLoc:
	V% = UTL_EXAM_LOCATION(PO_ORDERJOUR::FROMLOCATION, UTL_LOCATION_EXAM)

	I% = 0%

	IF EDIT$(UTL_LOCATION_EXAM::ADDRESS1, -1%) <> ""
	THEN
		I% = I% + 1%
		UTL_LOCATION.ADDLINE$(I%) = &
			EDIT$(UTL_LOCATION_EXAM::ADDRESS1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(UTL_LOCATION_EXAM::ADDRESS2, -1%) <> ""
	THEN
		I% = I% + 1%
		UTL_LOCATION.ADDLINE$(I%) = &
			EDIT$(UTL_LOCATION_EXAM::ADDRESS2, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%

	UTL_LOCATION.ADDLINE$(I%) = &
		EDIT$(EDIT$(UTL_LOCATION_EXAM::CITY, 128%) + ", " + &
		UTL_LOCATION_EXAM::STATE + " " + &
		UTL_LOCATION_EXAM::ZIP + " " + &
		UTL_LOCATION_EXAM::COUNTRY, 8% + 16% + 32% + 128%)

	UTL_LOCATION.ADDLINE$(LOOP%) = "" &
		FOR LOOP% = I% + 1% TO 3%	! Blank 'em out

	I% = 0%

	IF EDIT$(UTL_LOCATION_EXAM::SHPADDRESS1, -1%) <> ""
	THEN
		I% = I% + 1%
		UTL_LOCATION.SHPADDLINE$(I%) = &
			EDIT$(UTL_LOCATION_EXAM::SHPADDRESS1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(UTL_LOCATION_EXAM::SHPADDRESS2, -1%) <> ""
	THEN
		I% = I% + 1%
		UTL_LOCATION.SHPADDLINE$(I%) = &
			EDIT$(UTL_LOCATION_EXAM::SHPADDRESS2, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%

	UTL_LOCATION.SHPADDLINE$(I%) = &
		EDIT$(EDIT$(UTL_LOCATION_EXAM::SHPCITY, 128%) + ", " + &
		UTL_LOCATION_EXAM::SHPSTATE + " " + &
		UTL_LOCATION_EXAM::SHPZIP + " " + &
		UTL_LOCATION_EXAM::SHPCOUNTRY, 8% + 16% + 32% + 128%)

	UTL_LOCATION.SHPADDLINE$(LOOP%) = "" &
		FOR LOOP% = I% + 1% TO 3%	! Blank 'em out

	!
	! Get ACKNOWLEDGE code
	!
 GetAck:
2100	IF PO_ACKNOWLEDGE::CODE <> PO_ORDERJOUR::ACKNOW
	THEN
		WHEN ERROR IN
			GET #PO_ACKNOWLEDGE.CH%, &
				KEY #0% EQ PO_ORDERJOUR::ACKNOW, &
				REGARDLESS
		USE
			PO_ACKNOWLEDGE::CODE  = PO_ORDERJOUR::ACKNOW
			PO_ACKNOWLEDGE::DESCR = ""

			CONTINUE GetPOtype IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PO_ACKNOWLEDGE"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Get POTYPE code
	!
 GetPOtype:
2200	IF PO_TYPE::POTYPE <> PO_ORDERJOUR::POTYPE
	THEN
		WHEN ERROR IN
			GET #PO_TYPE.CH%, &
				KEY #0% EQ PO_ORDERJOUR::POTYPE, &
				REGARDLESS
		USE
			PO_TYPE::POTYPE = PO_ORDERJOUR::POTYPE
			PO_TYPE::DESCR  = ""

			CONTINUE GetCarrier IF ERR = 155% OR ERR = 9%
			FILENAME$ = "PO_TYPE"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Get CARRIER code
	!
 GetCarrier:
2300	IF UTL_CARRIER::CODE <> PO_ORDERJOUR::CARRIER
	THEN
		WHEN ERROR IN
			GET #UTL_CARRIER.CH%, &
				KEY #0% EQ PO_ORDERJOUR::CARRIER, &
				REGARDLESS
		USE
			UTL_CARRIER::CODE  = PO_ORDERJOUR::CARRIER
			UTL_CARRIER::DESCR = ""

			CONTINUE GetTerms IF ERR = 155% OR ERR = 9%
			FILENAME$ = "UTL_CARRIER"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Get TERMS code
	!
 GetTerms:
2400	IF UTL_TERMS::CODE <> PO_ORDERJOUR::TERMS
	THEN
		WHEN ERROR IN
			GET #UTL_TERMS.CH%, &
				KEY #0% EQ PO_ORDERJOUR::TERMS, &
				REGARDLESS
		USE
			UTL_TERMS::CODE     = PO_ORDERJOUR::TERMS
			UTL_TERMS::DESCR    = ""
			UTL_TERMS::DISCOUNT = 0.0

			CONTINUE GetFOB IF ERR = 155% OR ERR = 9%
			FILENAME$ = "UTL_TERMS"
			CONTINUE HelpError
		END WHEN
	END IF

	!
	! Get FOB code
	!
 GetFOB:
2500	IF UTL_FOB::FOBCODE <> PO_ORDERJOUR::FOB
	THEN
		WHEN ERROR IN
			GET #UTL_FOB.CH%, &
				KEY #0% EQ PO_ORDERJOUR::FOB, &
				REGARDLESS
		USE
			UTL_FOB::FOBCODE = PO_ORDERJOUR::FOB
			UTL_FOB::DESCR   = ""

			CONTINUE 2600 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "UTL_FOB"
			CONTINUE HelpError
		END WHEN
	END IF

2600	GOSUB PrintForm

2700	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetNextRec

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************
	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

	!
	! Erase Display
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	!
	! Change the width
	!
	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, 80%)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 PrintForm:
18000	!***************************************************************
	! Print the form now
	!***************************************************************

	LINE_COUNT% = 0%
	BODY_COUNT% = 0%
	ORDER_TOTAL = 0.0
	PAGE_NUMBER% = 1%

18010	!
	! Print the top of form
	!
	LINE_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

18030	!
	! Check out the lines
	!
	WHEN ERROR IN
		FIND #PO_ORDERLINE.CH%, &
			KEY #0% GE PO_ORDERJOUR::PO, &
			REGARDLESS
	USE
		CONTINUE 18090 IF ERR = 155% OR ERR = 11%
		FILENAME$ = "PO_ORDERLINE"
		CONTINUE HelpError
	END WHEN


 ReadLine:
	WHEN ERROR IN
		GET #PO_ORDERLINE.CH%, REGARDLESS
	USE
		CONTINUE 18090 IF ERR = 155% OR ERR = 11%
		FILENAME$ = "PO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	GOTO 18090 IF PO_ORDERLINE::PO <> PO_ORDERJOUR::PO

	!
	! See if we can find a bin location for this product
	!
	IC_BINMAP::BIN(I%) = "" FOR I% = 0% TO 3%

18035	WHEN ERROR IN
		GET #IC_BINMAP.CH%, &
			KEY #0% EQ PO_ORDERLINE::OUR_PRODUCT + &
			PO_ORDERJOUR::FROMLOCATION, &
			REGARDLESS
	USE
		CONTINUE 18037 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

18037	!
	! Look for a vendor part cross reference record
	!
	WHEN ERROR IN
		GET #PO_PARTCROSS.CH%, &
			KEY #1% EQ PO_PARTCROSS::VENDOR + &
			PO_PARTCROSS::PRODUCT, &
			REGARDLESS
	USE
		PO_PARTCROSS::PRODUCT = PO_ORDERLINE::OUR_PRODUCT
		PO_PARTCROSS::VENDOR = PO_ORDERJOUR::VENDOR
		PO_PARTCROSS::VENPROD = PO_ORDERLINE::VEN_PRODUCT
		PO_PARTCROSS::VENUOM = PO_ORDERLINE::OUR_UOM
		PO_PARTCROSS::VENFAC = 1.0
		PO_PARTCROSS::FACTOR = 1.0
		PO_PARTCROSS::DESCR = PO_ORDERLINE::DESCRIPTION
		PO_PARTCROSS::LEAD = 0%
		PO_PARTCROSS::MINQTY = 0.0
		PO_PARTCROSS::PRIORITY = ""
	END WHEN

18038	!
	! Skip to a new page if necessary
	!
	GOSUB NewPage IF BODY_COUNT% >= BODY_LINES%

	!
	! Print a line
	!
	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BODY%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

18040	!
	! Get the sublines
	!
	WHEN ERROR IN
		FIND #PO_ORDERSLINE.CH%, &
			KEY #0% GE PO_ORDERLINE::PO + &
			PO_ORDERLINE::PO_LINE, &
			REGARDLESS
	USE
		CONTINUE ReadLine IF ERR = 155% OR ERR = 11%
		FILENAME$ = "PO_ORDERSLINE"
		CONTINUE HelpError
	END WHEN

 ReadSubLine:
	WHEN ERROR IN
		GET #PO_ORDERSLINE.CH%, REGARDLESS
	USE
		CONTINUE ReadLine IF ERR = 155% OR ERR = 11%
		FILENAME$ = "PO_ORDERSLINE"
		CONTINUE HelpError
	END WHEN

	GOTO ReadLine IF PO_ORDERSLINE::PO <> PO_ORDERLINE::PO OR &
		PO_ORDERSLINE::PO_LINE <> PO_ORDERLINE::PO_LINE

	!
	! Calculate line total
	!
	LINE_TOTAL = FUNC_ROUND(PO_ORDERLINE::VEN_PRICE * &
		PO_ORDERSLINE::OUR_QTY, 2%)

	ORDER_TOTAL = ORDER_TOTAL + LINE_TOTAL

	!
	! Skip to a new page if necessary
	!
	GOSUB NewPage IF BODY_COUNT% >= BODY_LINES%

	!
	! Print a subline
	!
	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_SUBLINES%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	!
	! Print line notes
	!
	IF TRM$(PO_ORDERSLINE::NOTES(0%)) <> ""
	THEN
		!
		! Skip to a new page if necessary
		!
		GOSUB NewPage IF BODY_COUNT% >= BODY_LINES%

		BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_LNOTES%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)
	END IF

	GOTO ReadSubLine

18090	IF TRM$(PO_ORDERJOUR::NOTE(0%)) <> ""
	THEN
		!
		! Skip to a new page if necessary
		!
		GOSUB NewPage IF BODY_COUNT% >= BODY_LINES%

		!
		! Print notes in the header lines
		!
		BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_HNOTES%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)
	END IF

	CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
		FOR I% = BODY_COUNT% + 1% TO BODY_LINES%

	LINE_COUNT% = LINE_COUNT% + BODY_LINES%

	LAST_PAGE% = 1%

	!
	! Print the bottom of form
	!
	LINE_COUNT% = LINE_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BOTTOM%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	!
	! Print lines to botton of the form
	!
	CALL OUTP_NEWPAGE(UTL_REPORTX)

18100	RETURN

	%PAGE

	!*******************************************************************
	! Goto a new page
	!*******************************************************************
 NewPage:
	CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
		FOR I% = BODY_COUNT% + 1% TO BODY_LINES%

	LINE_COUNT% = LINE_COUNT% + BODY_LINES%

	LAST_PAGE% = 0%

	!
	! Print the bottom of form
	!
	LINE_COUNT% = LINE_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BOTTOM%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	!
	! Print lines to botton of the form
	!
	CALL OUTP_NEWPAGE(UTL_REPORTX)

	!
	! Print the top of form
	!
	PAGE_NUMBER% = PAGE_NUMBER% + 1%

	LINE_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	BODY_COUNT% = 0%

	RETURN

	%PAGE

 LoadForm:
	!*******************************************************************
	! Initilize form
	!*******************************************************************

	!
	! Get form from the PO form library
	!
	SMG_STATUS% = OUTP_FORMINIT( &
		PO_FORM.DEV$ + "PO_FORM", REPORT$, &
		FORM_TEXT$, FORM_GROUP%, FORM_GROUP())

	!
	! Was there an error?
	!
	IF SMG_STATUS% <> 0%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "order form is missing", "E", &
			SCOPE::PRG_PROGRAM, REPORT$,NUM1$(SMG_STATUS%))

		GOTO ExitProgram
	END IF

	!
	! Search for the desired parts of the form
	!
	FRM_TOP% = 0%
	FRM_BODY% = 0%
	FRM_BOTTOM% = 0%
	FRM_SUBLINES% = 0%
	FRM_LNOTES% = 0%
	FRM_HNOTES% = 0%

	FOR I% = 1% TO FORM_GROUP%

		SELECT FORM_GROUP(I%)::FGROUP

		CASE "FRM-TOP"
			FRM_TOP% = I%

		CASE "FRM-BODY"
			FRM_BODY% = I%

			BODY_LINES% = FORM_GROUP(I%)::NUMBER

		CASE "FRM-SUBLINES"
			FRM_SUBLINES% = I%

			BODY_LINES% = FORM_GROUP(I%)::NUMBER &
				IF FRM_BODY% = 0%

		CASE "FRM-LNOTES"
			FRM_LNOTES% = I%

		CASE "FRM-HNOTES"
			FRM_HNOTES% = I%

		CASE "FRM-BOTTOM"
			FRM_BOTTOM% = I%

		END SELECT

	NEXT I%

	RETURN

	%PAGE

 Alignment:
	!*******************************************************************
	! Print alignment form, if desireable
	!*******************************************************************

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	PAGE_NUMBER% = 1%
	LINE_COUNT% = 0%
	BODY_COUNT% = 0%

	UTL_REPORTX::LINENO = 0%
	UTL_REPORTX::PAGENO = 0%

	SCOPE::PRG_ITEM = "ALIGNMENT"
	!++
	! Abstract:ALIGNMENT
	!
	!
	! Index:
	!
	!
	!--

	JUNK$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Do you want an alignment form?  Confirm then press <Do> ", &
		"N", 0%, "'E", "")

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	SELECT SCOPE::SCOPE_EXIT

	!
	! An exit key was typed
	!
	CASE 3%, SMG$K_TRM_CTRLZ, SMG$K_TRM_F10
		GOTO ExitProgram

	!
	! Return, etc. act as next screen
	!
	CASE 10%, 12%, 13%, SMG$K_TRM_F7, SMG$K_TRM_DO

	!
	! Case else
	!
	CASE ELSE
		CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
		GOTO Alignment

	END SELECT

	GOTO AlignmentReturn IF JUNK$ <> "Y"

	!
	! Print the top of the form
	!
	LINE_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%)

	FOR I% = 1% TO 3%

		BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_BODY%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			1%)

		BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_LNOTES%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			1%)

	NEXT I%

	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_HNOTES%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%)

	CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
		FOR I% = BODY_COUNT% + 1% TO BODY_LINES%

	LINE_COUNT% = LINE_COUNT% + BODY_LINES%

	!
	! Display BOTTOM
	!
	LINE_COUNT% = LINE_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BOTTOM%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%)

	!
	! Print lines to bottom of the form
	!
	CALL OUTP_NEWPAGE(UTL_REPORTX)

	!
	! Do they need another?
	!
	GOTO Alignment

 AlignmentReturn:
	RETURN

	%PAGE

19000	!*******************************************************************
	! Trap errors
	!*******************************************************************

	!
	! Untrapped error
	!
	RESUME HelpError

 HelpError:
19990	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

	!
	! Handle the cases where a file couldn't be opened
	!
	END

20000	SUB FORM_LOADVAR(VARNAME$, REALVALUE, TEXTVALUE$)

	!*******************************************************************
	! This function is used to return the value of a field back to the
	! form printer.
	!*******************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! Define maps
	!
	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERJOUR.HB"
	MAP (PO_ORDERJOUR)	PO_ORDERJOUR_CDD	PO_ORDERJOUR

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERLINE.HB"
	MAP (PO_ORDERLINE)	PO_ORDERLINE_CDD	PO_ORDERLINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERSLINE.HB"
	MAP (PO_ORDERSLINE)	PO_ORDERSLINE_CDD	PO_ORDERSLINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_ACKNOWLEDGE.HB"
	MAP (PO_ACKNOWLEDGE)	PO_ACKNOWLEDGE_CDD	PO_ACKNOWLEDGE

	%INCLUDE "SOURCE:[PO.OPEN]PO_CATEGORY.HB"
	MAP (PO_CATEGORY)	PO_CATEGORY_CDD		PO_CATEGORY

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	COM (AP_VENDOR)		AP_VENDOR_CDD		AP_VENDOR_EXAM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	COM (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION_EXAM

	%INCLUDE "SOURCE:[PO.OPEN]PO_TYPE.HB"
	MAP (PO_TYPE)		PO_TYPE_CDD		PO_TYPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.HB"
	MAP (UTL_CARRIER)	UTL_CARRIER_CDD		UTL_CARRIER

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS)		UTL_TERMS_CDD		UTL_TERMS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_FOB.HB"
	MAP (UTL_FOB)		UTL_FOB_CDD		UTL_FOB

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.HB"
	MAP (PO_PARTCROSS)	PO_PARTCROSS_CDD	PO_PARTCROSS

	MAP (JOUR_FORM) &
		ORDER_TOTAL, &
		LINE_TOTAL, &
		PAGE_NUMBER%, &
		LAST_PAGE%, &
		AP_VENDOR.ADDLINE$(3%) = 50%, &
		AP_VENDOR.POADDLINE$(3%) = 50%, &
		PO_ORDERJOUR.TOADDLINE$(3%) = 50%, &
		UTL_LOCATION.ADDLINE$(3%) = 50%, &
		UTL_LOCATION.SHPADDLINE$(3%) = 50%

	COM (PO_ACKNOWLEDGE_CH) PO_ACKNOWLEDGE.CH%
	COM (PO_CATEGORY_CH)	PO_CATEGORY.CH%
	COM (PO_TYPE_CH)	PO_TYPE.CH%
	COM (UTL_CARRIER_CH)	UTL_CARRIER.CH%
	COM (UTL_TERMS_CH)	UTL_TERMS.CH%
	COM (UTL_FOB_CH)	UTL_FOB.CH%

	%PAGE

	!
	! Set up default values
	!
	REALVALUE = 0.0
	TEXTVALUE$ = "????????"

	!
	! Pick by variable
	!
	SELECT VARNAME$
	!************************************************************
	! Fields for the AP_VENDOR file
	!************************************************************

	CASE "AP_VENDOR::VENNUM"
		TEXTVALUE$ = AP_VENDOR_EXAM::VENNUM

	CASE "AP_VENDOR::VENNAM"
		TEXTVALUE$ = AP_VENDOR_EXAM::VENNAM

	CASE "AP_VENDOR::ADD1"
		TEXTVALUE$ = AP_VENDOR_EXAM::ADD1

	CASE "AP_VENDOR::ADD2"
		TEXTVALUE$ = AP_VENDOR_EXAM::ADD2

	CASE "AP_VENDOR::CITY"
		TEXTVALUE$ = AP_VENDOR_EXAM::CITY

	CASE "AP_VENDOR::STATE"
		TEXTVALUE$ = AP_VENDOR_EXAM::STATE

	CASE "AP_VENDOR::ZIP"
		TEXTVALUE$ = AP_VENDOR_EXAM::ZIP

	CASE "AP_VENDOR::COUNTRY"
		TEXTVALUE$ = AP_VENDOR_EXAM::COUNTRY

	CASE "AP_VENDOR.ADDLINE1"	! Substitute Vendor Address
		TEXTVALUE$ = AP_VENDOR.ADDLINE$(1%)

	CASE "AP_VENDOR.ADDLINE2"	! Substitute Vendor Address
		TEXTVALUE$ = AP_VENDOR.ADDLINE$(2%)

	CASE "AP_VENDOR.ADDLINE3"	! Substitute Vendor Address
		TEXTVALUE$ = AP_VENDOR.ADDLINE$(3%)

	CASE "AP_VENDOR::PHONE"
		TEXTVALUE$ = AP_VENDOR_EXAM::PHONE

	CASE "AP_VENDOR::POADD1"
		TEXTVALUE$ = AP_VENDOR_EXAM::POADD1

	CASE "AP_VENDOR::POADD2"
		TEXTVALUE$ = AP_VENDOR_EXAM::POADD2

	CASE "AP_VENDOR::POCITY"
		TEXTVALUE$ = AP_VENDOR_EXAM::POCITY

	CASE "AP_VENDOR::POSTATE"
		TEXTVALUE$ = AP_VENDOR_EXAM::POSTATE

	CASE "AP_VENDOR::POZIP"
		TEXTVALUE$ = AP_VENDOR_EXAM::POZIP

	CASE "AP_VENDOR::POCOUNTRY"
		TEXTVALUE$ = AP_VENDOR_EXAM::POCOUNTRY

	CASE "AP_VENDOR.POADDLINE1"	! Substitute Vendor Address
		TEXTVALUE$ = AP_VENDOR.POADDLINE$(1%)

	CASE "AP_VENDOR.POADDLINE2"	! Substitute Vendor Address
		TEXTVALUE$ = AP_VENDOR.POADDLINE$(2%)

	CASE "AP_VENDOR.POADDLINE3"	! Substitute Vendor Address
		TEXTVALUE$ = AP_VENDOR.POADDLINE$(3%)

	CASE "AP_VENDOR::POPHONE"
		TEXTVALUE$ = AP_VENDOR_EXAM::POPHONE

	CASE "AP_VENDOR::PURGE"
		TEXTVALUE$ = AP_VENDOR_EXAM::PURGE

	CASE "AP_VENDOR::FEDID"
		TEXTVALUE$ = AP_VENDOR_EXAM::FEDID

	CASE "AP_VENDOR::FLG1099"
		TEXTVALUE$ = AP_VENDOR_EXAM::FLG1099

	CASE "AP_VENDOR::DUEDAYS"
		REALVALUE = AP_VENDOR_EXAM::DUEDAYS

	CASE "AP_VENDOR::DUEDATE"
		TEXTVALUE$ = AP_VENDOR_EXAM::DUEDATE

	CASE "AP_VENDOR::DISDAYS"
		REALVALUE = AP_VENDOR_EXAM::DISDAYS

	CASE "AP_VENDOR::DISDATE"
		TEXTVALUE$ = AP_VENDOR_EXAM::DISDATE

	CASE "AP_VENDOR::DISCPER"
		REALVALUE = AP_VENDOR_EXAM::DISCPER

	CASE "AP_VENDOR::ALPSRT"
		TEXTVALUE$ = AP_VENDOR_EXAM::ALPSRT

	!************************************************************
	! Fields for the IC_BINMAPfile
	!************************************************************

	CASE "IC_BINMAP::PRODUCT"
		TEXTVALUE$ = IC_BINMAP::PRODUCT

	CASE "IC_BINMAP::LOCATION"
		TEXTVALUE$ = IC_BINMAP::LOCATION

	CASE "IC_BINMAP::BIN1"
		TEXTVALUE$ = IC_BINMAP::BIN(0%)

	CASE "IC_BINMAP::BIN2"
		TEXTVALUE$ = IC_BINMAP::BIN(1%)

	CASE "IC_BINMAP::BIN3"
		TEXTVALUE$ = IC_BINMAP::BIN(2%)

	CASE "IC_BINMAP::BIN4"
		TEXTVALUE$ = IC_BINMAP::BIN(3%)

	CASE "IC_BINMAP::SAFETY"
		REALVALUE = IC_BINMAP::SAFETY

	CASE "IC_BINMAP::MAXLEVEL"
		REALVALUE = IC_BINMAP::MAXLEVEL

	CASE "IC_BINMAP::ABC"
		TEXTVALUE$ = IC_BINMAP::ABC

	CASE "IC_BINMAP::CYCLEMAP"
		TEXTVALUE$ = IC_BINMAP::CYCLEMAP

	!************************************************************
	! Fields for the PO_ORDERJOUR file
	!************************************************************

	CASE "PO_ORDERJOUR::PO"
		TEXTVALUE$ = CONV_STRING(PO_ORDERJOUR::PO, CMC$_LEFT)

	CASE "PO_ORDERJOUR::POTYPE"
		TEXTVALUE$ = PO_ORDERJOUR::POTYPE

	CASE "PO_ORDERJOUR::PODATE"
		TEXTVALUE$ = PRNT_DATE(PO_ORDERJOUR::PODATE, 8%)

	CASE "PO_ORDERJOUR::PODATE6"
		TEXTVALUE$ = PRNT_DATE(PO_ORDERJOUR::PODATE, 6%)

	CASE "PO_ORDERJOUR::VENDOR"
		TEXTVALUE$ = PO_ORDERJOUR::VENDOR

	CASE "PO_ORDERJOUR::BUYER"
		TEXTVALUE$ = PO_ORDERJOUR::BUYER

	CASE "PO_ORDERJOUR::SALESMAN"
		TEXTVALUE$ = PO_ORDERJOUR::SALESMAN

	CASE "PO_ORDERJOUR::TERMS"
		TEXTVALUE$ = PO_ORDERJOUR::TERMS

	CASE "PO_ORDERJOUR::CARRIER"
		TEXTVALUE$ = PO_ORDERJOUR::CARRIER

	CASE "PO_ORDERJOUR::FOB"
		TEXTVALUE$ = PO_ORDERJOUR::FOB

	CASE "PO_ORDERJOUR::ACKNOW"
		TEXTVALUE$ = PO_ORDERJOUR::ACKNOW

	CASE "PO_ORDERJOUR::COL_PPD"
		TEXTVALUE$ = PO_ORDERJOUR::COL_PPD

	CASE "PO_ORDERJOUR::NOTE1"
		TEXTVALUE$ = PO_ORDERJOUR::NOTE(0%)

	CASE "PO_ORDERJOUR::NOTE2"
		TEXTVALUE$ = PO_ORDERJOUR::NOTE(1%)

	CASE "PO_ORDERJOUR::NOTE3"
		TEXTVALUE$ = PO_ORDERJOUR::NOTE(2%)

	CASE "PO_ORDERJOUR::NOTE4"
		TEXTVALUE$ = PO_ORDERJOUR::NOTE(3%)

	CASE "PO_ORDERJOUR::OPERATOR"
		TEXTVALUE$ = PO_ORDERJOUR::OPERATOR

	CASE "PO_ORDERJOUR::PRINTFORM"
		TEXTVALUE$ = PO_ORDERJOUR::PRINTFORM

	CASE "PO_ORDERJOUR::FROMLOCATION"
		TEXTVALUE$ = PO_ORDERJOUR::FROMLOCATION

	CASE "PO_ORDERJOUR::TONAME"
		TEXTVALUE$ = PO_ORDERJOUR::TONAME

	CASE "PO_ORDERJOUR::TOADD1"
		TEXTVALUE$ = PO_ORDERJOUR::TOADD1

	CASE "PO_ORDERJOUR::TOADD2"
		TEXTVALUE$ = PO_ORDERJOUR::TOADD2

	CASE "PO_ORDERJOUR::TOCITY"
		TEXTVALUE$ = PO_ORDERJOUR::TOCITY

	CASE "PO_ORDERJOUR::TOSTATE"
		TEXTVALUE$ = PO_ORDERJOUR::TOSTATE

	CASE "PO_ORDERJOUR::TOZIP"
		TEXTVALUE$ = PO_ORDERJOUR::TOZIP

	CASE "PO_ORDERJOUR::TOCOUNTRY"
		TEXTVALUE$ = PO_ORDERJOUR::TOCOUNTRY

	CASE "PO_ORDERJOUR.TOADDLINE1"	! Substitute Vendor Address
		TEXTVALUE$ = PO_ORDERJOUR.TOADDLINE$(1%)

	CASE "PO_ORDERJOUR.TOADDLINE2"	! Substitute Vendor Address
		TEXTVALUE$ = PO_ORDERJOUR.TOADDLINE$(2%)

	CASE "PO_ORDERJOUR.TOADDLINE3"	! Substitute Vendor Address
		TEXTVALUE$ = PO_ORDERJOUR.TOADDLINE$(3%)

	CASE "PO_ORDERJOUR::BATCH"
		TEXTVALUE$ = PO_ORDERJOUR::BATCH

	!************************************************************
	! Fields for the PO_ORDERLINE file
	!************************************************************

	CASE "PO_ORDERLINE::PO"
		TEXTVALUE$ = CONV_STRING(PO_ORDERLINE::PO, CMC$_LEFT)

	CASE "PO_ORDERLINE::PO_LINE"
		TEXTVALUE$ = PO_ORDERLINE::PO_LINE

	CASE "PO_ORDERLINE::OUR_PRODUCT"
		TEXTVALUE$ = PO_ORDERLINE::OUR_PRODUCT

	CASE "PO_ORDERLINE::OUR_UOM"
		TEXTVALUE$ = PO_ORDERLINE::OUR_UOM

	CASE "PO_ORDERLINE::VEN_PRODUCT"
		TEXTVALUE$ = PO_ORDERLINE::VEN_PRODUCT

	CASE "PO_ORDERLINE::DESCRIPTION"
		TEXTVALUE$ = PO_ORDERLINE::DESCRIPTION

	CASE "PO_ORDERLINE::VEN_PRICE"
		REALVALUE = PO_ORDERLINE::VEN_PRICE

	!************************************************************
	! Fields for the PO_ORDERSLINE file
	!************************************************************

	CASE "PO_ORDERSLINE::PO"
		TEXTVALUE$ = CONV_STRING(PO_ORDERSLINE::PO, CMC$_LEFT)

	CASE "PO_ORDERSLINE::PO_LINE"
		TEXTVALUE$ = PO_ORDERSLINE::PO_LINE

	CASE "PO_ORDERSLINE::OUR_QTY"
		REALVALUE = PO_ORDERSLINE::OUR_QTY

	CASE "PO_ORDERSLINE::RECEIVEDATE"
		TEXTVALUE$ = PRNT_DATE(PO_ORDERSLINE::RECEIVEDATE, 8%)

	CASE "PO_ORDERSLINE::RECEIVEDATE6"
		TEXTVALUE$ = PRNT_DATE(PO_ORDERSLINE::RECEIVEDATE, 6%)

	CASE "PO_ORDERSLINE::GL_ACCOUNT"
		TEXTVALUE$ = PO_ORDERSLINE::GL_ACCOUNT

	CASE "PO_ORDERSLINE::SUBACCT"
		TEXTVALUE$ = PO_ORDERSLINE::SUBACCT

	CASE "PO_ORDERSLINE::NOTES1"
		TEXTVALUE$ = PO_ORDERSLINE::NOTES(0%)

	CASE "PO_ORDERSLINE::NOTES2"
		TEXTVALUE$ = PO_ORDERSLINE::NOTES(1%)

	!************************************************************
	! Fields for the PO_ACKNOWLEDGE file
	!************************************************************

	CASE "PO_ACKNOWLEDGE::CODE"
		TEXTVALUE$ = PO_ACKNOWLEDGE::CODE

	CASE "PO_ACKNOWLEDGE::DESCR"
		TEXTVALUE$ = PO_ACKNOWLEDGE::DESCR

	!************************************************************
	! Fields for the PO_CATEGORY file
	!************************************************************

	CASE "PO_CATEGORY::CODE"
		TEXTVALUE$ = PO_ACKNOWLEDGE::CODE

	CASE "PO_CATEGORY::DESCR"
		TEXTVALUE$ = PO_ACKNOWLEDGE::DESCR

	!************************************************************
	! Purchase Order Type
	!************************************************************

	CASE "PO_TYPE::POTYPE"
		TEXTVALUE$ = PO_TYPE::POTYPE

	CASE "PO_TYPE::DESCR"
		TEXTVALUE$ = PO_TYPE::DESCR

	!************************************************************
	! FOB code table
	!************************************************************

	CASE "UTL_FOB::FOBCODE"
		TEXTVALUE$ = UTL_FOB::FOBCODE

	CASE "UTL_FOB::DESCR"
		TEXTVALUE$ = UTL_FOB::DESCR

	!************************************************************
	! Carrier
	!************************************************************

	CASE "UTL_CARRIER::CODE"
		TEXTVALUE$ = UTL_CARRIER::CODE

	CASE "UTL_CARRIER::DESCR"
		TEXTVALUE$ = UTL_CARRIER::DESCR

	!************************************************************
	! Terms
	!************************************************************

	CASE "UTL_TERMS::CODE"
		TEXTVALUE$ = UTL_TERMS::CODE

	CASE "UTL_TERMS::DESCR"
		TEXTVALUE$ = UTL_TERMS::DESCR

	CASE "UTL_TERMS::DISCOUNT"
		REALVALUE = UTL_TERMS::DISCOUNT

	!************************************************************
	! Fields for the UTL_LOCATION file
	!************************************************************

	CASE "UTL_LOCATION::LOCATION"
		TEXTVALUE$ = UTL_LOCATION_EXAM::LOCATION

	CASE "UTL_LOCATION::LOCNAME"
		TEXTVALUE$ = UTL_LOCATION_EXAM::LOCNAME

	CASE "UTL_LOCATION::REGION"
		TEXTVALUE$ = UTL_LOCATION_EXAM::REGION

	CASE "UTL_LOCATION::LOCGROUP"
		TEXTVALUE$ = UTL_LOCATION_EXAM::LOCGROUP

	CASE "UTL_LOCATION::ADDRESS1"
		TEXTVALUE$ = UTL_LOCATION_EXAM::ADDRESS1

	CASE "UTL_LOCATION::ADDRESS2"
		TEXTVALUE$ = UTL_LOCATION_EXAM::ADDRESS2

	CASE "UTL_LOCATION::CITY"
		TEXTVALUE$ = UTL_LOCATION_EXAM::CITY

	CASE "UTL_LOCATION::STATE"
		TEXTVALUE$ = UTL_LOCATION_EXAM::STATE

	CASE "UTL_LOCATION::ZIP"
		TEXTVALUE$ = UTL_LOCATION_EXAM::ZIP

	CASE "UTL_LOCATION::COUNTY"
		TEXTVALUE$ = UTL_LOCATION_EXAM::COUNTY

	CASE "UTL_LOCATION::COUNTRY"
		TEXTVALUE$ = UTL_LOCATION_EXAM::COUNTRY

	CASE "UTL_LOCATION.ADDLINE1"	! Substitute Location Address
		TEXTVALUE$ = UTL_LOCATION.ADDLINE$(1%)

	CASE "UTL_LOCATION.ADDLINE2"	! Substitute Location Address
		TEXTVALUE$ = UTL_LOCATION.ADDLINE$(2%)

	CASE "UTL_LOCATION.ADDLINE3"	! Substitute Location Address
		TEXTVALUE$ = UTL_LOCATION.ADDLINE$(3%)

	CASE "UTL_LOCATION::PHONE"
		TEXTVALUE$ = UTL_LOCATION_EXAM::PHONE

	CASE "UTL_LOCATION::SHPADDRESS1"
		TEXTVALUE$ = UTL_LOCATION_EXAM::SHPADDRESS1

	CASE "UTL_LOCATION::SHPADDRESS2"
		TEXTVALUE$ = UTL_LOCATION_EXAM::SHPADDRESS2

	CASE "UTL_LOCATION::SHPCITY"
		TEXTVALUE$ = UTL_LOCATION_EXAM::SHPCITY

	CASE "UTL_LOCATION::SHPSTATE"
		TEXTVALUE$ = UTL_LOCATION_EXAM::SHPSTATE

	CASE "UTL_LOCATION::SHPZIP"
		TEXTVALUE$ = UTL_LOCATION_EXAM::SHPZIP

	CASE "UTL_LOCATION::SHPCOUNTY"
		TEXTVALUE$ = UTL_LOCATION_EXAM::SHPCOUNTY

	CASE "UTL_LOCATION::SHPCOUNTRY"
		TEXTVALUE$ = UTL_LOCATION_EXAM::SHPCOUNTRY

	CASE "UTL_LOCATION.SHPADDLINE1"	! Substitute Location Shipping Address
		TEXTVALUE$ = UTL_LOCATION.SHPADDLINE$(1%)

	CASE "UTL_LOCATION.SHPADDLINE2"	! Substitute Location Shipping Address
		TEXTVALUE$ = UTL_LOCATION.SHPADDLINE$(2%)

	CASE "UTL_LOCATION.SHPADDLINE3"	! Substitute Location Shipping Address
		TEXTVALUE$ = UTL_LOCATION.SHPADDLINE$(3%)

	CASE "UTL_LOCATION::SHPPHONE"
		TEXTVALUE$ = UTL_LOCATION_EXAM::SHPPHONE

	!************************************************************
	! Non fielded values
	!************************************************************

	CASE "ORDER_TOTAL"
		REALVALUE = ORDER_TOTAL

	CASE "LINE_TOTAL"
		REALVALUE = LINE_TOTAL

	CASE "PAGE_NUMBER"
		REALVALUE = PAGE_NUMBER%
		TEXTVALUE$ = NUM1$(PAGE_NUMBER%)

	CASE "LAST_PAGE"
		REALVALUE = LAST_PAGE%

	!************************************************************
	! PO_PARTCROSS
	!************************************************************

	CASE "PO_PARTCROSS::PRODUCT"
		TEXTVALUE$ = PO_PARTCROSS::PRODUCT

	CASE "PO_PARTCROSS::VENDOR"
		TEXTVALUE$ = PO_PARTCROSS::VENDOR

	CASE "PO_PARTCROSS::PRODUCT"
		TEXTVALUE$ = PO_PARTCROSS::PRODUCT

	CASE "PO_PARTCROSS::VENUOM"
		TEXTVALUE$ = PO_PARTCROSS::VENUOM

	CASE "PO_PARTCROSS::VENFAC"
		REALVALUE = PO_PARTCROSS::VENFAC
		TEXTVALUE$ = NUM1$(PO_PARTCROSS::VENFAC)

	CASE "PO_PARTCROSS::FACTOR"
		REALVALUE = PO_PARTCROSS::FACTOR
		TEXTVALUE$ = NUM1$(PO_PARTCROSS::FACTOR)

	CASE "PO_PARTCROSS::DESCR"
		TEXTVALUE$ = PO_PARTCROSS::DESCR

	CASE "PO_PARTCROSS::LEAD"
		TEXTVALUE$ = NUM1$(PO_PARTCROSS::LEAD)
		REALVALUE = PO_PARTCROSS::LEAD

	CASE "PO_PARTCROSS::MINQTY"
		TEXTVALUE$ = NUM1$(PO_PARTCROSS::MINQTY)
		REALVALUE = PO_PARTCROSS::MINQTY

	CASE "PO_PARTCROSS::PRIORITY"
		TEXTVALUE$ = PO_PARTCROSS::PRIORITY

	END SELECT

	END SUB

