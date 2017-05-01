1	%TITLE "Cycle Count Worksheet"
	%SBTTL "IC_FORM_CYCLESHEET"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1998 BY
	!
	! Software Solutions, Inc
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:IC006
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Cycle Count Worksheet\*
	!	prints a report to be used as a turnaround document to cycle
	!	count inventory items. The report will contain the following
	!	categories:
	!	.table 3,25
	!	.te
	!	Product _#	Bin Location
	!	.te
	!	Description	Unit Of Measure
	!	.te
	!	Type	Category
	!	.te
	!	Pack	Form
	!	.te
	!	Unit Of Measure	Pack Form
	!	.te
	!	Product Form	Standard Cost
	!	.te
	!	Physical Count	Pack Unit
	!	.te
	!	Extend
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Cycle Count Worksheet>Report
	!	.x Cycle Count Worksheet>Report
	!	.x Report>Cycle Count Worksheet
	!	.x Report>Cycle Count Worksheet
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_FORM_CYCLESHEET/LINE
	!	$ LINK/EXE=IC_EXE: IC_FORM_CYCLESHEET, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_FORM_CYCLESHEET.OBJ;*
	!
	!
	! Author:
	!
	!	10/21/98 - Kevin Handy
	!		Based on IC_RPRT_CYCLESHEET
	!
	! Modification History:
	!
	!	10/26/98 - Kevin Handy
	!		Go with posted balance instead of running balance
	!
	!	10/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add a bunch more error traps
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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "FUNC_INCLUDE:PRINT35.INC"
	MAP (PRINTX) PRINTX_CDD PRINTX

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM			FORM_GROUP_CDD		FORM_GROUP(10%)

	RECORD IC_CYCLESHEET_CDD
		STRING BIN = 6%
		STRING PRODUCT = 14%
	END RECORD

	MAP (IC_CYCLESHEET) IC_CYCLESHEET_CDD IC_CYCLESHEET

	MAP (STMT_FORM) &
		STRING BIN = 6, &
		STRING CONTROL = 8, &
		STRING LOCATION = 4

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION READ_BIT
	EXTERNAL LONG   FUNCTION IC_READ_35BALANCE

	EXTERNAL LONG	FUNCTION OUTP_FORMINIT
	EXTERNAL LONG	FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$ = "ICYCLE"

	!
	! Look up device
	!
	CALL READ_DEVICE("IC_FORM", IC_FORM.DEV$, STAT%)

	!
	! Get Report
	!
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Bin _#\*
	!	.b
	!	.lm +5
	!	The ^*From Bin _#\* setting enters a
	!	bin _# with which the report will begin printing.
	!	.b
	!	A blank setting will cause the report to begin with the
	!	first bin _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Bin Number>Cycle Count Worksheet
	!	.x Cycle Count Worksheet>From Bin Number
	!	.x Bin Number>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Bin _#\*
	!	.b
	!	.lm +5
	!	The ^*To Bin _#\* field enters a bin _#
	!	with which the report will end printing.
	!	.b
	!	A blank field will cause the report to end with the last bin _#
	!	in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Bin Number>Cycle Count Worksheet
	!	.x Cycle Count Worksheet>To Bin Number
	!	.x Bin Number>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated bin
	!	_#'s to be printed by entering a "wildcard" value.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard Bin>Cycle Count Worksheet
	!	.x Cycle Count Worksheet>Wildcard Bin
	!
	!--

	WILDCARD_TYPE$ = TRM$(UTL_REPORTX::OPTDEF(3%))

	!++
	! Abstract:FLD04
	!	^*(03) Wildcard Product Type\*
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!	.x Wildcard Type>Cycle Count Worksheet
	!	.x Cycle Count Worksheet>Wildcard Type
	!
	!--

	LOCATION = LEFT$(UTL_REPORTX::OPTDEF(4%), 4%)

	!++
	! Abstract:FLD05
	!	^*(05) Locations\*
	!	.b
	!	.lm +5
	!	The ^*Locations\* field enters the location
	!	codes (which have been established in the Utility system) that are
	!	to be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Locations>Cycle Count Worksheet
	!	.x Cycle Count Worksheet>Locations
	!
	!--

	TO_WEEK% = VAL%(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) Week Number\*
	!	.b
	!	.lm +5
	!	The ^*Week Number\* field enters the number which will print
	!	a report of the items counted for this week. The flags for the count weeks
	!	are set in the cycle journal. This field is most effective if all inventory
	!	is not counted in the same week, otherwise all reports will be printed.
	!	.lm -5
	!
	! Index:
	!	.x Week Number
	!
	!--

	BEGIN_CONTROL$ = UTL_REPORTX::OPTDEF(6%)

	!++
	! Abstract:FLD07
	!	^*(07) Skip to Control Number\*
	!	.b
	!	.lm +5
	!	Starting control number.
	!	.lm -5
	!
	! Index:
	!	.x Control Number
	!
	!--

	CONTROL = UTL_REPORTX::OPTDEF(7%)

	!++
	! Abstract:FLD08
	!	^*(08) Start Control Number\*
	!	.b
	!	.lm +5
	!	Starting control number.
	!	.lm -5
	!
	! Index:
	!	.x Control Number
	!
	!--

	CONTROL_FILE$ = TRM$(UTL_REPORTX::OPTDEF(8%))

	!++
	! Abstract:FLD09
	!	^*(08) Control Number File\*
	!	.b
	!	.lm +5
	!	Text file to store a cross reference of control numbers.
	!	.lm -5
	!
	! Index:
	!	.x Control Number File
	!
	!--

	REPORT$ = REPORT$ + "$" + TRM$(UTL_REPORTX::OPTDEF(9%))

	!++
	! Abstract:FLD10
	!	^*(10) Form Name\*
	!	.b
	!	.lm +5
	!	The ^*Form Name\* field specifies the
	!	form to use for printing the report.
	!	.b
	!	(For more information on form printing refer to
	!	the Utility Section, Forms Controlling.)
	!	.lm -5
	!
	! Index:
	!	.x Form
	!
	!--

	!
	! Load the form
	!
	GOSUB LoadForm

	!
	! Get type of printer
	!
	IF PRINTX::ITEMS == 0%
	THEN
		CALL OUTP_INITIALIZE(UTL_REPORTX::PRINTTYPE)
	END IF

	CALL ASSG_CHANNEL(IC_CYCLESHEET.CH%, STAT%)
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.OPN"
	USE
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

330	!
	! Create a control file
	!
	CONTROL_FILE.CH% = 0%
	IF CONTROL_FILE$ <> ""
	THEN
		CALL ASSG_CHANNEL(CONTROL_FILE.CH%, STAT%)
		WHEN ERROR IN
			OPEN CONTROL_FILE$ FOR OUTPUT AS FILE CONTROL_FILE.CH%, &
				RECORDSIZE 132%
		USE
			FILENAME$ = "CONTROL.FILE"
			CONTINUE HelpError
		END WHEN
	END IF

340	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, &
			KEY #0% EQ LOCATION, &
			REGARDLESS
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

350	!
	! Create a work file
	!
	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "IC_CYCLESHEET.TMP" FOR OUTPUT AS FILE IC_CYCLESHEET.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP IC_CYCLESHEET, &
			PRIMARY KEY (IC_CYCLESHEET::BIN, &
				IC_CYCLESHEET::PRODUCT) DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "IC_CYCLESHEET.TMP"
		CONTINUE HelpError
	END WHEN

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #PD_PRODUCT.CH%
	USE
		CONTINUE ExitTotal
	END WHEN

17010	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE 17100
	END WHEN

	!
	! Only look at specific types
	!
	IF WILDCARD_TYPE$ <> ""
	THEN
		GOTO 17010 &
			IF COMP_STRING(PD_PRODUCT::PROD_TYPE, &
			WILDCARD_TYPE$) = 0%
	END IF

17020	!
	! Are there any balances for this product
	!
	GOTO 17010 IF 1% AND IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		LOCATION, BALANCE(,)) = 0%

 !	ONHAND = BALANCE(1%, 1%) + BALANCE(1%, 2%) + BALANCE(1%, 3%)
	ONHAND = BALANCE(1%, 1%) + BALANCE(1%, 2%)

	GOTO 17010 IF ABS(ONHAND) < 0.001

17030	!
	! Get any bin locations defined for this product
	!
	WHEN ERROR IN
		GET #IC_BINMAP.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM + LOCATION, &
			REGARDLESS
	USE
		CONTINUE 17040
	END WHEN

17035	IF TO_WEEK% <> 0%
	THEN
		GOTO 17010 IF READ_BIT(8%, IC_BINMAP::CYCLEMAP, TO_WEEK%) = 0%
	END IF

	!
	! Generate a binmap
	!
	MADE_ONE% = 0%
	FOR I% = 0% TO 3%
		IF IC_BINMAP::BIN(I%) <> ""
		THEN
			IF (COMP_STRING(EDIT$(IC_CYCLESHEET::BIN, -1%), WLDCRD$) <> 0% &
				OR WLDCRD$ = "") AND &
				((IC_CYCLESHEET::BIN >= TO_ITEM$) OR &
				TO_ITEM$ = "")
			THEN
				IC_CYCLESHEET::BIN = IC_BINMAP::BIN(I%)
				IC_CYCLESHEET::PRODUCT = PD_PRODUCT::PRODUCT_NUM

				PUT #IC_CYCLESHEET.CH%
				MADE_ONE% = -1%
			END IF

		END IF
	NEXT I%

	GOTO 17010 IF MADE_ONE%

17040	!
	! Don't have a binmap, or the binmap is empty
	!
	IC_CYCLESHEET::BIN = "____"
	IC_CYCLESHEET::PRODUCT = PD_PRODUCT::PRODUCT_NUM

	WHEN ERROR IN
		PUT #IC_CYCLESHEET.CH%
	USE
		FILENAME$ = "IC_CYCLESHEET.TMP"
		CONTINUE HelpError
	END WHEN

	GOTO 17010

 CreateBin:
17100	WHEN ERROR IN
		RESET #IC_CYCLESHEET.CH%
	USE
		FILENAME$ = "IC_CYCLESHEET.TMP"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17120	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #IC_CYCLESHEET.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal
	END WHEN

17200	!
	! Read Product file
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ IC_CYCLESHEET::PRODUCT, &
			REGARDLESS
	USE
		PD_PRODUCT::PRODUCT_NUM = &
			IC_CYCLESHEET::PRODUCT
		PD_PRODUCT::DESCRIPTION = &
			STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)
		PD_PRODUCT::CATEGORY = &
			STRING$(LEN(PD_PRODUCT::CATEGORY), A"?"B)
		PD_PRODUCT::PROD_TYPE = &
			STRING$(LEN(PD_PRODUCT::PROD_TYPE), A"?"B)
		PD_PRODUCT::UOM = &
			STRING$(LEN(PD_PRODUCT::UOM), A"?"B)
		PD_PRODUCT::SSTATUS = &
			STRING$(LEN(PD_PRODUCT::SSTATUS), A"?"B)

		CONTINUE 17210
	END WHEN

17210	!

17300	!
	! Print out one line
	!
	BIN = IC_CYCLESHEET::BIN

	IF CONTROL >= BEGIN_CONTROL$
	THEN
		!
		! Print the top of statement
		!
		LINE_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_BODY%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)

		!
		! Skip to the end of the page
		!
		CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
			FOR I% = LINE_COUNT% + 1% TO FORM_GROUP(FRM_BODY%)::NUMBER
 !		CALL OUTP_NEWPAGE(UTL_REPORTX)
	END IF

	IF CONTROL_FILE.CH% <> 0%
	THEN
		PRINT #CONTROL_FILE.CH%, &
			CONTROL + " " + &
			LOCATION + " " + &
			BIN + " " + &
			PD_PRODUCT::PRODUCT_NUM + " " + &
			PD_PRODUCT::UOM + " " + &
			TRM$(PD_PRODUCT::DESCRIPTION)
	END IF

	TEMP% = FUNC_INCREMENT(CONTROL)

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!

 ExitProgram:

	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)
	CLOSE CONTROL_FILE.CH%

	!
	! Erase Display
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	!
	! Change the width
	!
	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, 80%)

	!
	! Exit to next program or menu
	!
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 LoadForm:
17600	!*******************************************************************
	! Initilize Statement form
	!*******************************************************************

	!
	! Get form from the IC form library
	!
	SMG_STATUS% = OUTP_FORMINIT(IC_FORM.DEV$ + "IC_FORM", REPORT$, &
		FORM_TEXT$, FORM_GROUP%, FORM_GROUP())

	!
	! Was there an error?
	!
	IF SMG_STATUS% <> 0%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "Statement form is missing", &
			"E", SCOPE::PRG_PROGRAM, REPORT$, NUM1$(SMG_STATUS%))

		GOTO ExitProgram
	END IF

	!
	! Search for the desired parts of the form
	!
	FRM_TOP% = 0%
	FRM_BODY% = 0%
	FRM_BOTTOM% = 0%
	FRM_SUBBOTTOM% = 0%

	FOR I% = 1% TO FORM_GROUP%

		SELECT FORM_GROUP(I%)::FGROUP

		CASE "FRM-TOP"
			FRM_TOP% = I%

		CASE "FRM-BODY"
			FRM_BODY% = I%

		CASE "FRM-BOTTOM"
			FRM_BOTTOM% = I%
			FRM_SUBBOTTOM% = I% IF FRM_SUBBOTTOM% = 0%

		CASE "FRM-SUBBOTTOM"
			FRM_SUBBOTTOM% = I%

		END SELECT

	NEXT I%

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

	%Page

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

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
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP (UTL_PROFILE) UTL_PROFILE_CDD UTL_PROFILE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION) UTL_LOCATION_CDD UTL_LOCATION

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	MAP (STMT_FORM) &
		STRING BIN = 6, &
		STRING CONTROL = 8, &
		STRING LOCATION = 4

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

	!*******************************************************************
	! PD_PRODUCT
	!*******************************************************************

	CASE "PD_PRODUCT::PRODUCT_NUM"
		TEXTVALUE$ = PD_PRODUCT::PRODUCT_NUM

	CASE "PD_PRODUCT::DESCRIPTION"
		TEXTVALUE$ = PD_PRODUCT::DESCRIPTION

	CASE "PD_PRODUCT::PROD_TYPE"
		TEXTVALUE$ = PD_PRODUCT::PROD_TYPE

	CASE "PD_PRODUCT::CATEGORY"
		TEXTVALUE$ = PD_PRODUCT::CATEGORY

	CASE "PD_PRODUCT::UOM"
		TEXTVALUE$ = PD_PRODUCT::UOM

	CASE "PD_PRODUCT::PACK"
		TEXTVALUE$ = PD_PRODUCT::PACK

	CASE "PD_PRODUCT::LABEL"
		TEXTVALUE$ = PD_PRODUCT::LABEL

	CASE "PD_PRODUCT::METHOD"
		TEXTVALUE$ = PD_PRODUCT::METHOD

	CASE "PD_PRODUCT::BDATE"
		TEXTVALUE$ = PD_PRODUCT::BDATE

	CASE "PD_PRODUCT::SSTATUS"
		TEXTVALUE$ = PD_PRODUCT::SSTATUS

	CASE "PD_PRODUCT::EDATE"
		TEXTVALUE$ = PD_PRODUCT::EDATE

	CASE "PD_PRODUCT::SECONDARY_CODE"
		TEXTVALUE$ = PD_PRODUCT::SECONDARY_CODE

	CASE "PD_PRODUCT::BOMUOM"
		TEXTVALUE$ = PD_PRODUCT::BOMUOM

	CASE "PD_PRODUCT::WEIGHT"
		REALVALUE = PD_PRODUCT::WEIGHT
		TEXTVALUE$ = NUM1$(PD_PRODUCT::WEIGHT)

	CASE "PD_PRODUCT::PRODUCT_FACTOR"
		REALVALUE = PD_PRODUCT::PRODUCT_FACTOR
		TEXTVALUE$ = NUM1$(PD_PRODUCT::PRODUCT_FACTOR)


	!*******************************************************************
	! Other IC things
	!*******************************************************************

	CASE "BIN"
		TEXTVALUE$ = BIN

	CASE "CONTROL"
		TEXTVALUE$ = CONTROL

	CASE "LOCATION"
		TEXTVALUE$ = LOCATION
	!*******************************************************************
	! PROFILE
	!*******************************************************************

	CASE "UTL_PROFILE::MENU_NAME"
		TEXTVALUE$ = UTL_PROFILE::MENU_NAME

	CASE "UTL_PROFILE::REP_NAME"
		TEXTVALUE$ = UTL_PROFILE::REP_NAME

	CASE "UTL_PROFILE::MAINLOCATION"
		TEXTVALUE$ = UTL_PROFILE::MAINLOCATION

	CASE "UTL_PROFILE::DEFLOCATION"
		TEXTVALUE$ = UTL_PROFILE::DEFLOCATION

	!*******************************************************************
	! Location info
	!*******************************************************************

	CASE "UTL_LOCATION::LOCATION"
		TEXTVALUE$ = UTL_LOCATION::LOCATION

	CASE "UTL_LOCATION::LOCNAME"
		TEXTVALUE$ = UTL_LOCATION::LOCNAME

	CASE "UTL_LOCATION::REGION"
		TEXTVALUE$ = UTL_LOCATION::REGION

	CASE "UTL_LOCATION::LOCGROUP"
		TEXTVALUE$ = UTL_LOCATION::LOCGROUP

	CASE "UTL_LOCATION::ADDRESS1"
		TEXTVALUE$ = UTL_LOCATION::ADDRESS1

	CASE "UTL_LOCATION::ADDRESS2"
		TEXTVALUE$ = UTL_LOCATION::ADDRESS2

	CASE "UTL_LOCATION::CITY"
		TEXTVALUE$ = UTL_LOCATION::CITY

	CASE "UTL_LOCATION::STATE"
		TEXTVALUE$ = UTL_LOCATION::STATE

	CASE "UTL_LOCATION::ZIP"
		TEXTVALUE$ = UTL_LOCATION::ZIP

	CASE "UTL_LOCATION::COUNTY"
		TEXTVALUE$ = UTL_LOCATION::COUNTY

	CASE "UTL_LOCATION::COUNTRY"
		TEXTVALUE$ = UTL_LOCATION::COUNTRY

	CASE "UTL_LOCATION::PHONE"
		TEXTVALUE$ = UTL_LOCATION::PHONE

	CASE "UTL_LOCATION::SHPADDRESS1"
		TEXTVALUE$ = UTL_LOCATION::SHPADDRESS1

	CASE "UTL_LOCATION::SHPADDRESS2"
		TEXTVALUE$ = UTL_LOCATION::SHPADDRESS2

	CASE "UTL_LOCATION::SHPCITY"
		TEXTVALUE$ = UTL_LOCATION::SHPCITY

	CASE "UTL_LOCATION::SHPSTATE"
		TEXTVALUE$ = UTL_LOCATION::SHPSTATE

	CASE "UTL_LOCATION::SHPZIP"
		TEXTVALUE$ = UTL_LOCATION::SHPZIP

	CASE "UTL_LOCATION::SHPCOUNTY"
		TEXTVALUE$ = UTL_LOCATION::SHPCOUNTY

	CASE "UTL_LOCATION::SHPCOUNTRY"
		TEXTVALUE$ = UTL_LOCATION::SHPCOUNTRY

	CASE "UTL_LOCATION::SHPPHONE"
		TEXTVALUE$ = UTL_LOCATION::SHPPHONE

	END SELECT

	END SUB
