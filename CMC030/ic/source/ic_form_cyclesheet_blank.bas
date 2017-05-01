1	%TITLE "Cycle Count Worksheet"
	%SBTTL "IC_FORM_CYCLESHEET_BLANK"
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
	!	The ^*Blank Cycle Count Form\* prints out blank
	!	cycle count forms.
	!	.lm -5
	!
	! Index:
	!	.x Cycle Count Form
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_FORM_CYCLESHEET_BLANK/LINE
	!	$ LINK/EXE=IC_EXE: IC_FORM_CYCLESHEET_BLANK, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_FORM_CYCLESHEET_BLANK.OBJ;*
	!
	!
	! Author:
	!
	!	10/26/98 - Kevin Handy
	!		Based on IC_RPRT_CYCLESHEET
	!
	! Modification History:
	!
	!	10/30/2000 - Kevin Handy
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
	EXTERNAL LONG	FUNCTION OUTP_FORMINIT
	EXTERNAL LONG	FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG	FUNCTION OUTP_INITFORM

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

	REPORT$ = "ICYCLB"
	FORM$ = "ICYCLE"

	!
	! Look up device
	!
	CALL READ_DEVICE("IC_FORM", IC_FORM.DEV$, STAT%)

	!
	! Get Report
	!
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL

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

	COPIES% = VAL%(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) Copies to print\*
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!	.x Copies>Cycle Count Form
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

	FORM_NAME$ = FORM$ + "$" + TRM$(UTL_REPORTX::OPTDEF(9%))

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

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
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
			OPEN CONTROL_FILE$ AS FILE CONTROL_FILE.CH%, &
				RECORDSIZE 132%, &
				ACCESS APPEND
		USE
			FILENAME$ = "CONTROL.FILE"
			CONTINUE HelpError
		END WHEN
	END IF

340	GET #UTL_LOCATION.CH%, &
		KEY #0% EQ LOCATION, &
		REGARDLESS

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************


17200	!
	! Read Product file
	!
	UNDERLINE% = A"_"B
	PD_PRODUCT::PRODUCT_NUM = &
		STRING$(LEN(PD_PRODUCT::PRODUCT_NUM), UNDERLINE%)
	PD_PRODUCT::DESCRIPTION = &
		STRING$(LEN(PD_PRODUCT::DESCRIPTION), UNDERLINE%)
	PD_PRODUCT::CATEGORY = &
		STRING$(LEN(PD_PRODUCT::CATEGORY), UNDERLINE%)
	PD_PRODUCT::PROD_TYPE = &
		STRING$(LEN(PD_PRODUCT::PROD_TYPE), UNDERLINE%)
	PD_PRODUCT::UOM = &
		STRING$(LEN(PD_PRODUCT::UOM), UNDERLINE%)
	PD_PRODUCT::SSTATUS = &
		STRING$(LEN(PD_PRODUCT::SSTATUS), UNDERLINE%)

	BIN = "______"

17300	!
	! Print out one line
	!
	FOR LOOP% = 1% TO COPIES%

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

	NEXT LOOP%

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
	SMG_STATUS% = OUTP_FORMINIT(IC_FORM.DEV$ + "IC_FORM", FORM_NAME$, &
		FORM_TEXT$, FORM_GROUP%, FORM_GROUP())

	!
	! Was there an error?
	!
	IF SMG_STATUS% <> 0%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "Statement form is missing", &
			"E", SCOPE::PRG_PROGRAM, FORM_NAME$, NUM1$(SMG_STATUS%))

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
