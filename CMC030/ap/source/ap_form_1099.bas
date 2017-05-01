1	%TITLE "Print 1099 Using a Form"
	%SBTTL "AP_FORM_1099"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1987, 1988 BY
	!	Computer Management Center, Inc.
	!	Idaho Falls, Idaho.
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
	! ID:AP1FRM
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print\* program prints 1099 forms.  Each different type of 1099
	!	must be printed separately.  The 1099 year is selected by the user.
	!	.lm -5
	!
	! Index:
	!	.x Print 1099 Using a Form
	!	.x Form>Print 1099 Using a
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_FORM_1099/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_FORM_1099, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_FORM_1099.OBJ;*
	!
	! Author:
	!
	!	10/22/87 - Robert Peterson
	!
	! Modification history:
	!
	!	05/17/88 - Lance Williams
	!		Modified the header.
	!
	!	01/25/89 - Kevin Handy
	!		Check to determine why not all 1099's were
	!		being printed.  Turned out they did not reach
	!		the limit set in the 1099 table.
	!
	!	02/09/89 - Kevin Handy
	!		Modified for changes in ENTR_ENTER.
	!
	!	01/10/91 - Craig Tanner
	!		Changed where FILENAME$ = "AP_1099_YYYY" to =
	!		"AP_1099_" + YEAR_1099$ in error handler.
	!
	!	06/04/91 - J. Shad Rydalch
	!		Added form fields for address that will keep
	!		post office happy.
	!
	!	01/30/92 - Kevin Handy
	!		Attempt to fix screwed up mess that Frank generated
	!		by deleteing olf location name and replacing with
	!		non-working addresses.
	!
	!	01/31/92 - Dan Perkins
	!		Changed MAP statement in function to match MAP
	!		statement in main portion of program.
	!
	!	06/09/92 - Kevin Handy
	!		Modified to use OUTP_INITFORM
	!
	!	06/16/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/23/93 - Kevin Handy
	!		Removed JJ$ definition.
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 source format
	!		Change last parameter on entr_3choice from "" to 0%
	!
	!	10/09/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	01/24/96 - Kevin Handy
	!		Add location phone number.
	!
	!	05/09/97 - Kevin Handy
	!		Lose variable PRNT.CH%
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code
	!		Use integer for #key
	!
	!	08/22/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/29/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	01/27/98 - Kevin Handy
	!		Run phone numbers through prnt_phone before
	!		passing them on to print routines. (KBJ)
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Do an EDIT on YEAR_1099$ before checking its length
	!		(MAP variable is always the same length)
	!
	!	09/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	01/30/2001 - Kevin Handy
	!		Increased number of available boxes to 20
	!		Implement method to determine number of forms on
	!		a page.
	!
	!	01/31/2008 - Kevin Handy
	!		Use formff to handle laser forms better.
	!		I'm not worrying about stupid printers that don't
	!		know how to form feed.
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
	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[AP.OPEN]AP_1099_YYYY.HB"
	MAP (AP_1099_YYYY)	AP_1099_YYYY_CDD	AP_1099_YYYY

	%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.HB"
	MAP (AP_1099_TABLE)	AP_1099_TABLE_CDD	AP_1099_TABLE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP (UTL_PROFILE)	UTL_PROFILE_CDD	UTL_PROFILE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT) UTL_REPORT_CDD UTL_REPORT
	DECLARE UTL_REPORT_CDD UTL_REPORT_SYS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM FORM_GROUP_CDD FORM_GROUP(10%)	! Max of 10 groups

	!
	! ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE **
	!
	! Don't forget to change the MAX_TOTAL_LOC constant in the
	! FORM function too, and extend the select fields too.
	!
	! ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE ** NOTE **
	!
	DECLARE INTEGER CONSTANT MAX_TOTAL_LOC = 20%
	DECLARE INTEGER CONSTANT FORM_LENGTH = 33%	! Individual form length

	MAP (FORM_1099) &
		TOTAL_LOC(MAX_TOTAL_LOC), &
		FED_ID_NO$, &
		YEAR_1099$ = 4%, &
		AP_VENDOR.POADDLINE$(3%) = 50%, &
		AP_VENDOR.ADDLINE$(3%) = 50%, &
		UTL_LOCATION.ADDLINE$(3%) = 50%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OUTP_FORMINIT
	EXTERNAL LONG	FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG   FUNCTION OUTP_INITFORM

	!
	! Declare variables
	!
	DIM BASE_AMT(MAX_TOTAL_LOC)

	%PAGE

	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	FORMS_PER_PAGE% = 3%

	CALL READ_INITIALIZE

	REPORT$ = "AP1FRM"

	!
	! Look up device
	!
	CALL READ_DEVICE("AP_FORM", AP_FORM.DEV$, STAT%)

100	!
	! Query user for year of file
	!
	CALL FIND_FILE(AP_1099_YYYY.DEV$ + "AP_1099_*.HIS", &
		AP_1099_YYYY_FILE$(), 16%, "", "")

	AP_1099_YYYY_FILE% = VAL%(AP_1099_YYYY_FILE$(0%))

	IF AP_1099_YYYY_FILE%
	THEN
		AP_1099_YYYY_FILE$(LOOP%) = &
			MID(AP_1099_YYYY_FILE$(LOOP%), 9%, 4%) &
				FOR LOOP% = 1% TO AP_1099_YYYY_FILE%

		TEMP$ = "AP 1099 Register"

		X% = ENTR_3CHOICE(SCOPE, "", "", AP_1099_YYYY_FILE$(), "", &
			0%, TEMP$, "", 0%)

		IF X% > 0%
		THEN
			YEAR_1099$ = EDIT$(AP_1099_YYYY_FILE$(X%), -1%)
			GOTO 200
		END IF
	END IF

	!
	! Case to exit program
	!
	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	!
	! Ask for year
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		20%, &
		80%, &
		SMG_SCREEN_DATA% &
	)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "1099 Year:", 11%, 30%)

120	INP$ = LEFT(DATE_TODAY, 4%)

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 11%, 43%, INP$, -1%, 0%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	YEAR_1099$ = EDIT$(INP$, -1%)

	YEAR_1099$ = LEFT(DATE_TODAY, 2%) + INP$ &
		IF LEN(INP$) = 2%

	IF LEN(EDIT$(YEAR_1099$, -1%)) <> 4%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the 1099 register year in YYYY format", &
			0%)
		GOTO 120
	END IF

200	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

 SelectForm:
	!
	! Table for 1099 forms
	!
	EC$(1%) = "1  1099-A"
	EC$(2%) = "2  1099-B"
	EC$(3%) = "3  1099-DIV"
	EC$(4%) = "4  1099-G"
	EC$(5%) = "5  1099-INT"
	EC$(6%) = "6  1099-MISC"
	EC$(7%) = "7  1099-OID"
	EC$(8%) = "8  1099-PATR"
	EC$(9%) = "9  1099-R"

	TEMP$ = "1099 Forms"

	X% = ENTR_3CHOICE(SCOPE, "", "", EC$(), "", 0%, TEMP$, "", 0%)

	IF X% > 0%
	THEN
		FORM_NUM$ = NUM1$(X%)
		GOTO InitForm
	END IF

	SELECT SCOPE::SCOPE_EXIT
	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?
		GOTO ExitProgram

	END SELECT

	GOTO SelectForm

 InitForm:
	!*******************************************************************
	! Initilize Attachment form
	!*******************************************************************

	!
	! Get form from the AP form library
	!
	SMG_STATUS% = OUTP_FORMINIT( &
		AP_FORM.DEV$ + "AP_FORM", "1099_" + &
		FORM_NUM$, FORM_TEXT$, FORM_GROUP%, FORM_GROUP())

	!
	! Was there an error?
	!
	IF SMG_STATUS% <> 0%
	THEN
		CALL HELP_34MESSAGE(SCOPE, "1099 form is missing", "E", &
			SCOPE::PRG_PROGRAM, "1099_" + FORM_NUM$, &
			NUM1$(SMG_STATUS%))
		GOTO ExitProgram
	END IF

	!
	! Search for the desired parts of the form
	!
	FRM_1099% = 0%

	FOR I% = 1% TO FORM_GROUP%

		SELECT FORM_GROUP(I%)::FGROUP

		CASE "FRM-PER-PAGE"
			FORMS_PER_PAGE% = &
				FORM_GROUP(I%)::NUMBER

		CASE "FRM-1099"
			FRM_1099% = I%

		END SELECT

	NEXT I%

	!***************************************************************
	! Open all of the files
	!***************************************************************

300	!
	! Open vendor master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

310	!
	! Open ap 1099 Table file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.OPN"
	USE
		FILENAME$ = "AP_1099_TABLE"
		CONTINUE HelpError
	END WHEN

320	!
	! Open ap 1099 Year file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_1099_YYYY.OPN"
	USE
		FILENAME$ = "AP_1099_" + YEAR_1099$
		CONTINUE HelpError
	END WHEN

325	!
	! Open UTL_PROFILE file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.OPN"
		GET #UTL_PROFILE.CH%, RECORD 1%, REGARDLESS
		CLOSE UTL_PROFILE.CH%
	USE
		FILENAME$ = "UTL_PROFILE"
		CONTINUE HelpError
	END WHEN

327	!
	! Open LOCATION file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
		GET #UTL_LOCATION.CH%, &
			KEY #0% EQ UTL_PROFILE::MAINLOCATION, &
			REGARDLESS
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	!
	! Create an address line format that reduces white space
	!
	I% = 0%

	IF EDIT$(UTL_LOCATION::ADDRESS1, -1%) <> ""
	THEN
		I% = I% + 1%
		UTL_LOCATION.ADDLINE$(I%) = EDIT$(UTL_LOCATION::ADDRESS1, &
			8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(UTL_LOCATION::ADDRESS2, -1%) <> ""
	THEN
		I% = I% + 1%
		UTL_LOCATION.ADDLINE$(I%) = EDIT$(UTL_LOCATION::ADDRESS2, &
			8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%
	UTL_LOCATION.ADDLINE$(I%) = &
		EDIT$(EDIT$(UTL_LOCATION::CITY, 128%) + ", " + &
		UTL_LOCATION::STATE + " " + UTL_LOCATION::ZIP + " " + &
		UTL_LOCATION::COUNTRY, 8% + 16% + 32% + 128%)

	! Blank rest of 'em out
	UTL_LOCATION.ADDLINE$(LOOP%) = "" FOR LOOP% = I% + 1% TO 3%

330	!
	! Get Report
	!
	GOTO ExitProgram &
		IF OUTP_INITFORM(UTL_REPORTX, REPORT$, "") <> CMC$_NORMAL


	FED_ID_NO$ = UTL_REPORTX::OPTDEF(0%)
	!++
	! Abstract:FLD01
	!	^*(01) Federal Identification\*
	!	.b
	!	.lm +5
	!	The ^*Federal Identification\* field enters a social
	!	security number or other federal identification number of a vendor for whom a
	!	Form 1099 will be printed at year end.
	!	.B
	!	The field will accept up to 13 characters.
	!	.lm -5
	!
	! Index:
	!	.x Federal Identification>Print 1099 Form
	!
	!--

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Restore original values for the help message
	!
	SCOPE::PRG_IDENT = TEMP_IDENT$
	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$

	FORM_COUNT% = 0%

	!
	! GOTO aligment routine
	!
	GOSUB Alignment

	%PAGE

2000	!*******************************************************************
	! Read through CDJ file
	!*******************************************************************

	VENNUM$ = ""

	WHEN ERROR IN
		RESET #AP_1099_YYYY.CH%
	USE
		FILENAME$ = "AP_1099_" + YEAR_1099$
		CONTINUE HelpError
	END WHEN

2010	WHEN ERROR IN
		GET #AP_1099_YYYY.CH%, REGARDLESS
	USE
		CONTINUE 3000
	END WHEN

	IF AP_1099_YYYY::CODE = ""
	THEN
		GOTO 2010
	END IF

	IF VENNUM$ <> AP_1099_YYYY::VENNUM
	THEN
		IF VENNUM$ <> ""
		THEN
			GOSUB Print1099
		END IF

		TOTAL_LOC(I%) = 0.0 FOR I% = 1% TO MAX_TOTAL_LOC
	END IF

	VENNUM$ = AP_1099_YYYY::VENNUM

2020	!
	! Look up in table to find location and form #
	!
	WHEN ERROR IN
		FIND #AP_1099_TABLE.CH%, &
			KEY #0% EQ AP_1099_YYYY::CODE, REGARDLESS
		GET #AP_1099_TABLE.CH%, REGARDLESS
	USE
		CONTINUE 2030
	END WHEN

	IF AP_1099_TABLE::FRMNUM = FORM_NUM$
	THEN
		LOOP% = VAL%(AP_1099_TABLE::FRMLOC)
		TOTAL_LOC(LOOP%) = TOTAL_LOC(LOOP%) + AP_1099_YYYY::AMT1099
		BASE_AMT(LOOP%) = AP_1099_TABLE::BASEAMT
	END IF

2030	GOTO 2010

3000	!*******************************************************************
	! Found the end of the 1099 file
	!*******************************************************************

	IF VENNUM$ <> ""
	THEN
		GOSUB Print1099
	END IF

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

	!
	! Remove temp file
	!
	CLOSE AP_1099_YYYY.CH%

	!
	! Erase Display
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_SCREEN_DATA%)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	!
	! Change the width
	!
	SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, 80%)

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 Print1099:
18000	!***************************************************************
	! Print 1099 form
	!***************************************************************

	TEST_TOTAL% = 0%

	FOR LOOP% = 1% TO MAX_TOTAL_LOC
		IF BASE_AMT(LOOP%) > TOTAL_LOC(LOOP%)
		THEN
			TOTAL_LOC(LOOP%) = 0.0
		END IF

		TEST_TOTAL% = -1% IF TOTAL_LOC(LOOP%) <> 0.0
	NEXT LOOP%

	GOTO 18100 IF TEST_TOTAL% = 0%

18010	!
	! Find vendor for this CDJ
	!
	WHEN ERROR IN
		GET #AP_VENDOR.CH%, KEY #0% EQ VENNUM$, REGARDLESS
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! Create an address line format for vendor that reduces white space
	!
	I% = 0%

	IF EDIT$(AP_VENDOR::ADD1, -1%) <> ""
	THEN
		I% = I% + 1%
		AP_VENDOR.ADDLINE$(I%) = &
			EDIT$(AP_VENDOR::ADD1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(AP_VENDOR::ADD2, -1%) <> ""
	THEN
		I% = I% + 1%
		AP_VENDOR.ADDLINE$(I%) = &
			EDIT$(AP_VENDOR::ADD2, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%
	AP_VENDOR.ADDLINE$(I%) = EDIT$(EDIT$(AP_VENDOR::CITY, 128%) + ", " + &
		AP_VENDOR::STATE + " " + AP_VENDOR::ZIP + " " + &
		AP_VENDOR::COUNTRY, 8% + 16% + 32% + 128%)

	! Blank rest of 'em out
	AP_VENDOR.ADDLINE$(LOOP%) = "" FOR LOOP% = I% + 1% TO 3%

	!
	! Create an address line format for purchase order that
	! reduces white space
	!
	I% = 0%

	IF EDIT$(AP_VENDOR::POADD1, -1%) <> ""
	THEN
		I% = I% + 1%
		AP_VENDOR.POADDLINE$(I%) = &
			EDIT$(AP_VENDOR::POADD1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(AP_VENDOR::ADD2, -1%) <> ""
	THEN
		I% = I% + 1%
		AP_VENDOR.POADDLINE$(I%) = &
			EDIT$(AP_VENDOR::POADD2, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%
	AP_VENDOR.POADDLINE$(I%) = &
		EDIT$(EDIT$(AP_VENDOR::POCITY, 128%) + ", " + &
		AP_VENDOR::POSTATE + " " + AP_VENDOR::POZIP + " " + &
		AP_VENDOR::POCOUNTRY, 8% + 16% + 32% + 128%)

	! Blank rest of 'em out
	AP_VENDOR.POADDLINE$(LOOP%) = "" FOR LOOP% = I% + 1% TO 3%

	TEST_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_1099%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	FORM_COUNT% = FORM_COUNT% + 1%

	IF (FORM_COUNT% AND 1%) = 0%
	THEN
		CALL OUTP_NEWPAGE(UTL_REPORTX)
	ELSE
		CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
			FOR LOOP% = TEST_COUNT% + 1% TO FORM_LENGTH
	END IF


18100	!
	! End out of loop
	!
	RETURN

	%Page

 Alignment:
	!*******************************************************************
	! Print alignment form, if desireable
	!*******************************************************************

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	SCOPE::PRG_ITEM = "ALIGNMENT"
	JUNK$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Do you want an alignment form?  " + &
		"Confirm then press <Do> ", &
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

	FOR I% = 1% TO FORMS_PER_PAGE%
		TEST_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_1099%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			1%)

		FORM_COUNT% = FORM_COUNT% + 1%

		IF (FORM_COUNT% AND 1%) = 0%
		THEN
			CALL OUTP_NEWPAGE(UTL_REPORTX)
		ELSE
			CALL OUTP_LINENOTITLE(LYT_LINE$, UTL_REPORTX, "", 0%) &
				FOR LOOP% = TEST_COUNT% + 1% TO FORM_LENGTH
		END IF
	NEXT I%

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
	FILENAME$ = ""
	RESUME HelpError

 HelpError:
19990	!
	! This moved from inside error to outside so that errors occuring
	! at lower levels could be trapped.  Basic will not allow any
	! error to occur inside of an error no matter if it is in a
	! different module.
	!
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
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
	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[AP.OPEN]AP_1099_YYYY.HB"
	MAP (AP_1099_YYYY)	AP_1099_YYYY_CDD	AP_1099_YYYY

	%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.HB"
	MAP (AP_1099_TABLE)	AP_1099_TABLE_CDD	AP_1099_TABLE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	DECLARE INTEGER CONSTANT MAX_TOTAL_LOC = 20%

	MAP (FORM_1099) &
		TOTAL_LOC(MAX_TOTAL_LOC), &
		FED_ID_NO$, &
		YEAR_1099$ = 4%, &
		AP_VENDOR.POADDLINE$(3%) = 50%, &
		AP_VENDOR.ADDLINE$(3%) = 50%, &
		UTL_LOCATION.ADDLINE$(3%) = 50%

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
		TEXTVALUE$ = AP_VENDOR::VENNUM

	CASE "AP_VENDOR::VENNAM"
		TEXTVALUE$ = AP_VENDOR::VENNAM

	CASE "AP_VENDOR::ADD1"
		TEXTVALUE$ = AP_VENDOR::ADD1

	CASE "AP_VENDOR::ADD2"
		TEXTVALUE$ = AP_VENDOR::ADD2

	CASE "AP_VENDOR::CITY"
		TEXTVALUE$ = AP_VENDOR::CITY

	CASE "AP_VENDOR::STATE"
		TEXTVALUE$ = AP_VENDOR::STATE

	CASE "AP_VENDOR::ZIP"
		TEXTVALUE$ = AP_VENDOR::ZIP

	CASE "AP_VENDOR::COUNTRY"
		TEXTVALUE$ = AP_VENDOR::COUNTRY

	CASE "AP_VENDOR.ADDLINE1"	! Substitute Vendor Address
		TEXTVALUE$ = AP_VENDOR.ADDLINE$(1%)

	CASE "AP_VENDOR.ADDLINE2"	! Substitute Vendor Address
		TEXTVALUE$ = AP_VENDOR.ADDLINE$(2%)

	CASE "AP_VENDOR.ADDLINE3"	! Substitute Vendor Address
		TEXTVALUE$ = AP_VENDOR.ADDLINE$(3%)

	CASE "AP_VENDOR::PHONE"
		TEXTVALUE$ = PRNT_PHONE(AP_VENDOR::PHONE, 0%)

	CASE "AP_VENDOR::POADD1"
		TEXTVALUE$ = AP_VENDOR::POADD1

	CASE "AP_VENDOR::POADD2"
		TEXTVALUE$ = AP_VENDOR::POADD2

	CASE "AP_VENDOR::POCITY"
		TEXTVALUE$ = AP_VENDOR::POCITY

	CASE "AP_VENDOR::POSTATE"
		TEXTVALUE$ = AP_VENDOR::POSTATE

	CASE "AP_VENDOR::POZIP"
		TEXTVALUE$ = AP_VENDOR::POZIP

	CASE "AP_VENDOR::POCOUNTRY"
		TEXTVALUE$ = AP_VENDOR::POCOUNTRY

	CASE "AP_VENDOR::POPHONE"
		TEXTVALUE$ = AP_VENDOR::POPHONE

	CASE "AP_VENDOR.POADDLINE1"	! Substitute Purchase Order Address
		TEXTVALUE$ = ADDLINE$(1%)

	CASE "AP_VENDOR.POADDLINE2"	! Substitute Purchase Order Address
		TEXTVALUE$ = ADDLINE$(2%)

	CASE "AP_VENDOR.POADDLINE3"	! Substitute Purchase Order Address
		TEXTVALUE$ = ADDLINE$(3%)

	CASE "AP_VENDOR::PURGE"
		TEXTVALUE$ = AP_VENDOR::PURGE

	CASE "AP_VENDOR::FEDID"
		TEXTVALUE$ = AP_VENDOR::FEDID

	CASE "AP_VENDOR::FLG1099"
		TEXTVALUE$ = AP_VENDOR::FLG1099

	CASE "AP_VENDOR::DUEDAYS"
		REALVALUE = AP_VENDOR::DUEDAYS
		TEXTVALUE$ = NUM1$(AP_VENDOR::DUEDAYS)

	CASE "AP_VENDOR::DUEDAT"
		TEXTVALUE$ = AP_VENDOR::DUEDATE

	CASE "AP_VENDOR::DISDAYS"
		REALVALUE = AP_VENDOR::DISDAYS
		TEXTVALUE$ = NUM1$(AP_VENDOR::DISDAYS)

	CASE "AP_VENDOR::DISDATE"
		TEXTVALUE$ = AP_VENDOR::DISDATE

	CASE "AP_VENDOR::DISCPER"
		REALVALUE = AP_VENDOR::DISCPER / 100.0

	CASE "AP_VENDOR::ALPSRT"
		TEXTVALUE$ = AP_VENDOR::ALPSRT

	!************************************************************
	! Fields for the AP_1099_TABLE file
	!************************************************************

	CASE "AP_1099_TABLE::CODE"
		TEXTVALUE$ = AP_1099_TABLE::CODE

	CASE "AP_1099_TABLE::DESCR"
		TEXTVALUE$ = AP_1099_TABLE::DESCR

	CASE "AP_1099_TABLE::BASEAMT"
		REALVALUE = AP_1099_TABLE::BASEAMT

	CASE "AP_1099_TABLE::FRMNUM"
		TEXTVALUE$ = AP_1099_TABLE::FRMNUM

	CASE "AP_1099_TABLE::FRMLOC"
		TEXTVALUE$ = AP_1099_TABLE::FRMLOC

	!************************************************************
	! Fields for the AP_1099_YYYY file
	!************************************************************

	CASE "AP_1099_YYYY::VENNUM"
		TEXTVALUE$ = AP_1099_YYYY::VENNUM

	CASE "AP_1099_YYYY::CODE"
		TEXTVALUE$ = AP_1099_YYYY::CODE

	CASE "AP_1099_YYYY::TRANKEY"
		TEXTVALUE$ = AP_1099_YYYY::TRANKEY

	CASE "AP_1099_YYYY::INVNUM"
		TEXTVALUE$ = AP_1099_YYYY::INVNUM

	CASE "AP_1099_YYYY::INVDAT"
		TEXTVALUE$ = PRNT_DATE(AP_1099_YYYY::INVDAT, 8%)

	CASE "AP_1099_YYYY::CKNUM"
		TEXTVALUE$ = AP_1099_YYYY::CKNUM

	CASE "AP_1099_YYYY::CKDAT"
		TEXTVALUE$ = PRNT_DATE(AP_1099_YYYY::CKDAT, 8%)

	CASE "AP_1099_YYYY::AMT1099"
		REALVALUE = AP_1099_YYYY::AMT1099

	CASE "AP_1099_YYYY::CKAMT"
		REALVALUE = AP_1099_YYYY::CKAMT

	!************************************************************
	! Fields for the UTL_LOCATION file
	!************************************************************

	CASE "UTL_LOCATION::LOCNAME"
		TEXTVALUE$ = UTL_LOCATION::LOCNAME

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

	CASE "UTL_LOCATION::PHONE"
		TEXTVALUE$ = PRNT_PHONE(UTL_LOCATION::PHONE, 0%)

	CASE "UTL_LOCATION::COUNTRY"
		TEXTVALUE$ = UTL_LOCATION::COUNTRY

	CASE "UTL_LOCATION.ADDLINE1"	! Substitute Location Address
		TEXTVALUE$ = UTL_LOCATION.ADDLINE$(1%)

	CASE "UTL_LOCATION.ADDLINE2"	! Substitute Location Address
		TEXTVALUE$ = UTL_LOCATION.ADDLINE$(2%)

	CASE "UTL_LOCATION.ADDLINE3"	! Substitute Loaction Address
		TEXTVALUE$ = UTL_LOCATION.ADDLINE$(3%)

	!************************************************************
	! Non fielded values
	!************************************************************

	CASE "TOTAL_01"
		REALVALUE = TOTAL_LOC(1%)

	CASE "TOTAL_02"
		REALVALUE = TOTAL_LOC(2%)

	CASE "TOTAL_03"
		REALVALUE = TOTAL_LOC(3%)

	CASE "TOTAL_04"
		REALVALUE = TOTAL_LOC(4%)

	CASE "TOTAL_05"
		REALVALUE = TOTAL_LOC(5%)

	CASE "TOTAL_06"
		REALVALUE = TOTAL_LOC(6%)

	CASE "TOTAL_07"
		REALVALUE = TOTAL_LOC(7%)

	CASE "TOTAL_08"
		REALVALUE = TOTAL_LOC(8%)

	CASE "TOTAL_09"
		REALVALUE = TOTAL_LOC(9%)

	CASE "TOTAL_10"
		REALVALUE = TOTAL_LOC(10%)

	CASE "TOTAL_11"
		REALVALUE = TOTAL_LOC(11%)

	CASE "TOTAL_12"
		REALVALUE = TOTAL_LOC(12%)

	CASE "TOTAL_13"
		REALVALUE = TOTAL_LOC(13%)

	CASE "TOTAL_14"
		REALVALUE = TOTAL_LOC(14%)

	CASE "TOTAL_15"
		REALVALUE = TOTAL_LOC(15%)

	CASE "TOTAL_16"
		REALVALUE = TOTAL_LOC(16%)

	CASE "TOTAL_17"
		REALVALUE = TOTAL_LOC(17%)

	CASE "TOTAL_18"
		REALVALUE = TOTAL_LOC(18%)

	CASE "TOTAL_19"
		REALVALUE = TOTAL_LOC(19%)

	CASE "TOTAL_20"
		REALVALUE = TOTAL_LOC(20%)

	CASE "FED_ID_NO"
		TEXTVALUE$ = FED_ID_NO$

	CASE "YEAR"
		TEXTVALUE$ = YEAR$

	END SELECT

	END SUB
