1	%TITLE "Update Standard Cost"
	%SBTTL "BM_SPEC_COSTUPDATE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Update Standard Cost\* program adds new standard
	!	cost into the product cost file from the Bill of Materials System.
	!	.lm -5
	!
	! Index:
	!	.x Update Standard Cost
	!	.x Standard Cost>Update
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_SPEC_COSTUPDATE/LINE
	!	$ LINK/EXE=BM_EXE: BM_SPEC_COSTUPDATE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE BM_SPEC_COSTUPDATE.OBJ;*
	!
	! Author:
	!
	!	06/25/92 - Frank F. Starman
	!
	! Modification History:
	!
	!	07/31/92 - Frank F. Starman
	!		Allow to force labor dollar amount and recalculate
	!		total cost.
	!
	!	08/14/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/27/92 - Frank F. Starman
	!		Calculate burden as a percentage of the labor if
	!		percentage in not zero.
	!
	!	01/09/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/06/93 - Frank F. Starman
	!		Use PR_READ_PROD function to read operation rate.
	!
	!	12/27/93 - Frank F. Starman
	!		Fixed bug if doing auto cost update.
	!
	!	01/10/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/13/95 - Kevin Handy
	!		V3.6
	!		Update to V3.6 Calico standards.
	!		Include UTL_WINDOW.INC.
	!
	!	01/29/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...) in
	!		several places.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code.
	!		Use integer for #key
	!
	!	08/25/97 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	02/10/99 - Kevin Handy
	!		Bring over various changes from BM_MAST_PRODUCT
	!		routine to fix the bugs here.
	!		Finally made it ignore the effective date, and
	!		blap, everything finally works.
	!
	!	04/05/99 - Kevin Handy
	!		Use WHEN ERROR IN to try to figure out why some
	!		products didn't update correctly at LL. Could
	!		be caused by record locking problems.
	!
	!	04/05/99 - Kevin Handy
	!		Several minor changes and cleanup details to try
	!		and figure out why this bloody program will not
	!		calculate material costs correctly, unless I put
	!		a breakpoint in the program.
	!
	!	04/05/99 - Kevin Handy
	!		FOUND THE BUG. Used 'PRODUCT' instead of
	!		'PD_PRODUCT::PRODUCT_NUM' in PC_FIND_COST function,
	!		which is an undefined float variable.
	!
	!	04/09/99 - Kevin Handy
	!		Changed SMG$_BOLD to SMG$M_BOLD
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	!
	! Map file
	!
	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP	(PC_COST)	PC_COST_CDD	PC_COST

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP	(BM_RELATION)	BM_RELATION_CDD	BM_RELATION

	%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.HB"
	MAP	(BM_CONTROL)	BM_CONTROL_CDD	BM_CONTROL

	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	MAP	(BM_PRODOPER)	BM_PRODOPER_CDD	BM_PRODOPER

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP	(PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT
	DECLARE	PD_PRODUCT_CDD	PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.HB"
	MAP	(PR_OPER)	PR_OPER_CDD	PR_OPER
	DECLARE	PR_OPER_CDD	PR_OPER_READ

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP (UTL_PROFILE)	UTL_PROFILE_CDD		UTL_PROFILE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION
	DECLARE UTL_LOCATION_CDD	UTL_LOCATION_EXAM

	COM (CH_PC_COST_READ) PC_COST.CH%

	RECORD COMPONENT_RECORD
		STRING NUMBER   = 14%, &
		REAL   QUANTITY
	END RECORD

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION PR_READ_OPERATION
	EXTERNAL LONG	FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG	FUNCTION UTL_EXAM_LOCATION
	EXTERNAL REAL	FUNCTION PC_READ_COST(STRING, STRING, STRING, STRING)

	DECLARE LONG SYS_STATUS

	DIM COMPONENT_RECORD	COMPONENT(5000%)
	DIM RFA			RFA_LEVEL(500%)
	DIM REAL		QTY_LEVEL(500%)
	DIM STRING		TEST_PRODUCT(500%)


	! Declare constants
	!
	DECLARE LONG SMG_WIN, SMG_WIN2
	DECLARE RFA ADDRESS

	%PAGE

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	CALL READ_INITIALIZE

300	!
	! Open relation file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"
	USE
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

310	!
	! Open operation file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.CRE"
	USE
		FILENAME$ = "BM_PRODOPER"
		CONTINUE HelpError
	END WHEN

320	!
	! Open price file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PC.OPEN]PC_COST.CRE"
	USE
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

330	!
	! Open product file
	!
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"

350	WHEN ERROR IN
		%INCLUDE "SOURCE:[BM.OPEN]BM_CONTROL.OPN"
	USE
		FILENAME$ = "BM_CONTROL"
		CONTINUE HelpError
	END WHEN

	WHEN ERROR IN
		GET #BM_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		IF ERR=154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		FILENAME$ = "BM_CONTROL"
		CONTINUE HelpError
	END WHEN

	CLOSE #BM_CONTROL.CH%
	CALL ASSG_FREECHANNEL(BM_CONTROL.CH%)

360	!
	! Open Profile to get Default Location
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.OPN"
	USE
		FILENAME$ = "UTL_PROFILE"
		CONTINUE HelpError
	END WHEN

	WHEN ERROR IN
		GET #UTL_PROFILE.CH%, RECORD 1%, REGARDLESS
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		FILENAME$ = "UTL_PROFILE"
		CONTINUE HelpError
	END WHEN

	CLOSE #UTL_PROFILE.CH%
	CALL ASSG_FREECHANNEL(UTL_PROFILE.CH%)

500	!******************************************************************
	! Declare defaults for screen
	!******************************************************************

900	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_WIN, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_WIN, &
		"Update Standard Cost")

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_WIN, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)


	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN, "Sort By (C,D,P,T,S)", 2%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN, "From Item", 3%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN, "To Item", 4%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN, "Wildcard", 5%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN, "Location", 6%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN, "Eff Date", 7%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN, "Operation", 8%, 2%)

	SORTBY$    = "P"
	FROM_ITEM$ = SPACE$(14%)
	TO_ITEM$   = SPACE$(14%)
	WLDCRD$    = SPACE$(20%)
	OPERATION$ = SPACE$(8%)
	EFF_DATE$ = DATE_TODAY

	LOCATION$  = UTL_PROFILE::DEFLOCATION

	V% = UTL_EXAM_LOCATION(LOCATION$, UTL_LOCATION_EXAM)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN, &
		LOCATION$, 6%, 25%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN, &
		UTL_LOCATION_EXAM::LOCNAME, 6%, 35%, , SMG$M_BOLD)

	OPT$ = "C"
	GOTO SelOption

	%PAGE

1000	!******************************************************************
	! Main option menu
	!******************************************************************

	!
	! Enter options
	!
	SCOPE::PRG_ITEM = ""
	OPTLIST$ = "Change mLabor mcOst Alabor acosT Help eXit"
	OPT$ = ENTR_3OPTION(SCOPE, "COMMAND", OPTLIST$, OPT%, 0%)
	OPTION$ = SEG$(OPTLIST$, OPT%, INSTR(OPT%, OPTLIST$ + " ", " ") - 1%)

	SELECT SCOPE::SCOPE_EXIT
	!
	! Control c
	!
	CASE 3%
		GOTO 1000

	!
	! Exit key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	END SELECT

 SelOption:
	SELECT OPT$

	CASE "C"
 Sortby:
		SCOPE::PRG_ITEM   = "FLD01SORT"
	!++
	! Abstract:FLD01SORT
	!	^*Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field selects
	!	a particular sort key.
	!	.b
	!	The following settings are valid:
	!	.table 3,25
	!	.te
	!	^*C\* - Product Category
	!	.te
	!	^*D\* - Product Description
	!	.te
	!	^*P\* - Product Number
	!	.te
	!	^*S\* - Product Secondary Code
	!	.te
	!	^*T\* - Product Type
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
		SORTBY$ = ENTR_3STRING(SCOPE, SMG_WIN, "2;25", &
			"Sort By", SORTBY$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO Sortby

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

		SELECT SORTBY$

		CASE "C"
			SORT_KEY% = 2%

		CASE "D"
			SORT_KEY% = 3%

		CASE "P"
			SORT_KEY% = 0%

		CASE "S"
			SORT_KEY% = 4%

		CASE "T"
			SORT_KEY% = 1%

		END SELECT

 FromItem:
		SCOPE::PRG_ITEM   = "FLD02FROM"
	!++
	! Abstract:FLD02FROM
	!	^*From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* value causes the
	!	program to begin with a selected item number.
	!	.b
	!	A blank field will cause the program to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--
		FROM_ITEM$ = ENTR_3STRING(SCOPE, SMG_WIN, "3;25", &
			"From Item", FROM_ITEM$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO Sortby

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

 ToItem:
		SCOPE::PRG_ITEM   = "FLD03TO"
	!++
	! Abstract:FLD03TO
	!	^*To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* value causes
	!	the program to end with a selected item number.
	!	.b
	!	A blank setting will cause the program to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!
	!--
		TO_ITEM$ = ENTR_3STRING(SCOPE, SMG_WIN, "4;25", &
			"To Item", TO_ITEM$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO FromItem

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

 Wildcard:
		SCOPE::PRG_ITEM   = "FLD04WILDCARD"
	!++
	! Abstract:FLD04WILDCARD
	!	^*Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items
	!	to be printed by entering a "wildcard" value in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
		WLDCRD$ = ENTR_3STRING(SCOPE, SMG_WIN, "5;25", &
			"Wildcard", WLDCRD$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO ToItem

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT


 Location:
		SCOPE::PRG_ITEM   = "FLD05LOCATION"
	!++
	! Abstract:FLD05LOCATION
	!	^*Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field selects a designated
	!	product location for which the prices or cost will be changed.
	!	.b
	!	The ^*Location\* will default to the company profile location
	!	if no location is entered by the user.
	!	.b
	!	^*NOTE: The company profile location is the ^*default\*
	!	location, which means prices and costs will be changed for
	!	^*all locations\*, if no other locations are defined.\*
	!	.b
	!	Use caution when entering a location for the above reason.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
		LOCATION$ = ENTR_3STRING(SCOPE, SMG_WIN, "6;25", &
			"Location", LOCATION$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT
		!
		! List Choices
		!
		CASE SMG$K_TRM_F14
			SCOPE::PRG_ITEM   = "FLD05LOCATION"

			IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
				"VX") = 1%
			THEN
				LOCATION$ = UTL_LOCATION::LOCATION
			END IF

			GOTO Location

		CASE SMG$K_TRM_UP
			GOTO Wildcard

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

		V% = UTL_EXAM_LOCATION(LOCATION$, UTL_LOCATION_EXAM)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN, &
			LOCATION$, 6%, 25%, , SMG$M_BOLD)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN, &
			UTL_LOCATION_EXAM::LOCNAME, 6%, 35%, , SMG$M_BOLD)

 Effdate:

		SCOPE::PRG_ITEM   = "FLD06EFFDATE"
	!++
	! Abstract:FLD06EFFDATE
	!	^*Effective Date\*
	!	.b
	!	.lm +5
	!	The ^*Effective Date\* field allows entry of the date
	!	the price or cost changes become effective.  The field
	!	will default to today's date.  The date may be overridden by entering
	!	the correct date and pressing ^*Return\*.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
		EFF_DATE$ = ENTR_3DATE(SCOPE, SMG_WIN, "7;25", &
			"Eff Date", EFF_DATE$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO Location

		CASE SMG$K_TRM_DOWN
			GOTO 1120

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

1120		SCOPE::PRG_ITEM   = "FLD07OPER"
	!++
	! Abstract:FLD07OPER
	!	^* Operation\*
	!	.b
	!	.lm +5
	!	The ^*Operation\* code enters the location
	!	where the new cost will be added and calculated.
	!	.b
	!	Pressing ^*<List Choices>\*, while the cursor is located
	!	at this field, will cause a list of valid location codes
	!	to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Location>Update Standard Cost
	!	.x Update Standard Cost>Location
	!
	!--
		OPERATION$ = ENTR_3STRING(SCOPE, SMG_WIN, "8;25", &
			"Operation ", OPERATION$, FLAG%, "'E", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_UP
			GOTO EffDate

		CASE SMG$K_TRM_DOWN
			GOTO 1120

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO 1000

		END SELECT

		IF PR_READ_OPERATION(OPERATION$, &
			EFF_DATE$, PR_OPER_READ) = CMC$_NORMAL
		THEN
			OPERHOUR_RATE = PR_OPER_READ::HOUR_RATE
		ELSE
			OPERHOUR_RATE = 0.0
		END IF

1125		SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN, &
			FORMAT$(OPERHOUR_RATE, "Hour Rate <%>#.##"), 8%, 36%,,)


	!
	! This option calls out a help message describing the
	! program.
	!
	CASE "H"
		CALL HELP_3MESSAGE(SCOPE, "", "PROG", &
			SCOPE::PRG_PROGRAM, "HELP")

	CASE "X"
		GOTO ExitProgram

	CASE "A"
		FLG% = 1%

		SCOPE::PRG_ITEM = "CONFLAB"
		GOTO 1000 IF ENTR_3YESNO(SCOPE, SMG_WIN2, &
			"", "Confirm auto labor update - then press <Do> ", &
			"N", 0%, "", "") <> "Y"

		CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
		( &
			10%, &
			60%, &
			SMG_WIN2, &
			SMG$M_BORDER &
		)

		!
		! Label the display
		!
		SMG_STATUS% = SMG$LABEL_BORDER(SMG_WIN2, "Auto Labor Update")

		GOSUB Win2

	CASE "L"
		FLG% = 0%

		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
		( &
			10%, &
			60%, &
			SMG_WIN2, &
			SMG$M_BORDER &
		)

		!
		! Label the display
		!
		SMG_STATUS% = SMG$LABEL_BORDER(SMG_WIN2, "Manual Labor Change")

		GOSUB Win2

	CASE "O"
		FLG% = 0%

		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
		( &
			10%, &
			60%, &
			SMG_WIN2, &
			SMG$M_BORDER &
		)

		!
		! Label the display
		!
		SMG_STATUS% = SMG$LABEL_BORDER(SMG_WIN2, "Manual Cost Change")

		GOSUB Win2

	CASE "T"
		FLG% = 1%

		SCOPE::PRG_ITEM = "CONFCOST"
		GOTO 1000 IF ENTR_3YESNO(SCOPE, SMG_WIN2, &
			"", "Confirm auto cost update - then press <Do> ", &
			"N", 0%, "", "") <> "Y"

		CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
		( &
			10%, &
			60%, &
			SMG_WIN2, &
			SMG$M_BORDER &
		)

		!
		! Label the display
		!
		SMG_STATUS% = SMG$LABEL_BORDER(SMG_WIN2, "Auto Cost Update")

		GOSUB Win2

	END SELECT

	GOTO 1000

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the program
	!******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

 Win2:
2000	!*******************************************************************
	! Set Up Second Window
	!*******************************************************************

	!
	! Label the display
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Product #", 2%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Description", 3%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Material Cost", 5%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Labor Cost", 6%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Labor Hours ", 6%, 34%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Burden Cost", 7%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, &
		"------------------------------", 8%, 2%)
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, "Total Cost", 9%, 2%)


	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_WIN2, &
		SCOPE::SMG_PBID, &
		8%, &
		8% &
	)

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PD_PRODUCT.CH%, &
				KEY #SORT_KEY%
		ELSE
			FIND #PD_PRODUCT.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$ + "", &
				REGARDLESS
		END IF
	USE
		CONTINUE 2100 IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN


 GetNextRec:
2005	!
	! Main loop starts here
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		IF ERR=154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE OutAWin2 IF ERR=11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO GetNextRec IF PD_PRODUCT::SSTATUS = "O"

	CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)
	SELECT SORTBY$

	CASE "C"
		GOTO OutaWin2 IF (PD_PRODUCT::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::CATEGORY, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "D"
		GOTO OutaWin2 IF (PD_PRODUCT::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::DESCRIPTION, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "P"
		GOTO OutaWin2 IF (PD_PRODUCT::PRODUCT_NUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::PRODUCT_NUM, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"
		GOTO OutaWin2 IF (PD_PRODUCT::SECONDARY_CODE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::SECONDARY_CODE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO OutaWin2 IF (PD_PRODUCT::PROD_TYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING(EDIT$( &
			PD_PRODUCT::PROD_TYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	END SELECT

2007	TOTAL_COST = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM + "", &
		UTL_PROFILE::DEFLOCATION, EFF_DATE$, EFFDATE$)

	TOTAL_MAT = 0.0
 ! print "TOTAL MAT = 0"
 ! input x$

	!
	! Put the junk on the screen
	!
	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, PD_PRODUCT::PRODUCT_NUM, &
		2%, 20%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, PD_PRODUCT::DESCRIPTION, &
		3%, 20%, , SMG$M_BOLD)

	WHEN ERROR IN
		FIND #BM_RELATION.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM + "", &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR=155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	!*******************************************************************
	! Main loop starts here
	!*******************************************************************

	WHEN ERROR IN
		GET #BM_RELATION.CH%, REGARDLESS
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE IF ERR = 11%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	ITEM_LOOP% = 0%
	QTY_LEVEL(0%), LEVEL% = 1%

	!
	! Add this item to new level
	!
 GoDownTree:
	TEST_PRODUCT(LEVEL%) = BM_RELATION::PRODUCT
	QTY_LEVEL(LEVEL%) = QTY_LEVEL(LEVEL% - 1%) * BM_RELATION::QUANTITY
	RFA_LEVEL(LEVEL%) = GETRFA(BM_RELATION.CH%)

2020	!
	! Try for another (deeper) level
	!
	WHEN ERROR IN
		GET #BM_RELATION.CH%, &
			KEY #0% EQ BM_RELATION::COMPONENT + "", &
			REGARDLESS
	USE
		IF ERR=154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 2025 IF ERR = 155%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	!
	! Test for eternal loop
	!
	FOR L% = 1% TO LEVEL%

		IF BM_RELATION::COMPONENT = TEST_PRODUCT(LEVEL%)
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Infinite Tree by " + &
				TRM$(BM_RELATION::PRODUCT) + " -> "      + &
				TRM$(BM_RELATION::COMPONENT), 0%)

			GOTO ExitProgram
		END IF

	NEXT L%

	!
	! Item seems ok, so try to go even deeper
	!
	LEVEL% = LEVEL% + 1%
	GOTO GoDownTree

	!
	! We can't go any lower, so add this item to compoment list,
	! and back up one level.
	!
2025	GOSUB 18000
	GOTO 2030

	!
	! Climb up one level, if there is a level to climb up to.
	!
 GoUpTree:
	GOTO AddToPriceFile IF LEVEL% = 1%

	LEVEL% = LEVEL% - 1%

2030	!
	! Try for next component on this level
	!
	WHEN ERROR IN
		GET #BM_RELATION.CH%, RFA RFA_LEVEL(LEVEL%), REGARDLESS

		GET #BM_RELATION.CH%, REGARDLESS
	USE
		IF ERR=154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE GoUpTree IF ERR = 155% OR ERR = 11%
		FILENAME$ = "BM_RELATION"
		CONTINUE HelpError
	END WHEN

	IF BM_RELATION::PRODUCT <> TEST_PRODUCT(LEVEL%)
	THEN
		!
		! Different product, go up yet another level
		!
		GOTO GoUpTree
	ELSE
		!
		! Still on same product, go down to level under this part
		! if possible.
		!
		GOTO GoDownTree
	END IF

 AddToPriceFile:
	FOR I% = 1% TO ITEM_LOOP%

		V% = PD_EXAM_PRODUCT(COMPONENT(I%)::NUMBER, PD_PRODUCT_EXAM)

		PD_PRODUCT_EXAM::PRODUCT_FACTOR = 1.0 IF &
			PD_PRODUCT_EXAM::PRODUCT_FACTOR = 0.0

		COST = PC_READ_COST(COMPONENT(I%)::NUMBER, &
			UTL_PROFILE::DEFLOCATION, EFF_DATE$, "")

		COST = COST / PD_PRODUCT_EXAM::PRODUCT_FACTOR

		TOTAL_MAT = TOTAL_MAT + &
			FUNC_ROUND(COMPONENT(I%)::QUANTITY * COST, 6%)

 ! print "Debug: ["; COMPONENT(I%)::NUMBER; "] ="; &
 ! COMPONENT(I%)::QUANTITY * COST; "("; &
 ! COMPONENT(I%)::QUANTITY; "*"; COST; "TOTAL_MAT ="; TOTAL_MAT
 ! input x$

	NEXT I%

	LAST_HOURS, LAST_LABOR, TOTAL_LABOR, TOTAL_HOURS = 0.0

	LAST_OPERATION$ = SPACE$(LEN(BM_PRODOPER::OPERATION) + 1%)

2035	WHEN ERROR IN
		FIND #BM_PRODOPER.CH%, &
			KEY #1% EQ TEST_PRODUCT(1%) + "", &
			REGARDLESS
	USE
		CONTINUE DisplayTotals IF ERR = 155%
		FILENAME$ = "BM_PRODOPER"
		CONTINUE HelpError
	END WHEN

2037	WHEN ERROR IN
		GET #BM_PRODOPER.CH%, REGARDLESS
	USE
		IF ERR=154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE DisplayTotals IF ERR = 11%
		FILENAME$ = "BM_PRODOPER"
		CONTINUE HelpError
	END WHEN

	GOTO DisplayTotals IF BM_PRODOPER::PRODUCT <> TEST_PRODUCT(1%)

	GOTO 2037 IF BM_PRODOPER::STAT <> "A"
 !	GOTO 2037 IF BM_PRODOPER::EFFDATE > EFF_DATE$

	IF BM_PRODOPER::OPERATION <> LAST_OPERATION$
	THEN
		TOTAL_HOURS = TOTAL_HOURS + BM_PRODOPER::HOURS
		LABOR = 0.0
	ELSE
		TOTAL_HOURS = TOTAL_HOURS + BM_PRODOPER::HOURS - LAST_HOURS
	END IF

	LAST_HOURS = BM_PRODOPER::HOURS
	LAST_OPERATION$ = BM_PRODOPER::OPERATION

2039	IF PR_READ_OPERATION(BM_PRODOPER::OPERATION, &
		"", PR_OPER_READ) = CMC$_NORMAL
	THEN
		HOUR_RATE = PR_OPER_READ::HOUR_RATE
	ELSE
		HOUR_RATE = 0.0
	END IF

	TOTAL_LABOR = TOTAL_LABOR + &
		FUNC_ROUND(HOUR_RATE * BM_PRODOPER::HOURS, 6%) - LABOR

	LABOR = FUNC_ROUND(HOUR_RATE * BM_PRODOPER::HOURS, 6%)

	GOTO 2037

 DisplayTotals:
	IF BM_CONTROL::BURDENPERC <> 0.0
	THEN
		TOTAL_BURDEN = FUNC_ROUND(TOTAL_LABOR * &
			BM_CONTROL::BURDENPERC / 100.0, 6%)
	ELSE
		TOTAL_BURDEN = FUNC_ROUND(TOTAL_HOURS * &
			BM_CONTROL::BURDENRATE, 6%)
	END IF

	COST = TOTAL_MAT + TOTAL_LABOR + TOTAL_BURDEN

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, &
		FORMAT$(TOTAL_MAT, "###,###.##"), &
		5%, 22%,,)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, FORMAT$(TOTAL_LABOR, &
		"###,###.##"), 6%, 22%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, FORMAT$(TOTAL_HOURS, &
		"###,###.###"), 6%, 49%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, FORMAT$(TOTAL_BURDEN, &
		"###,###.##"), &
		7%, 22%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, FORMAT$(COST, "###,###.##"), &
		9%, 22%,, SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, FORMAT$(TOTAL_COST, &
		"###,###.##"), 9%, 49%,,)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WIN2, FORMAT$(TOTAL_COST - COST, &
		"###,###.##"), 10%, 49%,,)

	SELECT OPT$
	CASE "O"
		CALL ENTR_3MESSAGE(SCOPE, "", 1%)
		COST = ENTR_3NUMBER(SCOPE, SMG_WIN2, "9;22", "Total Cost", &
			TOTAL_COST, FLG%, "###,###.##", DEFLT$)

		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_NEXT
			GOTO 2100

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO OutAWin2

		END SELECT

	CASE "L"
		CALL ENTR_3MESSAGE(SCOPE, "", 1%)
		TOTAL_LABOR = ENTR_3NUMBER(SCOPE, SMG_WIN2, "6;22", &
			"Total Labor", TOTAL_LABOR, FLG%, "###,###.##", DEFLT$)
		SELECT SCOPE::SCOPE_EXIT

		CASE SMG$K_TRM_NEXT
			GOTO 2100

		CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
			GOTO OutAWin2

		END SELECT

		COST = TOTAL_MAT + TOTAL_LABOR + TOTAL_BURDEN
	END SELECT

2050	SELECT OPT$
	CASE "L", "O"
		IF OPERHOUR_RATE
		THEN
			TOTAL_HOURS = FUNC_ROUND((COST - TOTAL_MAT - &
				TOTAL_BURDEN) / OPERHOUR_RATE, 6%)
		ELSE
			TOTAL_HOURS = 0.0
		END IF

		ITEM% = 0%
		ORIG_HOURS = 0.0

		WHEN ERROR IN
			FIND #BM_PRODOPER.CH%, &
				KEY #1% EQ TEST_PRODUCT(1%) + OPERATION$
		USE
			CONTINUE 2060 IF ERR = 155%
			FILENAME$ = "BM_PRODOPER"
			CONTINUE HelpError
		END WHEN

2055		WHEN ERROR IN
			GET #BM_PRODOPER.CH%
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF
			CONTINUE 2060 IF ERR = 11%
			FILENAME$ = "BM_PRODOPER"
			CONTINUE HelpError
		END WHEN

		GOTO 2060 IF OPERATION$ + TEST_PRODUCT(1%) <> &
			BM_PRODOPER::OPERATION + BM_PRODOPER::PRODUCT

		GOTO 2055 IF BM_PRODOPER::STAT <> "A"

		ITEM% = ITEM% + 1%
		IF BM_PRODOPER::EFFDATE < EFF_DATE$
		THEN
			ORIG_HOURS = BM_PRODOPER::HOURS
			GOTO 2055
		END IF

		IF BM_PRODOPER::EFFDATE = EFF_DATE$
		THEN
			IF BM_PRODOPER::HOURS <> TOTAL_HOURS
			THEN
				BM_PRODOPER::HOURS = TOTAL_HOURS
				WHEN ERROR IN
					UPDATE #BM_PRODOPER.CH%
				USE
					FILENAME$ = "BM_PRODOPER"
					CONTINUE HelpError
				END WHEN
			END IF
			GOTO 2070
		END IF

2060		IF ORIG_HOURS <> TOTAL_HOURS
		THEN
			ITEM% = ITEM% + 1%
			BM_PRODOPER::PRODUCT   = TEST_PRODUCT(1%)
			BM_PRODOPER::ITEMNUM   = FORMAT$(ITEM%, "<0>###")
			BM_PRODOPER::OPERATION = OPERATION$
			BM_PRODOPER::HOURS     = TOTAL_HOURS
			BM_PRODOPER::EFFDATE   = EFF_DATE$
			BM_PRODOPER::STAT      = "A"

			WHEN ERROR IN
				PUT #BM_PRODOPER.CH%
			USE
				CONTINUE 2060 IF ERR = 134%
				FILENAME$ = "BM_PRODOPER"
				CONTINUE HelpError
			END WHEN
		END IF

		WHEN ERROR IN
			FIND #BM_PRODOPER.CH%, KEY #0% EQ TEST_PRODUCT(1%) + ""
		USE
			FILENAME$ = "BM_PRODOPER"
			CONTINUE HelpError
		END WHEN

2070		WHEN ERROR IN
			GET #BM_PRODOPER.CH%

			GOTO 2100 IF TEST_PRODUCT(1%) <> &
				BM_PRODOPER::PRODUCT

			GOTO 2070 IF OPERATION$ + TEST_PRODUCT(1%) = &
				BM_PRODOPER::OPERATION + BM_PRODOPER::PRODUCT

			BM_PRODOPER::STAT      = "I"
			UPDATE #BM_PRODOPER.CH%
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF
			CONTINUE 2100 IF ERR = 11%
			FILENAME$ = "BM_PRODOPER"
			CONTINUE HelpError
		END WHEN

		GOTO 2070
	END SELECT

2100	IF BM_CONTROL::BURDENPERC <> 0.0
	THEN
		TOTAL_BURDEN = &
			FUNC_ROUND(TOTAL_LABOR * &
			BM_CONTROL::BURDENPERC / 100.0, 6%)
	ELSE
		TOTAL_BURDEN = FUNC_ROUND(TOTAL_HOURS * &
			BM_CONTROL::BURDENRATE, 2%)
	END IF

	COST = TOTAL_MAT + TOTAL_LABOR + TOTAL_BURDEN

	IF COST = TOTAL_COST
	THEN
		GOTO GetNextRec
	END IF

 ! CALL ENTR_3MESSAGE(SCOPE, "TEST: Product=" + TEST_PRODUCT(1%) + &
 !	",  TotalLabor = " + num1$(TOTAL_LABOR) + &
 !	"Cost=" + num1$(COST), 1% + 16%)

2110	!
	! Update the cost
	!
 ! print "Debug: TOTAL_MAT ="; TOTAL_MAT
 ! input x$
	WHEN ERROR IN
		GET #PC_COST.CH%, KEY #0% EQ TEST_PRODUCT(1%) + &
			UTL_LOCATION::LOCATION + EFF_DATE$
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 2120 IF ERR = 155%
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

	PC_COST::COST = COST

	WHEN ERROR IN
		UPDATE #PC_COST.CH%
	USE
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec

2120	PC_COST::PRODUCT  = TEST_PRODUCT(1%)
	PC_COST::LOCATION = UTL_LOCATION::LOCATION
	PC_COST::EFFDATE  = EFF_DATE$
	PC_COST::COST     = COST

	WHEN ERROR IN
		PUT #PC_COST.CH%
	USE
		FILENAME$ = "PC_COST"
		CONTINUE HelpError
	END WHEN

	GOTO GetNextRec

 OutAWin2:
	SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SMG_WIN2, SCOPE::SMG_PBID)
	CALL ENTR_3MESSAGE(SCOPE, "", 1%)
	RETURN

	%PAGE

18000	!*******************************************************************
	! Array of terminals
	!*******************************************************************

	FOR I% = 1% TO ITEM_LOOP%

		IF BM_RELATION::COMPONENT = COMPONENT(I%)::NUMBER
		THEN
			COMPONENT(I%)::QUANTITY = COMPONENT(I%)::QUANTITY + &
				FUNC_ROUND(QTY_LEVEL(LEVEL%), 6%)

			GOTO EndTerm
		END IF

	NEXT I%

	V% = PD_EXAM_PRODUCT(BM_RELATION::COMPONENT, PD_PRODUCT_EXAM)

18010	IF COMP_STRING(PD_PRODUCT_EXAM::PROD_TYPE, BM_CONTROL::PRODTYPE)
	THEN
		ITEM_LOOP% = ITEM_LOOP% + 1%
		COMPONENT(ITEM_LOOP%)::NUMBER = BM_RELATION::COMPONENT
		COMPONENT(ITEM_LOOP%)::QUANTITY = &
			FUNC_ROUND(QTY_LEVEL(LEVEL%), 6%)
	END IF

 EndTerm:
	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitProgram

	END

20000	FUNCTION LONG MAINT_GROUP(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "FUNC_INCLUDE:PR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"

	EXTERNAL LONG FUNCTION PR_MAIN_OPER
	EXTERNAL LONG FUNCTION UTL_MAIN_LOCATION

	%PAGE

	!
	! Process the proper window
	!
	SELECT SMG_WINDOW::IDENT

	CASE PR_MAIN_OPER.ID

		MAINT_GROUP = PR_MAIN_OPER(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	CASE UTL_MAIN_LOCATION.ID

		MAINT_GROUP = UTL_MAIN_LOCATION(SMG_WINDOW, &
			MOPTION, MLOOP, MFLAG, MVALUE)

	END SELECT

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:ALABOR
	!	^*Alabor\*
	!	.lm +5
	!	.b
	!	Actual Labor.
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:ACOST
	!	^*acosT\*
	!	.lm +5
	!	.b
	!	Actual Cost.
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:MLABOR
	!	^*mcost\*
	!	.lm +5
	!	.b
	!	Material cost.
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:MCOST
	!	^*mcost\*
	!	.lm +5
	!	.b
	!	Material Cost\*
	!	.lm -5
	!
	! Index:
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD001
	!	^*(01) Date\*
	!	.b
	!	.lm +5
	!	The ^*Date\* field confirms the date of
	!	this transaction. The system will automatically default
	!	to the current date. The date may be changed to a desired
	!	date by using the ^*Change\* option in the COMMAND menu.
	!	.lm -5
	!
	! Index:
	!	.x Date>Update Standard Cost
	!	.x Update Standard Cost>Date
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD002
	!	^*(02) Product _#\*
	!	.b
	!	.lm +5
	!	The ^*Product _#\* field enters an assigned
	!	number which identifies a specific product.
	!	.b
	!	The field will accommodate up to fourteen (14) alphanumeric
	!	characters.
	!	.b
	!	Pressing ^*<List Choices>\*, while the cursor is located at
	!	this field, will cause a list of valid Product _#'s to be
	!	displayed.
	!	.lm -5
	!
	! Index:
	!	.x Product Number>Update Standard Cost
	!	.x Update Standard Cost>Product Number
	!	.x Number>Product
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD003
	!	^*(03) Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* code enters the location
	!	where the new cost will be added and calculated.
	!	.b
	!	Pressing ^*<List Choices>\*, while the cursor is located
	!	at this field, will cause a list of valid location codes
	!	to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Location>Update Standard Cost
	!	.x Update Standard Cost>Location
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD004
	!	^*(04) Component Type\*
	!	.b
	!	.lm +5
	!	The ^*Component Type\* field enters
	!	a code for a particular ingredient which has been
	!	established in the Component Type Table.
	!	.b
	!	The field will accommodate two (2) alphanumeric
	!	characters.
	!	.lm -5
	!
	! Index:
	!	.x Component Type>Update Standard Cost
	!	.x Update Standard Cost>Component Type
	!	.x Type>Component
	!
	!--
