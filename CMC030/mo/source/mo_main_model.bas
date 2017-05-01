1	%TITLE "Manufacturing Orders Model Master Maintenance"
	%SBTTL "MO_MAIN_MODEL"
	%IDENT "V3.6a Calico"

	FUNCTION LONG MO_MAIN_MODEL(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1991 BY
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Information concerning a particular Model and its associated options is
	!	maintained through the ^*Model Master Maintenance\*.
	!	.b
	!	The ^*Model Master\* screen contains the following fields:
	!	.lm 15
	!	.b
	!	.LIST 0,"*"
	!	.LE
	!	(01) Model
	!	.le
	!	(02) Make Size
	!	.le
	!	(03) Make Class
	!	.le
	!	(04) Inventory Product Number
	!	.els
	!	.lm -10
	!
	! Index:
	!	.x Maintenance>Model
	!	.x Model>Maintenance
	!
	! Option:
	!
	! Author:
	!	02/27/91 - Val James Allen
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_MAIN_MODEL/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN MO_MAIN_MODEL
	!	$ DELETE MO_MAIN_MODEL.OBJ;*
	!
	! Modification history:
	!
	!	08/19/91 - Dan Perkins
	!		Added F17 key function.
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	01/24/92 - Frank F. Starman
	!		Added a new field. Box kit product number.
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/25/93 - Dan Perkins
	!		Fixed OPT_VIEW and OPT_FIND to work properly.
	!
	!	07/24/93 - Frank F. Starman
	!		Do not display a box kit product number.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/26/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in
	!		several places.
	!
	!	08/14/96 - Kevin Handy
	!		Reformat goofy 'if' statement.
	!
	!	08/15/96 - Kevin Handy
	!		Remove another goofy 'if' statement.
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/31/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MO_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODEL.HB"
	MAP (MO_MODEL)		MO_MODEL_CDD		MO_MODEL
	MAP (MO_MODEL_OLD)	MO_MODEL_CDD		MO_MODEL_OLD, MO_MODEL2

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKECLASS.HB"
	MAP (MO_MAKECLASS)	MO_MAKECLASS_CDD	MO_MAKECLASS

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKESIZE.HB"
	MAP (MO_MAKESIZE)	MO_MAKESIZE_CDD		MO_MAKESIZE

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODELLINE.HB"
	MAP (MO_MODELLINE)	MO_MODELLINE_CDD	MO_MODELLINE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODELCODE.HB"
	MAP (MO_MODELCODE)	MO_MODELCODE_CDD	MO_MODELCODE

	!
	! Common Statements
	!
	COM (CH_MO_MODEL) &
		MO_MODEL.CH%, &
		MO_MODEL.READONLY%

	COM (CH_MO_MODELLINE) &
		MO_MODELLINE.CH%, &
		MO_MODELLINE.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	DIM MO_MODELLINE_CDD KEEPMODEL(200%)

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION
	!
	! Initilization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Model Master Maintenance"
		SMG_WINDOW::NHELP = "MO_MAIN_MODEL"
		SMG_WINDOW::CHAN  = MO_MODEL.CH%
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 8%
		SMG_WINDOW::HVIEW = 78%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 4%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Model"
			SMG_WINDOW::KFIELD(0%, 0%) = 3%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
			SMG_WINDOW::KFIELD(0%, 3%) = 3%

		SMG_WINDOW::KNAME(1%) = "Product"
			SMG_WINDOW::KFIELD(1%, 0%) = 4%
			SMG_WINDOW::KFIELD(1%, 1%) = 4%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%
			SMG_WINDOW::KFIELD(1%, 3%) = 2%
			SMG_WINDOW::KFIELD(1%, 4%) = 3%

		IF INSTR(1%, " QV", MVALUE) <= 1%
		THEN
			!
			! Load in defaults
			!
			CALL READ_DEFAULTS(SMG_WINDOW)
		END IF

700		!
		! Declare channels
		!
		IF MO_MODEL.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF MO_MODEL.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_MODEL.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			MO_MAIN_MODEL = ERR
			CONTINUE 770
		END WHEN

		MO_MODEL.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_MODEL.OPN"
		USE
			MO_MAIN_MODEL = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		MO_MODEL.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(MO_MODEL.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = MO_MODEL.CH%

		WHEN ERROR IN
			RESET #MO_MODEL.CH%
			GET #MO_MODEL.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
		END WHEN

800		IF MO_MAIN_MODELLINE.CH% <= 0%
		THEN
			WHEN ERROR IN
				%INCLUDE "SOURCE:[MO.OPEN]MO_MODELLINE.CRE"
			USE
				MO_MAIN_MODEL = ERR
				CONTINUE 810
			END WHEN
		END IF

810		!

		!
		! Display the background
		!
		! This option is used to display the background information
		! on the screen.  It must first clear any junk on the screen,
		! and then write the background onto it.
		!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)
		DATA	02, 03, "(01) Model Code", &
			03, 03, "(02) Make Size", &
			04, 03, "(03) Make Class", &
			05, 03, "(04) Final Product#", &
			0,  0, ""

		RESTORE

		READ XPOS%, YPOS%, XSTR$
		I% = 0%
		WHILE (XPOS% <> 0%)
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS%, YPOS%) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
			READ XPOS%, YPOS%, XSTR$
		NEXT

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	CASE OPT_OPTLIST
		MVALUE = MVALUE + " copY"


	CASE OPT_MOREMENU

		SELECT EDIT$(MVALUE, -1%)

		CASE "COPY"
			GOSUB CopyOption

		END SELECT

		!
		! Enter/Display/Default
		!
		! This option is used to enter the data from the user, display
		! data, set defaults, and return the data back according
		! to MFLAG.
		!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Model Code\*
	!	.b
	!	.lm +5
	!	The ^*Model Code\* field enters the Model code for
	!	attaching options to.
	!	.b
	!	An entry is required in this field. The field will
	!	accommodate four (4) alphanumeric characters.
	!	.b
	!	Pressing ^*List Choices\* while the cursor is located at this
	!	field displays a list of valid Model Codes.  Additional Model Codes
	!	may be added by pressing ^*F17\*.
	!	.lm -5
	!
	! Index:
	!	.x Model Code
	!
	!--

		MO_MODEL::MODELCODE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
			"2;25", TEMP$, &
			MO_MODEL::MODELCODE, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF (MAIN_WINDOW(MO_MAIN_MODELCODE.ID, "VX") = 1%)
				THEN
					MO_MODEL::MODELCODE = MO_MODELCODE::MODELCODE
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(MO_MAIN_MODELCODE.ID, "M")
				MO_MODEL::MODELCODE = MO_MODELCODE::MODELCODE
				GOTO ReEnter
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Make Size\*
	!	.b
	!	.lm +5
	!	The ^*Make Size\* field enters the size
	!	of the Make.
	!	.b
	!	The field will accommodate four (4) alphanumeric characters.
	!	.b
	!	Pressing ^*List Choices\* while the cursor is located at this
	!	field will display a list of Make sizes.  Additional Make Size codes
	!	may be added by pressing ^*F17\*.
	!	.lm -5
	!
	! Index:
	!	.x Make Size
	!
	!--

		MO_MODEL::MSIZE = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
			"3;25", TEMP$, &
			MO_MODEL::MSIZE, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF (MAIN_WINDOW(MO_MAIN_MAKESIZE.ID, "VX") = 1%)
				THEN
					MO_MODEL::MSIZE = MO_MAKESIZE::MSIZE
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(MO_MAIN_MAKESIZE.ID, "M")
				MO_MODEL::MSIZE = MO_MAKESIZE::MSIZE
				GOTO ReEnter
			END IF


		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Make Class\*
	!	.b
	!	.lm +5
	!	The ^*Make Class\* field enters the class
	!	of the Make. This field will accommodate four (4) alphanumeric characters.
	!	.b
	!	Pressing ^*List Choices\* while the cursor is located at this
	!	field displays a list of Make classes.  Additional make class codes
	!	may be added by pressing ^*F17\*.
	!	.lm -5
	!
	! Index:
	!	.x Make Class
	!
	!--

		MO_MODEL::CLASS = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
			"4;25", TEMP$, &
			MO_MODEL::CLASS, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				IF (MAIN_WINDOW(MO_MAIN_MAKECLASS.ID, "VX") = 1%)
				THEN
					MO_MODEL::CLASS = MO_MAKECLASS::CLASS
				END IF
				GOTO ReEnter
			END IF

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
			THEN
				V% = MAIN_WINDOW(MO_MAIN_MAKECLASS.ID, "M")
				MO_MODEL::CLASS = MO_MAKECLASS::CLASS
				GOTO ReEnter
			END IF

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Final Product _#\*
	!	.b
	!	.lm +5
	!	The ^*Final Product _#\* field enters the number
	!	of the product to which the specified record refers.
	!	.b
	!	The ^*List Choices\* key displays a list of the valid product numbers.  The
	!	field will accommodate fourteen (14) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x MODEL>Final Product
	!	.x Final Product>MODEL
	!
	!--

				MO_MODEL::PRODUCT = ENTR_3STRING(SCOPE, &
					SMG_WINDOW::WNUMBER, "5;25", TEMP$, &
					MO_MODEL::PRODUCT, MFLAG, "'E", &
					MVALUE)

				IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
				THEN
					IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
					THEN
						MO_MODEL::PRODUCT = &
							PD_PRODUCT::PRODUCT_NUM
					END IF
					GOTO Reenter
				END IF

				IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
				THEN
					V% = MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "M")
					MO_MODEL::PRODUCT = PD_PRODUCT::PRODUCT_NUM
					GOTO ReEnter
				END IF

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Box Kit Product _#\*
	!	.b
	!	.lm +5
	!	The ^*Box Kit Product _#\* field enters the number of the product
	!	related to a box kit of the model.
	!	.b
	!	The ^*List Choices\* key displays a list of the valid product numbers.  The
	!	field will accommodate fourteen (14) alphanumeric characters.
	!	.lm -5
	!
	! Index:
	!	.x MODEL>Box Kit Product
	!	.x Box Kit Product>MODEL
	!
	!--

				MO_MODEL::BPRODUCT = ENTR_3STRING(SCOPE, &
					SMG_WINDOW::WNUMBER, "6;25", TEMP$, &
					MO_MODEL::BPRODUCT, MFLAG, "'E", &
					MVALUE)

				IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
				THEN
					IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX") = 1%
					THEN
						MO_MODEL::BPRODUCT = &
							PD_PRODUCT::PRODUCT_NUM
					END IF
					GOTO Reenter
				END IF

				IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F17
				THEN
					V% = MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "M")
					MO_MODEL::BPRODUCT = PD_PRODUCT::PRODUCT_NUM
					GOTO ReEnter
				END IF


		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		MO_MAIN_MODEL = 0%

		SELECT MLOOP

		CASE 1%
			MO_MAIN_MODEL = FUNC_TESTENTRY(SMG_WINDOW, &
				MO_MODEL::MODELCODE, &
				MO_MODELCODE::DESCR, &
				"MO", MLOOP, "PROG", &
				"Model Code", MO_MAIN_MODELCODE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MODELCODE::DESCR, 2%, 35%,, SMG$M_BOLD)

		CASE 2%
			IF MO_MODEL::MSIZE <> ""
			THEN
				MO_MAIN_MODEL = FUNC_TESTENTRY(SMG_WINDOW, &
					MO_MODEL::MSIZE, &
					MO_MAKESIZE::DESCR, &
					"MO", MLOOP, "PROG", &
					"Make Size", MO_MAIN_MAKESIZE.ID)
			ELSE
				MO_MAKESIZE::DESCR = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MAKESIZE::DESCR, 3%, 35%,, &
				SMG$M_BOLD)

		CASE 3%
			IF MO_MODEL::CLASS <> ""
			THEN
				MO_MAIN_MODEL = FUNC_TESTENTRY(SMG_WINDOW, &
					MO_MODEL::CLASS, &
					MO_MAKECLASS::DESCR, &
					"MO", MLOOP, "PROG", &
					"Make Class", MO_MAIN_MAKECLASS.ID)
			ELSE
				MO_MAKECLASS::DESCR = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MAKECLASS::DESCR, 4%, 35%,, &
				SMG$M_BOLD)

				SELECT MVALUE

				CASE "ADD"
					WHEN ERROR IN
						GET #MO_MODEL.CH%, &
							KEY #0% EQ MO_MODEL::MODELCODE + MO_MODEL::MSIZE + MO_MODEL::CLASS, &
							REGARDLESS
					USE
						CONTINUE ExitProgram IF ERR = 155%
						EXIT HANDLER
					END WHEN

					MO_MAIN_MODEL = 2%
					CALL ENTR_3MESSAGE(SCOPE, "Record Already Exists", 1%)

				END SELECT

		CASE 4%
			MO_MAIN_MODEL = FUNC_TESTENTRY(SMG_WINDOW, &
				MO_MODEL::PRODUCT, &
				PD_PRODUCT::DESCRIPTION, &
				"MO", MLOOP, "PROG", &
				"Product", PD_MAIN_PRODUCT.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 5%, 40%,, SMG$M_BOLD)

		CASE 5%
			IF MO_MODEL::BPRODUCT <> ""
			THEN
				MO_MAIN_MODEL = FUNC_TESTENTRY(SMG_WINDOW, &
					MO_MODEL::BPRODUCT, &
					PD_PRODUCT::DESCRIPTION, &
					"MO", MLOOP, "PROG", &
					"Product", PD_MAIN_PRODUCT.ID)
			ELSE
				PD_PRODUCT::DESCRIPTION = SPACE$( &
					LEN(PD_PRODUCT::DESCRIPTION))
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 6%, 40%,, SMG$M_BOLD)

		END SELECT

	!
	! Test option
	!
	CASE OPT_TESTOPT
		MO_MAIN_MODEL = 0%

	CASE OPT_DISPLAY
		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(MO_MAIN_MODELCODE.ID, &
				"Q0" + MO_MODEL::MODELCODE) <> 1%
			THEN
				MO_MODELCODE::DESCR = STRING$(40%, A"?"B)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MODELCODE::DESCR, 2%, 35%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(2%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(MO_MAIN_MAKESIZE.ID, &
				"Q0" + MO_MODEL::MSIZE) <> 1%
			THEN
				MO_MAKESIZE::DESCR = STRING$(40%, A"?"B)
			END IF
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MAKESIZE::DESCR, 3%, 35%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(MO_MAIN_MAKECLASS.ID, &
				"Q0" + MO_MODEL::CLASS) <> 1%
			THEN
				MO_MAKECLASS::DESCR = STRING$(40%, A"?"B)
			END IF
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				MO_MAKECLASS::DESCR, 4%, 35%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, &
				"Q0" + MO_MODEL::PRODUCT) <> 1%
			THEN
				PD_PRODUCT::DESCRIPTION = STRING$(40%, A"?"B)
			END IF
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 5%, 40%,, SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(5%) AND 2%) = 0%
		THEN
			IF MO_MODEL::BPRODUCT <> ""
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, &
					"Q0" + MO_MODEL::BPRODUCT) <> 1%
				THEN
					PD_PRODUCT::DESCRIPTION = STRING$(40%, A"?"B)
				END IF
			ELSE
				PD_PRODUCT::DESCRIPTION = SPACE$( &
					LEN(PD_PRODUCT::DESCRIPTION))
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::DESCRIPTION, 6%, 40%,, SMG$M_BOLD)
		END IF


	!
	! Set MO_MODEL_OLD value
	!
20500	CASE OPT_SETOLD
		MO_MODEL_OLD = MO_MODEL

	!
	! Restore MO_MODEL_OLD value
	!
	CASE OPT_RESETOLD
		MO_MODEL = MO_MODEL_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		MO_MODEL2 = MO_MODEL

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		MO_MODEL = MO_MODEL2

	!
	! View header
	!
	CASE OPT_VIEW

		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Model Size Class FinProduct     BoxKitProd"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "008,013,019,034"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = MO_MODEL::MODELCODE + "  " + &
				MO_MODEL::MSIZE + " "  + &
				MO_MODEL::CLASS + "  " + &
				MO_MODEL::PRODUCT + " " + &
				MO_MODEL::BPRODUCT
		END SELECT
	!
	! Find
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #MO_MODEL.CH%, KEY #0% &
				GE MO_MODEL::MODELCODE + &
				MO_MODEL::MSIZE + &
				MO_MODEL::CLASS, REGARDLESS

		CASE 1%
			FIND #MO_MODEL.CH%, KEY #1% &
				GE MO_MODEL::PRODUCT + &
				MO_MODEL::MODELCODE + &
				MO_MODEL::MSIZE + &
				MO_MODEL::CLASS, REGARDLESS

		END SELECT

	!
	! Handle finishing various options specially
	!
	CASE OPT_AFTEROPT

		SELECT SCOPE::PRG_ITEM

		!
		! Add records
		!
		CASE "Add"
			!
			! Add line items also
			!
			MO_MAIN_MODEL = MAIN_WINDOW(MO_MAIN_MODEL_LINE.ID, "A")

		!
		! Change records
		!
		CASE "Change"
			!
			! Change line items to match new header
			! if the key was changed.
			!
			! The original record must be the one in the
			! MAP for this to be able to work.  The new
			! key is passed through the QUERY$ variable.
			!
			IF MO_MODEL_OLD::MODELCODE = MO_MODEL::MODELCODE AND &
				MO_MODEL_OLD::MSIZE = MO_MODEL::MSIZE AND &
				MO_MODEL_OLD::CLASS = MO_MODEL::CLASS
			THEN
				GOTO ExitProgram
			END IF

			IF MO_MODELLINE.CH% > 0%
			THEN
				GOTO 20650
			END IF

20600			%INCLUDE "SOURCE:[MO.OPEN]MO_MODELLINE.CRE"
			MO_MODELLINE.READONLY% = 0%

20650			WHEN ERROR IN
				FIND #MO_MODELLINE.CH%, &
					KEY #0% GE MO_MODEL_OLD::MODELCODE + &
					MO_MODEL_OLD::MSIZE + &
					MO_MODEL_OLD::CLASS
			USE
				CONTINUE ExitProgram IF ERR = 155%
				EXIT HANDLER
			END WHEN

20660			GET #MO_MODELLINE.CH%

			GOTO ExitProgram &
				IF MO_MODELLINE::MODELCODE <> MO_MODEL_OLD::MODELCODE OR &
				MO_MODELLINE::MSIZE <> MO_MODEL_OLD::MSIZE OR &
				MO_MODELLINE::CLASS <> MO_MODEL_OLD::CLASS

20665			DELETE #MO_MODELLINE.CH%

			MO_MODELLINE::MODELCODE = MO_MODEL::MODELCODE
			MO_MODELLINE::MSIZE = MO_MODEL::MSIZE
			MO_MODELLINE::CLASS = MO_MODEL::CLASS

20700			PUT #MO_MODELLINE.CH%

			GOTO 20650

		!
		! Erase record
		!
		CASE "Erase", "DELCNF"
			!
			! Erase any line items under the header
			!
			IF MO_MODELLINE.CH% > 0%
			THEN
				GOTO 20850
			END IF

20800			%INCLUDE "SOURCE:[MO.OPEN]MO_MODELLINE.CRE"
			MO_MODELLINE.READONLY% = 0%

20850			WHEN ERROR IN
				FIND #MO_MODELLINE.CH%, &
					KEY #0% GE MO_MODEL_OLD::MODELCODE + &
					MO_MODEL_OLD::MSIZE + &
					MO_MODEL_OLD::CLASS
			USE
				CONTINUE ExitProgram IF ERR = 155%
				EXIT HANDLER
			END WHEN

20860			GET #MO_MODELLINE.CH%

			GOTO ExitProgram &
				IF MO_MODELLINE::MODELCODE <> MO_MODEL_OLD::MODELCODE OR &
				MO_MODELLINE::MSIZE <> MO_MODEL_OLD::MSIZE OR &
				MO_MODELLINE::CLASS <> MO_MODEL_OLD::CLASS

20865			DELETE #MO_MODELLINE.CH%

			GOTO 20850

		END SELECT

	END SELECT

 ExitProgram:
	EXIT FUNCTION

	!*********************************************************************
	! Option to copy info from another model
	!*********************************************************************
 CopyOption:
22000	!
	! Ask what to copy from
	!
	REASON$ = ""
	FROMCODE$ = SPACE$(4%)
	FROMCODE$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
		"", "Model Code to Copy From", &
		FROMCODE$, 0%, "'E", "")
	GOTO 22890 IF FROMCODE$ = ""

22010	FROMSIZE$ = SPACE$(4%)
	FROMSIZE$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
		"", "Model Size to Copy From", &
		FROMSIZE$, 0%, "'E", "")
	GOTO 22890 IF FROMSIZE$ = ""

22020	FROMCLASS$ = SPACE$(4%)
	FROMCLASS$ = ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
		"", "Model Class to Copy From", &
		FROMCLASS$, 0%, "'E", "")
	GOTO 22890 IF FROMCLASS$ = ""

22100	!
	! Find first model line in copied product
	!
	KEEPMODEL% = 0%
	WHEN ERROR IN
		GET #MO_MODELLINE.CH%, &
			KEY #0% EQ FROMCODE$ + FROMSIZE$ + FROMCLASS$, &
			REGARDLESS
	USE
		REASON$ = ERT$(ERR)
		CONTINUE CopyFailure
	END WHEN

	!
	! Stash away al of the lines
	!
	WHILE (MO_MODELLINE::MODELCODE = FROMCODE$ AND &
		MO_MODELLINE::MSIZE = FROMSIZE$ AND &
		MO_MODELLINE::CLASS = FROMCLASS$)

		KEEPMODEL% = KEEPMODEL% + 1%
		KEEPMODEL(KEEPMODEL%) = MO_MODELLINE

		WHEN ERROR IN
			GET #MO_MODELLINE.CH%, REGARDLESS
		USE
			MO_MODELLINE::MODELCODE = ""
		END WHEN
	NEXT

	!
	! Now write them out marking with current model data
	!
	FOR LOOP% = 1% TO KEEPMODEL%

		MO_MODELLINE = KEEPMODEL(LOOP%)
		MO_MODELLINE::MODELCODE = MO_MODEL::MODELCODE
		MO_MODELLINE::MSIZE = MO_MODEL::MSIZE
		MO_MODELLINE::CLASS = MO_MODEL::CLASS

		WHEN ERROR IN
			PUT #MO_MODELLINE.CH%
		USE
			REASON$ = ERT$(ERR)
			CONTINUE CopyFailure
		END WHEN

	NEXT LOOP%

	CALL ENTR_3MESSAGE(SCOPE, "Copied " + NUM1$(KEEPMODEL%) + " lines", 0%)

	GOTO 22890

	!
	! Faied to copy
	!
 CopyFailure:
22800	CALL ENTR_3MESSAGE(SCOPE, "Unable to do copy " + REASON$, 0%)

22890	RETURN

29000	!
	! Trap Errors
	!
	ON ERROR GO BACK

29900	!
	! Handle no header for Line function here
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Sorry, but there is no current header item", 0%)

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:OPTION
	!	^*Manufacturing Order Model Master Options Attachments\*
	!	.b
	!	.lm +5
	!	The ^*Manufacturing Order Model Master Options Attachments\* program
	!	maintains the Options attached to the Model Master.
	!	.lm -5
	!
	! Index:
	!
	!--
