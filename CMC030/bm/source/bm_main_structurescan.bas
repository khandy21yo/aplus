1	%TITLE "Maintain Product Structure Scan"
	%SBTTL "BM_MAIN_STRUCTURESCAN"
	%IDENT "V3.6a Calico"

	FUNCTION LONG BM_MAIN_STRUCTURESCAN(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Record\* program maintains the product structure file in the Bill
	!	of Materials system.
	!	.lm -5
	!
	! Index:
	!	.x Record
	!	.x Maintain Product Structure Scan
	!	.x Scan>Maintain Product Structure
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS BM_SOURCE:BM_MAIN_STRUCTURESCAN/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP BM_MAIN_STRUCTURESCAN
	!	$ DELETE BM_MAIN_STRUCTURESCAN.OBJ;*
	!
	! Author:
	!
	!	10/09/88 - Frank F. Starman
	!
	! Modification history:
	!
	!	05/11/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	06/16/92 - Kevin Handy
	!		Clean up (check)
	!
	!	03/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/22/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/09/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	10/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION)	BM_RELATION_CDD		BM_RELATION
	MAP (BM_RELATION_OLD)	BM_RELATION_CDD		BM_RELATION_OLD, &
							BM_RELATION2, &
							BM_RELATION3

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	!
	! Common Areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_BM_RELATION) &
		BM_RELATION.CH%, &
		BM_RELATION.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG    FUNCTION MAIN_WINDOW
	EXTERNAL LONG    FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	BM_MAIN_STRUCTURESCAN = 0%

	SELECT MOPTION

	!
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!
	CASE OPT_INIT

		!*************************************************************
		! Set up information
		!*************************************************************
		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Product Component Level"
		SMG_WINDOW::CURREC = -2%
		SMG_WINDOW::NHELP = "BM_MAIN_STRUCTURESCAN"
		SMG_WINDOW::HSIZE = 68%
		SMG_WINDOW::VSIZE = 14%
		SMG_WINDOW::HPOS  = 8%
		SMG_WINDOW::VPOS  = 4%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 6%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Product_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 2%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
			SMG_WINDOW::KFIELD(0%, 2%) = 2%
		SMG_WINDOW::KNAME(1%) = "Component"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 3%
			SMG_WINDOW::KFIELD(1%, 2%) = 1%

		SMG_WINDOW::HVIEW = 68%
		SMG_WINDOW::VVIEW = 14%
		SMG_WINDOW::VHPOS = 8%
		SMG_WINDOW::VVPOS = 4%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%
700		!
		! Declare channels
		!
		IF BM_RELATION.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if was
			! that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF BM_RELATION.READONLY%
			GOTO 790
		END IF

		!
		! Open main file (existing) for modification
		!
750		WHEN ERROR IN
			%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			BM_MAIN_STRUCTURESCAN = ERR
			CONTINUE 770
		END WHEN

		BM_RELATION.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open with read
		! access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"
		USE
			BM_MAIN_STRUCTURESCAN = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		BM_RELATION.READONLY% = -1%

		GOTO 790

770		!
		! File not open, so reset channel
		!
		CALL ASSG_FREECHANNEL(BM_RELATION.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = BM_RELATION.CH%

		WHEN ERROR IN
			RESET #BM_RELATION.CH%
			GET #BM_RELATION.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
	! This option is used to display the background information on the
	! screen.  It must first clear any junk on the screen, and then
	! write the background onto it.
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	03,10, "(01) Product #", &
			04,10, "(02) Item #", &
			05,10, "(03) Component #", &
			06,10, "(04) Quantity", &
			07,10, "(05) Operation", &
			08,10, "(06) Scrap (%)", &
			0, 0, ""

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

	!
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display data,
	! set defaults, and return the data back according to MFLAG.
	!
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")
 ReEnter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Product _#\*
	!	.b
	!	.lm +5
	!	The ^*Product _#\* field
	!	enters an assigned number which identifies a
	!	specific product.
	!	.b
	!	The field will accommodate up to fourteen (14) alphanumeric
	!	characters.
	!	.b
	!	Pressing ^*<List Choices>\* while the cursor is located at
	!	this field will cause a list of valid Product _# codes to be
	!	displayed.
	!	.lm -5
	!
	! Index:
	!	.x Product Number>Product Structure Scan
	!	.x Product Structure Scan>Product Number
	!	.x Number>Product
	!
	!--
			BM_RELATION::PRODUCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"3;26",	TEMP$, BM_RELATION::PRODUCT, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX")=1%
				THEN
					BM_RELATION::PRODUCT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reenter
			END IF

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Item _#\*
	!	.b
	!	.lm +5
	!	The ^*Item _#\* field indicates the sequence
	!	number for components which go into a product. When adding
	!	a component relative to a product, the value in the item _#
	!	field will automatically increment. Item _#'s can be changed.
	!	.b
	!	Duplicate item numbers for the same product are not allowed.
	!	.lm -5
	!
	! Index:
	!	.x Item Number>Product Structure Scan
	!	.x Product Structure Scan>Item Number
	!	.x Number>Item
	!
	!--
			IF TEMP$ = "Add"
			THEN
				GOSUB 28500 IF (MFLAG AND 1%) = 0%
			END IF

			ITEMNUM = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "4;26", TEMP$, &
				VAL%(EDIT$(BM_RELATION::ITEMNUM, -1%)), &
				MFLAG, "<0>###", MVALUE)

			BM_RELATION::ITEMNUM = FORMAT$(ITEMNUM, "<0>###")

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Component\*
	!	.b
	!	.lm +5
	!	The ^*Component\* field enters a code which
	!	identifies a particular component in a product.
	!	.b
	!	The component as previously defined in the Product
	!	Description file will automatically appear. If the Component
	!	code is not valid, a message will appear on the screen: ^*Input
	!	undefined, enter anyway <Yes/No>: No\*. The invalid code may be
	!	entered by typing "Y" and pressing ^*<Do>\*
	!	.b
	!	Pressing ^*<List Choices>\*, while the cursor is located at this
	!	field, will cause a list of valid Component _#'s to be displayed.
	!	.lm -5
	!
	! Index:
	!	.x Component>Product Structure Scan
	!	.x Product Structure Scan>Component
	!
	!--

			BM_RELATION::COMPONENT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"05;26", TEMP$, &
				BM_RELATION::COMPONENT, MFLAG, "'E", &
				MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF (MAIN_WINDOW(PD_MAIN_PRODUCT.ID, "VX  ") = 1%)
				THEN
					BM_RELATION::COMPONENT = &
						PD_PRODUCT::PRODUCT_NUM
				END IF
				GOTO Reenter
			END IF

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Quantity\*
	!	.b
	!	.lm +5
	!	The ^*Quantity\* field enters the quantity
	!	of a component used in a particular product. The quantity
	!	is expressed in a Unit of Measure which is defined
	!	in the Product Description file.
	!	.lm -5
	!
	! Index:
	!	.x Quantity>Product Structure Scan
	!	.x Product Structure Scan>Quantity
	!
	!--
			BM_RELATION::QUANTITY = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"06;26", TEMP$, &
				BM_RELATION::QUANTITY, MFLAG, "##,###.###", &
				MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) Operation\*
	!	.b
	!	.lm +5
	!	The ^*Operation\* field enters the designation
	!	for an operation in reference to a particular product where the
	!	component is first used.
	!	.lm -5
	!
	! Index:
	!	.x Operation>Product Structure Scan
	!	.x Product Structure Scan>Operation
	!
	!--
			BM_RELATION::OPERATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"07;26", TEMP$, &
				BM_RELATION::OPERATION, MFLAG, "'E", &
				MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) Scrap (%)\*
	!	.b
	!	.lm +5
	!	The ^*Scrap (%)\* field enters the percentage
	!	of scrap or shrinkage expected of a component.
	!	.lm -5
	!
	! Index:
	!	.x Scrap>Product Structure Scan
	!	.x Product Structure Scan>Scrap
	!
	!--
			BM_RELATION::SCRAP = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"08;26", TEMP$, &
				BM_RELATION::SCRAP * 0.01, MFLAG, "###.##", &
				MVALUE) * 100%


		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY

		BM_MAIN_STRUCTURESCAN = 0%

		SELECT MLOOP

		CASE 1%
			!
			! Is the input defined?
			!
			BM_MAIN_STRUCTURESCAN = FUNC_TESTENTRY( SMG_WINDOW, &
				BM_RELATION::PRODUCT, &
				PD_PRODUCT::DESCRIPTION, &
				"BM", MLOOP, "PRG", &
				"Product", PD_MAIN_PRODUCT.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PD_PRODUCT::DESCRIPTION, 37%), &
				3%, 41%, , SMG$M_BOLD)

		CASE 2%
			IF BM_RELATION::ITEMNUM = ""
			THEN
				BM_MAIN_STRUCTURESCAN = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ BM_RELATION::PRODUCT+ &
							BM_RELATION::ITEMNUM, &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					BM_MAIN_STURCTURESCAN = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 0%)
				END IF
			END IF

		CASE 3%
			!
			! Is the input defined?
			!
			PD_PRODUCT::BOMUOM = &
				STRING$(LEN(PD_PRODUCT::BOMUOM), A"?"B)

			BM_MAIN_STRUCTURESCAN = FUNC_TESTENTRY( SMG_WINDOW, &
				BM_RELATION::COMPONENT, &
				PD_PRODUCT::DESCRIPTION, &
				"BM", MLOOP, "PRG", &
				"Product", PD_MAIN_PRODUCT.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PD_PRODUCT::DESCRIPTION, 37%), &
				5%, 41%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::BOMUOM, 6%, 41%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(1%) AND 2%) = 0%
		THEN
			PD_PRODUCT::DESCRIPTION = &
				STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, &
				"Q0" + BM_RELATION::PRODUCT) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PD_PRODUCT::DESCRIPTION, 37%), &
				3%, 41%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(3%) AND 2%) = 0%
		THEN
			IF MAIN_WINDOW(PD_MAIN_PRODUCT.ID, &
				"Q0" + BM_RELATION::COMPONENT) <> 1%
			THEN
				PD_PRODUCT::DESCRIPTION = &
					STRING$(LEN(PD_PRODUCT::DESCRIPTION), &
					A"?"B)
				PD_PRODUCT::BOMUOM = &
					STRING$(LEN(PD_PRODUCT::BOMUOM), A"?"B)
			END IF


			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(PD_PRODUCT::DESCRIPTION, 37%), &
				5%, 41%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				PD_PRODUCT::BOMUOM, 6%, 41%, , SMG$M_BOLD)
		END IF

	!
	! Set BM_RELATION_OLD value
	!
20500	CASE OPT_SETOLD
		BM_RELATION_OLD = BM_RELATION

	!
	! Restore BM_RELATION_OLD value
	!
	CASE OPT_RESETOLD
		BM_RELATION = BM_RELATION_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		BM_RELATION2 = BM_RELATION

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		BM_RELATION = BM_RELATION2

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Product #      Item Component#        " + &
				"Quantity Operation  Scrap"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "017,022,037,049,059"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = BM_RELATION::PRODUCT + " " + &
				BM_RELATION::ITEMNUM + " " + &
				BM_RELATION::COMPONENT + " " + &
				FORMAT$(BM_RELATION::QUANTITY, &
				"###,###.###") + " " + &
				BM_RELATION::OPERATION + "  " + &
				FORMAT$(0.01 * BM_RELATION::SCRAP, "###.##")

		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE BM_RELATION::PRODUCT + &
				BM_RELATION::ITEMNUM, &
				REGARDLESS

		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE BM_RELATION::COMPONENT + &
				BM_RELATION::PRODUCT, &
				REGARDLESS

		END SELECT

	END SELECT

28000	EXIT FUNCTION

28500	!
	! Find the next item number
	!
	ITEM% = 0%
	WORK_ITEMNUM$ = ""
	WORK_PRODUCT$ = BM_RELATION::PRODUCT
	BM_RELATION3 = BM_RELATION

	WHEN ERROR IN
		FIND  #BM_RELATION.CH%, &
			KEY #0% EQ BM_RELATION::PRODUCT + "", &
			REGARDLESS
	USE
		CONTINUE 28530 IF ERR = 155%
		EXIT HANDLER
	END WHEN

28510	WHILE ITEM% < 9999% - 5%
		ITEM% = ITEM% + 5%

		WHEN ERROR IN
			FIND  #BM_RELATION.CH%, KEY #0% GE &
				WORK_PRODUCT$ + FORMAT$(ITEM%, "<0>###"), &
				REGARDLESS
			GET #BM_RELATION.CH%, REGARDLESS
		USE
			CONTINUE 28515 IF ERR = 155%
			EXIT HANDLER
		END WHEN

		GOTO 28515 IF WORK_PRODUCT$ <> BM_RELATION::PRODUCT
	NEXT

28515	ITEM% = ITEM% - 5%
	FIND #BM_RELATION.CH%, &
		KEY #0% GE WORK_PRODUCT$ + FORMAT$(ITEM%, "<0>###"), &
		REGARDLESS

	GET #BM_RELATION.CH%, REGARDLESS

28520	IF WORK_PRODUCT$ = BM_RELATION::PRODUCT
	THEN
		WORK_ITEMNUM$ = BM_RELATION::ITEMNUM

		WHEN ERROR IN
			GET #BM_RELATION.CH%, REGARDLESS
		USE
			CONTINUE 28530 IF ERR = 11%
			EXIT HANDLER
		END WHEN

		GOTO 28520
	END IF

28530	BM_RELATION = BM_RELATION3
	BM_RELATION::ITEMNUM = FORMAT$(VAL%(WORK_ITEMNUM$) + 1%, "<0>###")

	RETURN

29000	!***************************************************************
	! Trap errors
	!***************************************************************

	ON ERROR GO BACK

32767	END FUNCTION
