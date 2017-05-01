1	%TITLE "Function to Enter an Inventory Product Number"
	%SBTTL "ENTR_3PRODUCT"
	%IDENT "V3.6a Calico"

	FUNCTION STRING ENTR_3PRODUCT(SCOPE_STRUCT SCOPE, &
		LONG XX_VDID, STRING OP_CPOS, STRING OP_PROMPT, &
		STRING OP_XSTART, LONG OP_FLAG, STRING OP_XFORMAT, &
		STRING OP_DEFLT)

	!
	! COPYRIGHT (C) 1994 BY
	! Computer Management Center, Idaho Falls, Idaho.
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Enters/Displays an Inventory Product Number.
	!	.lm -5
	!
	! Index:
	!
	! Parameter:
	!
	!	XX_VDID
	!		The virtual display ID number.
	!
	!	POS$
	!		The  passed final position of the data on screen.
	!		Format as 'ROW;COL'.  If POS$='' (blank), data will
	!		be entered but not displayed above on screen.
	!
	!	OP_PROMPT
	!		Passed prompt string. (Will be followed by
	!		'ALPHA:', i.e. 'ADD' will generate 'ADD ALPHA:')
	!
	!	OP_FLAG
	!		An integer flag word.
	!		.table
	!			1 - Don't enter data (display only?)
	!			2 - Rset string instead of lset
	!			4 - Force keypunch input(no <CR> after input)
	!			8 - Indicates a timeout on input will occur
	!			16 - Convert from lowercase to uppercase
	!			32 - Use default value
	!			64 - Don't display
	!			128 - Return final value in default
	!		.endtable
	!
	!	FORMAT$
	!		A passed BASIC+2 print-using format for string.
	!		It may also start with ~ to indicate a CMC
	!		format as follows:
	!		.table
	!			"~abx"
	!		where	~	indicates CMC format
	!			a	L or R for Pad to the left or right
	!			b	the pad character
	!			x	the normal BASIC print-using format
	!					as required.
	!		.endtable
	!
	!		Example:  "~LZ'E" means pad on the left side with
	!			the letter Z and use "'E" for the print-using.
	!
	!	DEFAULT$
	!		Returned default data value to use if <CR> is typed.
	!
	!
	!	Returns the string entered.
	!
	!	Returns DEFAULT$ if bit one is set in OP_FLAG.
	!
	! Example:
	!
	!	CN$ = ENTR_3PRODUCT(SMG_SCREEN_DATA%, '3;19',"Customer Number", &
	!		CUSTOM.NUMBER$, OP_FLAG, "'E")
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:ENTR_3PRODUCT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP ENTR_3PRODUCT
	!	$ DELETE ENTR_3PRODUCT.OBJ;*
	!
	! Author:
	!
	!	06/07/94 - Kevin Handy
	!
	! Modification history:
	!
	!	06/28/94 - Kevin Handy
	!		Modifications to make setting default value
	!		work correctly (op_flag = 97).
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/24/97 - Kevin Handy
	!		Lose unecessary externals
	!		Lose line number 1700
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/29/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
	%INCLUDE "FUNC_INCLUDE:PD_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	!
	! Map files
	!
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[PD.OPEN]PD_SUBSTITUTE.HB"
	MAP	(PD_SUBSTITUTE)		PD_SUBSTITUTE_CDD	PD_SUBSTITUTE

	!
	! Common Areas
	!
	COM (CH_PD_PRODUCT) &
		PD_PRODUCT.CH%, &
		PD_PRODUCT.READONLY%

	COM (CH_PD_SUBSTITUTE) &
		PD_SUBSTITUTE.CH%, &
		PD_SUBSTITUTE.READONLY%

	!
	! Init
	!
	ON ERROR GOTO 19000

	!
	! Skip over all the complicated junk if this is just a display
	!
	THIS_PRODUCT$ = SPACE$(30%)
	LSET THIS_PRODUCT$ = OP_XSTART

	IF (OP_FLAG AND 1%) = 1%
	THEN
		GOTO 1900
	END IF

 Reentry:
	!
	! Input the product number
	!
	THIS_PRODUCT$ = ENTR_3STRING(SCOPE, &
		XX_VDID, OP_CPOS, OP_PROMPT, &
		THIS_PRODUCT$, OP_FLAG, "'LLLLLLLLLLLLL", &
		OP_DEFLT)

	%PAGE

	!*******************************************************************
	! Check out this part number
	!*******************************************************************

 CheckProduct:
1500	!
	! Open main file (existing) for modification
	!
	IF PD_PRODUCT.CH% = 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.CRE"
		USE
			FILENAME$ = "PD_PRODUCT"
			CONTINUE HelpError
		END WHEN
		PD_PRODUCT.READONLY% = 0%
	END IF

1510	!
	! See if product exists in main product file
	!
	WHEN ERROR IN
		FIND #PD_PRODUCT.CH%, &
			KEY #0% EQ LEFT(THIS_PRODUCT$, LEN(PD_PRODUCT::PRODUCT_NUM)), &
			REGARDLESS
	USE
		CONTINUE 1600
	END WHEN

	GOTO 1900

1600	!
	! Open substitute part number file
	!
	IF PD_SUBSTITUTE.CH% = 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_SUBSTITUTE.CRE"
		USE
			FILENAME$ = "PD_SUBSTITUTE"
			CONTINUE HelpError
		END WHEN
		PD_SUBSTITUTE.READONLY% = 0%
	END IF

1610	!
	! See if product exists in substitute product file
	!
	WHEN ERROR IN
		GET #PD_SUBSTITUTE.CH%, &
			KEY #1% EQ THIS_PRODUCT$, &
			REGARDLESS
	USE
		CONTINUE 1900
	END WHEN

	THIS_PRODUCT$ = PD_SUBSTITUTE::OUR_PRODUCT

1900	!
	! Return result
	!
	THIS_PRODUCT$ = ENTR_3STRING(SCOPE, &
		XX_VDID, OP_CPOS, OP_PROMPT, &
		LEFT(THIS_PRODUCT$, LEN(PD_PRODUCT::PRODUCT_NUM)), &
		OP_FLAG OR 1%, OP_XFORMAT, &
		OP_DEFLT)

	ENTR_3PRODUCT = LEFT(THIS_PRODUCT$, LEN(PD_PRODUCT::PRODUCT_NUM))

	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO 1900

	%PAGE

	!*******************************************************************
	! Trap errors
	!*******************************************************************

19000	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

	END FUNCTION
