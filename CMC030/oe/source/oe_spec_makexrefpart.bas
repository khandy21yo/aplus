1	%TITLE "Product Description List"
	%SBTTL "OE_SPEC_MAKEXREFPART"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2004 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:PD005
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Product Description\* option
	!	accesses the print routine which will print a list of
	!	product descriptions.
	!	.b
	!	The following fields are included:
	!	.table 3,25
	!	.te
	!	Product
	!	.te
	!	Product Description
	!	.te
	!	Product Type
	!	.te
	!	Product Category
	!	.te
	!	Unit of Measure
	!	.te
	!	Label Code
	!	.te
	!	Costing Method
	!	.te
	!	Onset Date
	!	.te
	!	End Date
	!	.te
	!	Secondary Code
	!	.te
	!	Weight
	!	.te
	!	Manufacturer UOM
	!	.te
	!	Product Factor
	!	.end table
	!	The list may be printed in product code or type code order.
	!
	! Index:
	!	.x Reports>Product Description List
	!	.x Product Description>List
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_SPEC_MAKEXREFPART/LINE
	!	$ LINK/EXE=OE_EXE: OE_SPEC_MAKEXREFPART, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_SPEC_MAKEXREFPART.OBJ;*
	!
	! Author:
	!
	!	05/24/2004 - Kevin Handy
	!
	! Modification History:
	!
	!--

	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[PD.OPEN]PD_SUBSTITUTE.HB"
	MAP (PD_SUBSTITUTE) PD_SUBSTITUTE_CDD PD_SUBSTITUTE

	%PAGE

	ON ERROR GOTO 19000

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_SUBSTITUTE.MOD"
	USE
		FILENAME$ = "PD_SUBSTITUTE"
		CONTINUE HelpError
	END WHEN


 ReportTitle:

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #PD_PRODUCT.CH%
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PD_SUBSTITUTE"
		CONTINUE HelpError
	END WHEN

	PRINT ".";
	PRINT IF CCPOS(0%) >= 50%

	!
	! Skip numeric starting codes
	!
 !	GOTO GetNextRec &
 !		IF INSTR(1%, "0123456789", LEFT(PD_PRODUCT::PRODUCT_NUM, 1%))

	!
	! Must start with a letter
	!
	GOTO GetNextRec &
		IF INSTR(1%, "ABCDEFGHIJKLMNOPQRSTUVWXYZ", &
		LEFT(PD_PRODUCT::PRODUCT_NUM, 1%)) = 0%

	!
	! Look for start of numeric part
	!
	FOR LOOP% = 1% TO 14%
		CH$ = MID$(PD_PRODUCT::PRODUCT_NUM, LOOP%, 1%)

		!
		! Did we find a numeric digit
		!
		GOTO AddThis IF INSTR(1%, "0123456789", CH$)

		!
		! Skip if it isn't a alpha string
		!
		GOTO GetNextRec &
			IF INSTR(1%, "ABCDEFGHIJKLMNOPQRSTUVWXYZ", &
			CH$) = 0%
	NEXT LOOP%

	!
	! Must be fully alpha for 14 characters
	!
	GOTO GetNextRec

 AddThis:
	!
	! Ok, we now have a valid product, see if we can make a
	! entry in the table for it.
	!
	PD_SUBSTITUTE::OUR_PRODUCT = PD_PRODUCT::PRODUCT_NUM
	PD_SUBSTITUTE::THEIR_PRODUCT = "X" + &
		RIGHT(PD_PRODUCT::PRODUCT_NUM, LOOP%)
	PD_SUBSTITUTE::VENDOR = "XREFXREF"

	WHEN ERROR IN
		PUT #PD_SUBSTITUTE.CH%
	USE
		PRINT "?";
		PRINT IF CCPOS(0%) >= 50%

		CONTINUE GetNextRec IF ERR = 134%
		FILENAME$ = "PD_SUBSTITUTE"
		CONTINUE HelpError
	END WHEN

	PRINT "#";
	PRINT IF CCPOS(0%) >= 50%

	GOTO GetNextRec

	%PAGE

 ExitProgram:
	!
	! Exit to next program or menu
	!
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
 !	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
 !		"E", ERN$, FILENAME$, NUM1$(ERR))

	PRINT "Error: "; ERl; " "; ERR; " "; ERT$(ERR)

	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	RESUME HelpError

32767	END
