1	%TITLE "Read Customer Discount Price File"
	%SBTTL "OE_READ_CUSTDISC"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OE_READ_CUSTDISC (XPRODUCT$, XCUSTOMER$, XCUSTTYPE$, &
		OE_CUSTDISC_CDD OE_CUSTDISC_READ)

	!
	! COPYRIGHT (C) 1990 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho
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
	!	This function returns product price discount for a customer.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Input:
	!
	!	XPRODUCT$ is a product number
	!	XCUSTOMER$   is the customer number
	!
	! Output:
	!
	!	a record from customer discount file
	!
	! Example:
	!
	!	FUNCTION LONG OE_READ_CUSTDISC("product#", "customer#",
	!		OE_CUSTDISC_READ)
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_READ_CUSTDISC/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OE_READ_CUSTDISC
	!	$ DELETE OE_READ_CUSTDISC.OBJ;*
	!
	! Author:
	!
	!	04/19/93 - Frank F. Starman
	!
	! Modification history:
	!
	!	04/21/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/01/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	COM (OE_CUSTDISC_READ) OE_CUSTDISC.CH%

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE PD_PRODUCT_CDD PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[OE.OPEN]OE_CUSTDISC.HB"
	MAP (OE_CUSTDISC)	OE_CUSTDISC_CDD	OE_CUSTDISC

	!
	! External functions
	!
	EXTERNAL LONG FUNCTION PD_EXAM_PRODUCT

	DECLARE LONG EXIT_STATUS

	%PAGE

	!
	! Assume cannot find any promotion prices
	!
	EXIT_STATUS = CMC$_WARNING

1000	IF OE_CUSTDISC.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_CUSTDISC.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "OE_CUSTDISC"
			CONTINUE HelpError
		END WHEN
	END IF

	GOTO ExitFunction &
		IF PD_EXAM_PRODUCT(XPRODUCT$, PD_PRODUCT_EXAM) <> CMC$_NORMAL

2000	!
	! Locate to first promo master by ship date
	!
	WHEN ERROR IN
		FIND #OE_CUSTDISC.CH%, KEY #0% EQ XCUSTOMER$, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 155% OR ERR = 9%
		FILENAME$ = "OE_CUSTDISC"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	!
	! Get next promo master record and check for use
	!
	WHEN ERROR IN
		GET #OE_CUSTDISC.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11% OR ERR = 9%
		FILENAME$ = "OE_CUSTDISC"
		CONTINUE HelpError
	END WHEN

	GOTO ExitFunction IF OE_CUSTDISC::CUSNUM <> XCUSTOMER$

	!
	! Resolve if this is okay to use
	!
	GOTO GetNextRec IF COMP_ARRAY(XPRODUCT$, OE_CUSTDISC::PRODUCT) = 0% &
		AND OE_CUSTDISC::PRODUCT <> ""

	GOTO GetNextRec IF COMP_ARRAY(PD_PRODUCT_EXAM::PROD_TYPE, &
		OE_CUSTDISC::PRODTYPE) = 0% AND OE_CUSTDISC::PRODTYPE <> ""

	GOTO GetNextRec IF COMP_ARRAY(PD_PRODUCT_EXAM::CATEGORY, &
		OE_CUSTDISC::PRODCAT) = 0% AND OE_CUSTDISC::PRODCAT <> ""

	!
	! This is it - matched up so return with normal status and
	! calculate the new sale price
	!
	OE_CUSTDISC_READ = OE_CUSTDISC
	OE_CUSTDISC_READ::PRICETYPE = XCUSTTYPE$ &
		IF OE_CUSTDISC_READ::PRICETYPE = ""

	EXIT_STATUS = CMC$_NORMAL

 ExitFunction:
	OE_READ_CUSTDISC = EXIT_STATUS

	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

	END FUNCTION
