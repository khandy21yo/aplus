1	%TITLE "Examination Product Number"
	%SBTTL "PD_EXAM_PRODUCT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PD_EXAM_PRODUCT(STRING PRODUCT, &
		PD_PRODUCT_CDD PD_PRODUCT_EXAM)

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
	!
	! Abstract:HELP
	!	.p
	!	The ^*Product Master\* option
	!	accesses the routine where records for new or additional
	!	products are entered, including the assignment of product numbers
	!	and the describing of all product identifications.
	!
	! Index:
	!	.x Product Description>Table
	!	.x Tables>Product Description
	!
	! Option:
	!
	!
	! Inputs:
	!
	!	XPRODUCT$ = Product number
	!
	! Outputs:
	!
	!	PD_EXAM_PRODUCT = 0%
	!	PD_EXAM_PRODUCT = 1% Can't find product number
	!
	! Compile:
	!
	!	$ BAS PD_SOURCE:PD_EXAM_PRODUCT
	!	$ LIB FUNC_LIB:CMCFUN/REP PD_EXAM_PRODUCT
	!	$ DELETE PD_EXAM_PRODUCT.OBJ;*
	!
	! Author:
	!
	!	05/02/88 - Frank Starman
	!
	! Modification History:
	!
	!	06/02/93 - Frank F. Starman
	!		Added PD_PRODUCT_ORIG
	!
	!	07/24/93 - Frank F. Starman
	!		Return CMC$_WARNING if subaccount matched.
	!
	!	03/07/94 - Kevin Handy
	!		Modification for more speed.
	!		Don't look in file if we already have the record
	!		loaded.
	!
	!	03/07/94 - Kevin Handy
	!		Modification for more speed.
	!		Don't set EXIT_STATUS in a select, and then
	!		immediately set it to something else.
	!
	!	03/07/94 - Kevin Handy
	!		Added some comments.
	!
	!	03/07/94 - Kevin Handy
	!		Modification for more speed.
	!		Modified where PD_PRODUCT_ORIG was managed, so it
	!		doesn't get used if it doesn't need to be.
	!
	!	03/07/94 - Kevin Handy
	!		Modification for more speed.
	!		Modified location of playing with PD_PRODUCT_INI
	!		so it didn't get set so often.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	10/20/96 - Kevin Handy
	!		Reformat source code
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	01/18/98 - Kevin Handy
	!		Added error trap at 200 for error 145, which
	!		shouldn't really happen but seems to occur
	!		frequently anyway.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Clean up putting back original product information.
	!
	!	11/01/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	01/08/2003 - Kevin Handy
	!		Fill BOMUOM with '?'s too.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP	(PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT
	COM	(PD_PRODUCT_INI)	PD_PRODUCT_CDD	PD_PRODUCT_INI
	DECLARE	PD_PRODUCT_CDD	PD_PRODUCT_ORIG

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP	(SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT
	COM	(SB_SUBACCOUNT_INI) SB_SUBACCOUNT_CDD SB_SUBACCOUNT_INI

	COM (CH_PD_PRODUCT_READ) PD_PRODUCT.CH%
	COM (CH_SB_SUBACCOUNT_READ) SB_SUBACCOUNT.CH%

	DECLARE LONG EXIT_STATUS

	%PAGE

	ON ERROR GOTO 19000

	!
	! Assume undefined
	!
	EXIT_STATUS = CMC$_UNDEFINED

100	!
	! Open product file, and create a default product record,
	! if file is not yet open (and pray that nothing else has opened
	! it and set the channel number).
	!
	IF PD_PRODUCT.CH% <= 0%
	THEN
		PD_PRODUCT_INI::PRODUCT_NUM = &
			STRING$(LEN(PD_PRODUCT_INI::PRODUCT_NUM), A"?"B)
		PD_PRODUCT_INI::DESCRIPTION = &
			STRING$(LEN(PD_PRODUCT_INI::DESCRIPTION), A"?"B)
		PD_PRODUCT_INI::PROD_TYPE = &
			STRING$(LEN(PD_PRODUCT_INI::PROD_TYPE), A"?"B)
		PD_PRODUCT_INI::CATEGORY = &
			STRING$(LEN(PD_PRODUCT_INI::CATEGORY), A"?"B)
		PD_PRODUCT_INI::UOM = &
			STRING$(LEN(PD_PRODUCT_INI::UOM), A"?"B)
		PD_PRODUCT_INI::BOMUOM = &
			STRING$(LEN(PD_PRODUCT_INI::BOMUOM), A"?"B)
		PD_PRODUCT_INI::PACK = &
			STRING$(LEN(PD_PRODUCT_INI::PACK), A"?"B)
		PD_PRODUCT_INI::LABEL = &
			STRING$(LEN(PD_PRODUCT_INI::LABEL), A"?"B)
		PD_PRODUCT_INI::METHOD = &
			STRING$(LEN(PD_PRODUCT_INI::METHOD), A"?"B)
		PD_PRODUCT_INI::BDATE = &
			STRING$(LEN(PD_PRODUCT_INI::BDATE), A"?"B)
		PD_PRODUCT_INI::SSTATUS = &
			STRING$(LEN(PD_PRODUCT_INI::SSTATUS), A"?"B)
		PD_PRODUCT_INI::EDATE = &
			STRING$(LEN(PD_PRODUCT_INI::EDATE), A"?"B)
		PD_PRODUCT_INI::SECONDARY_CODE = &
			STRING$(LEN(PD_PRODUCT_INI::SECONDARY_CODE), A"?"B)

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "PD_PRODUCT"
			CONTINUE HelpError
		END WHEN
	END IF

110	!
	! Open subaccount file, and create a default record,
	! if file is not yet open (and pray that nothing else has opened
	! it and set the channel number).
	!
	IF SB_SUBACCOUNT.CH% <= 0%
	THEN
		SB_SUBACCOUNT_INI::SUBJECT = &
			STRING$(LEN(SB_SUBACCOUNT_INI::SUBJECT), A"?"B)
		SB_SUBACCOUNT_INI::SUBACCOUNT = &
			STRING$(LEN(SB_SUBACCOUNT_INI::SUBACCOUNT), A"?"B)
		SB_SUBACCOUNT_INI::DESCR = &
			STRING$(LEN(SB_SUBACCOUNT_INI::DESCR), A"?"B)
		SB_SUBACCOUNT_INI::TTYPE = &
			STRING$(LEN(SB_SUBACCOUNT_INI::TTYPE), A"?"B)
		SB_SUBACCOUNT_INI::CLASS = &
			STRING$(LEN(SB_SUBACCOUNT_INI::CLASS), A"?"B)
		SB_SUBACCOUNT_INI::BDATE = &
			STRING$(LEN(SB_SUBACCOUNT_INI::BDATE), A"?"B)
		SB_SUBACCOUNT_INI::SSTATUS = &
			STRING$(LEN(SB_SUBACCOUNT_INI::SSTATUS), A"?"B)
		SB_SUBACCOUNT_INI::EDATE = &
			STRING$(LEN(SB_SUBACCOUNT_INI::EDATE), A"?"B)

		WHEN ERROR IN
			%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
		USE
			CONTINUE 200 IF ERR = 5%
			FILENAME$ = "SB_SUBACCOUNT"
			CONTINUE HelpError
		END WHEN
	END IF

200	!
	! If we don't already have the product record
	!
	IF PD_PRODUCT_EXAM::PRODUCT_NUM <> PRODUCT
	THEN
		!
		! Get the product
		!
		PD_PRODUCT_ORIG = PD_PRODUCT

 !		PD_PRODUCT_EXAM::PRODUCT_NUM = PRODUCT
		WHEN ERROR IN
			GET #PD_PRODUCT.CH%, KEY #0% EQ PRODUCT, REGARDLESS
		USE
			PD_PRODUCT = PD_PRODUCT_ORIG
			PD_PRODUCT_EXAM = PD_PRODUCT_INI
			CONTINUE 210 IF (ERR = 155%) OR (ERR = 9%) OR &
				(ERR = 145%)
			FILENAME$ = "PD_PRODUCT"
			CONTINUE HelpError
		END WHEN

		PD_PRODUCT_EXAM = PD_PRODUCT

		PD_PRODUCT = PD_PRODUCT_ORIG
	END IF

	!
	! This code is canceled out by the line that follows it (which
	! was already here), so I am commenting it out.
	!
	! SELECT PD_PRODUCT_EXAM::SSTATUS
	!
	!	CASE "O"
	!		EXIT_STATUS = CMC$_TERMINATED
	!	CASE ELSE
	!		EXIT_STATUS = CMC$_NORMAL
	!
	! END SELECT

	EXIT_STATUS = CMC$_NORMAL
	GOTO ExitFunction

210	!
	! We only make it here if we cannot find the product in the master
	! file. We then try to find it in the subaccount as an 'E' type.
	!
	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, KEY #0% EQ &
			"E" + LEFT(PRODUCT, LEN(SB_SUBACCOUNT::SUBACCOUNT)), &
			REGARDLESS
	USE
		CONTINUE ExitFunction IF (ERR = 155%) OR (ERR = 9%)
		FILENAME$ = "SB_SUBACCOUNT"
		CONTINUE HelpError
	END WHEN

	!
	! Fake up a description
	!
	PD_PRODUCT_EXAM::DESCRIPTION = SB_SUBACCOUNT::DESCR
	PD_PRODUCT_EXAM::PROD_TYPE   = SB_SUBACCOUNT::TTYPE
	PD_PRODUCT_EXAM::CATEGORY    = SB_SUBACCOUNT::CLASS

	EXIT_STATUS = CMC$_WARNING

 ExitFunction:
	PD_EXAM_PRODUCT = EXIT_STATUS

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

	%PAGE

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END FUNCTION
