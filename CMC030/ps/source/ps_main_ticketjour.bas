1	%TITLE "Order-Invoice Entry Journal"
	%SBTTL "PS_MAIN_TICKETJOUR"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PS_MAIN_TICKETJOUR(CDD_WINDOW_CDD SMG_WINDOW, LONG MOPTION, &
		LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1990 BY
	!
	! Computer Management Center
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
	!	The Order-Invoice Entry Journal transactions are entered and
	!	maintained through the ^*Order-Invoice Entry Journal\* option.
	!	.lm -5
	!
	! Index:
	!	.x Maintain>Ticket Entry Journal
	!	.x Batch Number>User
	!	.x User Batch Number
	!	.x Journal>Entry Maintain
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PS_SOURCE:PS_MAIN_TICKETJOUR/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PS_MAIN_TICKETJOUR
	!	$ DELETE PS_MAIN_TICKETJOUR.OBJ;*
	!
	! Author:
	!	10/30/91 - Frank F. Starman
	!
	! Modification history:
	!
	!	02/05/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	02/28/92 - Kevin Handy
	!		Changed "CMC$NORMAL" to "CMC$_NORMAL".
	!
	!	03/12/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	03/20/92 - Dan Perkins
	!		Display totals after Fld-21 when adding.
	!
	!	04/15/92 - Frank F. Starman
	!		Change MISCACCT to CREASON field.
	!
	!	04/21/92 - Frank F. Starman
	!		Added format for ENTR_3STRING.
	!
	!	04/28/92 - Kevin handy
	!		Clean up (check)
	!
	!	05/15/92 - Frank F. Starman
	!		Display credit limit on the screen.
	!
	!	06/01/92 - Frank F. Starman
	!		Check number of payments. Do not allow zero.
	!
	!	06/03/92 - Frank F. Starman
	!		Check operator entry.
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	02/02/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/18/93 - Dan Perkins
	!		Fixed OPT_VIEW and view by titles.
	!
	!	04/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/07/93 - Frank F. Starman
	!		Balnk REG_FLAG everytime in the option OPT_RESETDEFAULT
	!
	!	04/29/94 - Kevin Handy
	!		Modified check number field (25) so that it doesn't
	!		disable itself when receiving back orders. Field (24)
	!		amount received was already fixed this way.
	!
	!	06/17/94 - Kevin Handy
	!		Modifications to sales tax calculation to leave
	!		out non-taxable portion of OE_READ_ORDERTOTAL.
	!
	!	07/05/94 - Kevin Handy
	!		Added error trap for record lock errors.
	!		Wait a few seconds and try again.
	!
	!	01/06/94 - Kevin Handy
	!		Modified so that pressing F17 on item 03 will
	!		pull up the customer maintenance instead of
	!		some odd query screen.
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards.
	!		Change UTL_MAIN_CARRIER.ID to UT_MAIN_CARRIER.ID
	!
	!	05/26/95 - Kevin Handy
	!		Modified to use customer status of "C" to leave off
	!		aging, instead of checking the customer category
	!		off of a list of exempted categories.
	!
	!	06/15/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/14/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	01/26/96 - Kevin Handy
	!		Remove lots of commented out code.
	!		Set several fields to "" instead of
	!		a STRING$(...) which did the same thing.
	!
	!	01/29/96 - Kevin Handy
	!		Change STRING$(...,ASCII(" ")) to SPACE$(...) in
	!		several places.
	!
	!	10/25/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/13/98 - Kevin Handy
	!		Add TAXFLAG parameter to OE_READ_SALESTAX
	!
	!	10/16/98 - Kevin Handy
	!		Fix salestax bug (ar instead of oe flag used)
	!
	!	12/16/98 - Kevin Handy
	!		Make sure new deposit number field is blanked out
	!		Added entry field for deposit number.
	!
	!	12/17/98 - Kevin Handy
	!		Modified so that a bad misc account will not
	!		lock you into the reason code field.
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	05/26/99 - Kevin Handy
	!		Fix bug displaying field 30 on second page properly.
	!		(LPAGE(1) not set properly)
	!
	!	11/03/2000 - Kevin Handy
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

	%INCLUDE "FUNC_INCLUDE:OE_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:AR_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"
	%INCLUDE "FUNC_INCLUDE:SA_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:SB_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:PS_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:GL_WINDOW.INC"

	!
	! Include CDD's
	!
	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR
	MAP (OE_ORDERJOUR_OLD)	OE_ORDERJOUR_CDD	OE_ORDERJOUR_OLD, &
							OE_ORDERJOUR_DEF

	%INCLUDE "SOURCE:[OE.OPEN]OE_SALESTAX.HB"
	MAP (OE_SALESTAX)	OE_SALESTAX_CDD		OE_SALESTAX
	DECLARE			OE_SALESTAX_CDD		OE_SALESTAX_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPTO.HB"
	MAP (OE_SHIPTO)		OE_SHIPTO_CDD		OE_SHIPTO

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREASON.HB"
	MAP (OE_CREASON)	OE_CREASON_CDD		OE_CREASON

	%INCLUDE "SOURCE:[OE.OPEN]OE_REASONACCT.HB"
	MAP (OE_REASONACCT)	OE_REASONACCT_CDD	OE_REASONACCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.HB"
	MAP (UTL_CARRIER)	UTL_CARRIER_CDD		UTL_CARRIER

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS)		UTL_TERMS_CDD		UTL_TERMS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP (UTL_COUNTRY)	UTL_COUNTRY_CDD		UTL_COUNTRY

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_STATE.HB"
	MAP (UTL_STATE)		UTL_STATE_CDD		UTL_STATE

	%INCLUDE "SOURCE:[PS.OPEN]PS_CONTROL.HB"
	MAP (PS_CONTROL)	PS_CONTROL_CDD		PS_CONTROL

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERTYPE.HB"
	MAP (OE_ORDERTYPE)	OE_ORDERTYPE_CDD	OE_ORDERTYPE

	%INCLUDE "SOURCE:[OE.OPEN]OE_CATEGORY.HB"
	MAP (OE_CATEGORY)	OE_CATEGORY_CDD		OE_CATEGORY

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER)	OE_REGHEADER_CDD	OE_REGHEADER
	DECLARE			OE_REGHEADER_CDD	OE_REGHEADER_READ

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN
	DECLARE			SA_SALESMAN_CDD		SA_SALESMAN_EXAM

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT
	DECLARE			SB_SUBACCOUNT_CDD	SB_SUBACCOUNT_EXAM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP (AR_CUSBAL)		AR_CUSBAL_CDD		AR_CUSBAL
	DIM			AR_CUSBAL_CDD		ARRAY_CUSBAL(50%)

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD		GL_CHART

	!
	! This common area must be mapped in both the main program and
	! in OE_MAIN_ORDERLINE.
	!
	COM (CH_OE_ORDERJOUR) &
		OE_ORDERJOUR.CH%

	COM (BATCH_NO) &
		BATCH_NO$ = 7%

	COM (CH_PS_CONTROL) &
		PS_CONTROL.CH%

	COM (CH_GL_CHART) &
		GL_CHART.CH%

	COM (TT_OE_TAXFLAG) &
		TAXFLAG_T$ = 40%, &
		TAXFLAG$(5%) = 40%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION OE_READ_REGHEADER
	EXTERNAL LONG	FUNCTION SA_EXAM_SALESMAN
	EXTERNAL LONG	FUNCTION OE_READ_ORDERTOTAL
	EXTERNAL LONG	FUNCTION AR_FUNC_AGE
	EXTERNAL LONG	FUNCTION OE_READ_SALESTAX
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	!
	! Declare data
	!
	DECLARE LONG XPOS, YPOS

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!******************************************************************
	! Initialization
	!
	! This option is used to initialize the window structure,
	! set up the default values for add, and open all files
	! necessary that have not already been opened.
	!******************************************************************
	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR  = "Order-Invoice Entry Journal " + BATCH_NO$
		SMG_WINDOW::NHELP  = "PS_MAIN_TICKETJOUR"
		SMG_WINDOW::CHAN   = OE_ORDERJOUR.CH%
		SMG_WINDOW::HSIZE  = 77%
		SMG_WINDOW::VSIZE  = 18%
		SMG_WINDOW::HVIEW  = 78%
		SMG_WINDOW::VVIEW  = 18%
		SMG_WINDOW::HPOS   = 2%
		SMG_WINDOW::VPOS   = 2%
		SMG_WINDOW::NITEMS = 30%

		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::LWIDTH  = 78%
		SMG_WINDOW::LHEIGHT = 15%
		SMG_WINDOW::LHPOS   = 2%
		SMG_WINDOW::LVPOS   = 5%
		SMG_WINDOW::LLAST   = 1%
		SMG_WINDOW::LTITLE(0%) = "First Page"
		SMG_WINDOW::LPAGE(0%)  = 9%
		SMG_WINDOW::LTITLE(1%) = "Last Page"
		SMG_WINDOW::LPAGE(1%)  = 30%

		SMG_WINDOW::NKEYS = 4%
		SMG_WINDOW::KNAME(0%) = "Document_number"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "sales_Type"
			SMG_WINDOW::KFIELD(1%, 0%) = 1%
			SMG_WINDOW::KFIELD(1%, 1%) = 21%
		SMG_WINDOW::KNAME(2%) = "customer_Number"
			SMG_WINDOW::KFIELD(2%, 0%) = 2%
			SMG_WINDOW::KFIELD(2%, 1%) = 5%
			SMG_WINDOW::KFIELD(2%, 2%) = 1%
		SMG_WINDOW::KNAME(3%) = "sales_Category"
			SMG_WINDOW::KFIELD(3%, 0%) = 2%
			SMG_WINDOW::KFIELD(3%, 1%) = 4%
			SMG_WINDOW::KFIELD(3%, 2%) = 1%

		!
		! TAX FLAGS
		!
		TAXFLAG_T$ = "Flag Description                   "
		TAXFLAG$(0%) = "5"
		TAXFLAG$(1%) = "1    Taxable                       "
		TAXFLAG$(2%) = "2    Service                       "
		TAXFLAG$(3%) = "4    Resale                        "
		TAXFLAG$(4%) = "5    Out of State                  "
		TAXFLAG$(5%) = "6    Church, School, and Government"

		COM (PS_MAIN_TICKETJOUR_FRM) FRM$(28%)

		CALL READ_DEFAULTS(SMG_WINDOW)

20010		IF GL_CHART.CH% <= 0%
		THEN
			!
			! Just read only
			!
			%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.OPN"
		END IF

		GOTO 20040 IF OE_ORDERJOUR.CH% > 0%

		!
		! Open OE_ORDERJOUR
		!
		%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.CRE"

20040		SMG_WINDOW::CHAN = OE_ORDERJOUR.CH%
		WHEN ERROR IN
			RESET	#OE_ORDERJOUR.CH%
			GET	#OE_ORDERJOUR.CH%, REGARDLESS
		USE
			CONTINUE 32767 IF ERR = 11%
			EXIT HANDLER
		END WHEN


	!******************************************************************
	! Display the background
	!
	! This option is used to display the background information
	! on the screen.  It must first clear any junk on the screen,
	! and then write the background onto it.
	!******************************************************************
	CASE OPT_BACKGROUND

		SELECT MLOOP
		!
		! Main screen
		!
		CASE 0%

			SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

			SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

			DATA	1,  1, "(01) Document #", &
				2,  1, "(02) Order Date", &
				3,  1, "(03) Written By", &
				4,  1, "(04) Sales Cat.", &
				5,  1, "(05) Customer #", &
				9,  1, "(06) Ship to ", &
				15, 1, "(07) Cust Po#", &
				16, 1, "(08) Location", &
				17, 1, "(09) Ship Date", &
				13, 1, "     City/St. ", &
				14, 1, "     ZIP/Cntry ", &
				0,  0, ""

			RESTORE

			READ XPOS, YPOS, XSTR$
			I% = 0%

			WHILE (XPOS <> 0%)
				I% = I% + 1%

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
					XSTR$, XPOS, YPOS) &
					IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%

				READ XPOS, YPOS, XSTR$
			NEXT

			SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)


		!
		! 2nd page
		!
		CASE 1%
			SMG_STATUS% = &
				SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::LWINDOW)

			SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::LWINDOW)

			DATA	1,  1, "(10) Ship VIA", &
				2,  1, "(11) Terms", &
				3,  1, "(12) Freight", &
				4,  1, "(13) Handling", &
				5,  1, "(14) Sale Disc %", &
				6,  1, "(15) Misc. Chrg", &
				7,  1, "(16) Reason Code", &
				8,  1, "(17) Misc Acct #", &
				9,  1, "(18) Tax Code", &
				10, 1, "(19) Tax Flag", &
				11, 1, "(20) Sales Tax %", &
				12, 1, "(21) Sales Type", &
				1, 52, "(22) Salesman", &
				2, 52, "(23) Comm %", &
				3, 52, "(24) Paid Amt", &
				4, 52, "(25) Check #", &
				5, 52, "(26) Date", &
				6, 52, "(27) Inv# ", &
				7, 52, "(28) Payments", &
				13, 1, "(29) Notes", &
				8, 52, "(30) Deposit#", &
				0,  0, ""

			RESTORE
			XPOS = -1%
			READ XPOS, YPOS, XSTR$ UNTIL XPOS = 0%
			READ XPOS, YPOS, XSTR$

			I% = SMG_WINDOW::LPAGE(0%)

			WHILE (XPOS <> 0%)

				I% = I% + 1%

				SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
					XSTR$, XPOS, YPOS) &
					IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%

				READ XPOS, YPOS, XSTR$
			NEXT

			SMG_STATUS% = &
				SMG$END_DISPLAY_UPDATE(SMG_WINDOW::LWINDOW)

		END SELECT

20200	!******************************************************************
	! Enter/Display/Default
	!
	! This option is used to enter the data from the user, display
	! data, set defaults, and return the data back according to
	! MFLAG.
	!******************************************************************
	CASE OPT_ENTRY
		TEMP$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View "

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%
	!++
	! Abstract:FLD001
	!	.x Document Number
	!	^*(01) Document Number\*
	!	.b
	!	.lm +5
	!	The ^*Document Number\* field enters the
	!	document number for a specific Order-Invoice entry.  The system
	!	will automatically increment the document number to be the next
	!	higher number after the last document entered. The user may
	!	override the system assigned number by entering a different number, or accept
	!	the system assigned number by pressing ^*Return\*. If the user overrides a
	!	system assigned number, that number will be assigned
	!	to the next Order-Invoice added.
	!	.b
	!	An entry is required in this field.  The field will accommodate ten (10)
	!	characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_ORDERJOUR::ORDNUM = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"1;17", TEMP$, OE_ORDERJOUR::ORDNUM, &
				MFLAG OR 2%, "~R 'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(OE_MAIN_REGHEADER.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::ORDNUM = &
						OE_REGHEADER::ORDNUM
				END IF
				GOTO Reenter
			END IF

		CASE 2%
	!++
	! Abstract:FLD002
	!	.x Sales Date
	!	^*(02) Sales Date\*
	!	.b
	!	.lm +5
	!	The ^*Sales Date\* field enters the date a
	!	particular Order-Invoice was created.
	!	.b
	!	The field will automatically default to the system date.
	!	The system date may be accepted by pressing ^*Return\* or the
	!	date may be overridden by entering the correct date and pressing
	!	^*Return\*.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::ORDDATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"2;17", TEMP$, OE_ORDERJOUR::ORDDATE, &
				MFLAG, "'E", MVALUE)

		CASE 3%
	!++
	! Abstract:FLD003
	!	.x Written By
	!	^*(03) Written By\*
	!	.b
	!	.lm +5
	!	The ^*Written By\* field enters the name or initials of the
	!	operator who enters the Order-Invoice.
	!	.b
	!	An entry is required in this field.  The field will accommodate ten (10)
	!	characters.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_ORDERJOUR::OPERATOR = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"3;17", TEMP$, OE_ORDERJOUR::OPERATOR, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(SA_MAIN_SALESMAN.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::OPERATOR = &
						SA_SALESMAN::SALESMAN
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(SA_MAIN_SALESMAN.ID, "M")
				OE_ORDERJOUR::OPERATOR = SA_SALESMAN::SALESMAN
				GOTO ReEnter


			END SELECT

		CASE 4%
	!++
	! Abstract:FLD004
	!	.x Sales Category
	!	^*(04) Sales Category\*
	!	.b
	!	.lm +5
	!	The ^*Sales Category\* field enters the user defined
	!	category relative to a specific entry.
	!	.b
	!	Valid sales category codes may be viewed by pressing ^*List Choices\*.
	!	Additional sales category codes may be added by pressing ^*F17\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::ORDCAT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"4;17", TEMP$, OE_ORDERJOUR::ORDCAT, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(OE_MAIN_CATEGORY.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::ORDCAT = &
						OE_CATEGORY::ORDCAT
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(OE_MAIN_CATEGORY.ID, "M")
				OE_ORDERJOUR::ORDCAT = OE_CATEGORY::ORDCAT
				GOTO ReEnter

			END SELECT

		CASE 5%
	!++
	! Abstract:FLD005
	!	.x Customer<Number
	!	^*(05) Customer\*
	!	.b
	!	.lm +5
	!	The ^*Customer\* field enters a number which
	!	references a particular customer.
	!	.b
	!	Valid customer numbers and the related customer name may be viewed
	!	by pressing ^*List Choices\* or additional customers may be added by pressing
	!	^*F17\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::CUSNUM = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"5;17", TEMP$, OE_ORDERJOUR::CUSNUM, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::CUSNUM = &
						AR_35CUSTOM::CUSNUM
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17
				V% = MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, "M")
				OE_ORDERJOUR::CUSNUM = AR_35CUSTOM::CUSNUM
				GOTO ReEnter

			END SELECT

		CASE 6%
	!++
	! Abstract:FLD006
	!	.x Ship to
	!	^*(06) Ship to\*
	!	.b
	!	.lm +5
	!	The ^*Ship to\* field
	!	enters the name and address to which an
	!	Order-Invoice is to be shipped. If the name and address to which
	!	the Order-Invoice is to be shipped is the customer name and
	!	address, the customer's name and address will be duplicated by
	!	pressing ^*Return\*. Any customer may have one or more "ship to"
	!	records defined in the customer master file. By entering the number
	!	of a valid "ship to" name and address, the related name and address
	!	will be displayed in the record.
	!	.b
	!	Valid ship to codes may be viewed by pressing ^*List Choices\* or
	!	additional ship to codes may be added by pressing ^*F17\*.
	!	.b
	!	If an invalid number is entered, the "ship to" name and address information
	!	will be blank.
	!	.lm -5
	!
	! Index:
	!
	!--
 Fld006Line:
			SCOPE::PRG_ITEM = "FLD006"

			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::SHIPLIN = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;17", TEMP$, OE_ORDERJOUR::SHIPLIN, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14
				IF OE_ORDERJOUR::CUSNUM <> ""
				THEN
					IF MAIN_WINDOW(OE_MAIN_SHIPTO.ID, &
						"VX" + OE_ORDERJOUR::CUSNUM) = 1%
					THEN
						OE_ORDERJOUR::SHIPLIN = &
							OE_SHIPTO::LINES
					END IF
				ELSE
					IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
						"VX") = 1%
					THEN
						OE_ORDERJOUR::SHIPLIN = &
							UTL_LOCATION::LOCATION
					END IF
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17
				IF OE_ORDERJOUR::CUSNUM <> ""
				THEN
					IF MAIN_WINDOW(OE_MAIN_SHIPTO.ID, &
						"M0" + OE_ORDERJOUR::CUSNUM) = 1%
					THEN
						OE_ORDERJOUR::SHIPLIN = &
							OE_SHIPTO::LINES
					END IF
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_UP
				GOTO ExitFunction

			END SELECT

	!
	! Skip the rest of 06 if an address has been found
	!
		IF (MVALUE ="ADD" OR MVALUE = "CHANGE")
		THEN
			GOSUB LoadShip IF OE_ORDERJOUR::REG_FLAG <> "Y"
			MFLAG = MFLAG OR 1% IF V% = 1%
		END IF

	!++
	! Abstract:FLD006A
	!	^*Ship Name\*
	!	.b
	!	.lm +5
	!	The ^*Ship Name\* field enters
	!	the address for shipping the Order-Invoice.
	!	.lm -5
	!
	! Index:
	!	.x Ship Name
	!
	!--
 Fld006Name:
			SCOPE::PRG_ITEM = "FLD006A"

			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::SHIPNAM = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"9;28", TEMP$, OE_ORDERJOUR::SHIPNAM, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld006Line

			END SELECT

	!++
	! Abstract:FLD006B
	!	.x Ship to Address 1
	!	^*Ship to Address 1\*
	!	.lm +5
	!	.b
	!	The ^*Ship to Address 1\* field enters
	!	the first line of the address for shipping the Order-Invoice.
	!	.lm -5
	!
	! Index:
	!
	!--
 Fld006Add1:
			SCOPE::PRG_ITEM = "FLD006B"

			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::ADD1 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"10;28", TEMP$, OE_ORDERJOUR::ADD1, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld006Name

			END SELECT


	!++
	! Abstract:FLD006C
	!	.x Ship to Address 2
	!	^* Ship to Address 2\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 2\* field enters
	!	the second line of the address for shipping the Order-Invoice.
	!	.lm -5
	!
	! Index:
	!
	!--
 Fld006Add2:
			SCOPE::PRG_ITEM = "FLD006C"

			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::ADD2 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"11;28", TEMP$, OE_ORDERJOUR::ADD2, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld006Add1

			END SELECT

	!++
	! Abstract:FLD006D
	!	.x Ship to Address 3
	!	^* Ship to Address 3\*
	!	.b
	!	.lm +5
	!	The ^*Ship to Address 3\* field enters
	!	the third line of the address for shipping the Order-Invoice.
	!	.lm -5
	!
	! Index:
	!
	!--
 Fld006Add3:
			SCOPE::PRG_ITEM = "FLD006D"

			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::ADD3 = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"12;28", TEMP$, OE_ORDERJOUR::ADD3, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld006Add2

			END SELECT

	!++
	! Abstract:FLD006E
	!	.x Customer>City
	!	^*City\*
	!	.b
	!	.lm +5
	!	The ^*City\* field enters the city in which the
	!	customer is located.
	!	.lm -5
	!
	! Index:
	!	.x City>Customer
	!
	!--
 Fld006City:
			SCOPE::PRG_ITEM = "FLD006E"

			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::CITY = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;28", TEMP$, OE_ORDERJOUR::CITY, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld006Add3

			END SELECT

	!++
	! Abstract:FLD006F
	!	.x Customer>State
	!	^*State\*
	!	.b
	!	.lm +5
	!	The ^*State\* field enters the state in which
	!	the customer is located.
	!	.b
	!	The field will accommodate a two (2) character state postal code.
	!	.lm -5
	!
	! Index:
	!	.x State>Customer
	!
	!--
 Fld006State:
			SCOPE::PRG_ITEM = "FLD006F"

			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::STATE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"13;44", TEMP$, OE_ORDERJOUR::STATE, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld006City

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(UTL_MAIN_STATE.ID, &
					"VX" + OE_ORDERJOUR::COUNTRY) = 1%
				THEN
					OE_ORDERJOUR::STATE = UTL_STATE::STATE
				END IF
				GOTO  ReEnter

			END SELECT

	!++
	! Abstract:FLD006G
	!	.x Customer>Zip
	!	^*Zip\*
	!	.b
	!	.lm +5
	!	The ^*Zip\* field enters the zip or postal
	!	code for the area in which a customer is located.
	!	.b
	!	The field will accommodate ten (10) characters.
	!	.lm -5
	!
	! Index:
	!	.x Zip>Customer
	!
	!--
 Fld006Zip:
			SCOPE::PRG_ITEM = "FLD006G"

			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::ZIP = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;28", TEMP$, OE_ORDERJOUR::ZIP, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld006State

			END SELECT

	!++
	! Abstract:FLD006H
	!	.x Customer>Country
	!	^*Country\*
	!	.b
	!	.lm +5
	!	The ^*Country\* field enters the country if
	!	a customer is located in a foreign country.
	!	.b
	!	Valid country codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Country>Customer
	!
	!--

 Fld006Country:
			SCOPE::PRG_ITEM = "FLD006H"

			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::COUNTRY = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"14;39", TEMP$, OE_ORDERJOUR::COUNTRY, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO Fld006Zip

			CASE SMG$K_TRM_F14
				IF MAIN_WINDOW(UTL_MAIN_COUNTRY.ID, "VX" + &
					OE_ORDERJOUR::COUNTRY) = 1%
				THEN
					OE_ORDERJOUR::COUNTRY = &
						UTL_COUNTRY::COUNTRY
				END IF
				GOTO ReEnter

			END SELECT


		CASE 7%
	!++
	! Abstract:FLD007
	!	.x Customer>Purchase Order
	!	^*(07) Customer PO\*
	!	.b
	!	.lm +5
	!	The ^*Customer PO\* field enters a customer's purchase
	!	order number.
	!	.b
	!	The field will accept up to ten (10) characters.
	!	.lm -5
	!
	! Index:
	!	.x Purchase Order>Customer
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::CUSTPO = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"15;17", TEMP$, OE_ORDERJOUR::CUSTPO, &
				MFLAG, "'E", MVALUE)

		CASE 8%
	!++
	! Abstract:FLD008
	!	.x Location>Number
	!	^*(08) Location Number\*
	!	.b
	!	.lm +5
	!	The ^*Location Number\* field enters a user defined code
	!	which identifies a company location from which the shipment is
	!	to be made.
	!	.b
	!	Valid location codes may be viewed by pressing ^*List Choices\*, or
	!	additional locations may be added by pressing ^*F17\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::LOCATION = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::WNUMBER, &
				"16;17", TEMP$, OE_ORDERJOUR::LOCATION, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::LOCATION = &
						UTL_LOCATION::LOCATION
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(UTL_MAIN_LOCATION.ID, "M")
				OE_ORDERJOUR::LOCATION = &
					UTL_LOCATION::LOCATION
				GOTO ReEnter

			END SELECT

		CASE 9%
	!++
	! Abstract:FLD009
	!	.x Ship to Date
	!	^*(09) Ship Date\*
	!	.b
	!	.lm +5
	!	The ^*Ship Date\* field indicates the date
	!	the Order-Invoice is requested or expected to be shipped.  The field
	!	will default to the system date.  To accept the system date, press ^*Return\*.
	!	To override the default, enter the correct date and press ^*Return\*.
	!	.b
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_ORDERJOUR::SHIPDATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::WNUMBER, &
				"17;17", TEMP$, OE_ORDERJOUR::SHIPDATE, &
				MFLAG, "'E", MVALUE)

		CASE 10%
	!++
	! Abstract:FLD010
	!	.x Ship VIA
	!	^*(10) Ship VIA\*
	!	.b
	!	.lm +5
	!	The ^*Ship VIA\* field enters a user defined
	!	code which identifies a particular carrier or method of shipment.
	!	.b
	!	Valid ship via codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_ORDERJOUR::SHIPVIA = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"1;18", TEMP$, OE_ORDERJOUR::SHIPVIA, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(UT_MAIN_CARRIER.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::SHIPVIA = &
						UTL_CARRIER::CODE
				END IF
				GOTO ReEnter

			CASE SMG$K_TRM_F17

				V% = MAIN_WINDOW(UT_MAIN_CARRIER.ID, "M")
				OE_ORDERJOUR::SHIPVIA = UTL_CARRIER::CODE
				GOTO ReEnter

			END SELECT

		CASE 11%
	!++
	! Abstract:FLD011
	!	.x Terms Code
	!	^*(11) Terms Code\*
	!	.b
	!	.lm +5
	!	The ^*Terms Code\* field enters a user defined
	!	number which will identify a particular terms description.
	!	.b
	!	Valid terms codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_ORDERJOUR::TERMS = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"2;18", TEMP$, OE_ORDERJOUR::TERMS, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(UT_MAIN_TERMS.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::TERMS = UTL_TERMS::CODE
				END IF
				GOTO Reenter

			END SELECT

		CASE 12%
	!++
	! Abstract:FLD012
	!	.ts 55
	!	^*(12) Freight\*
	!	.b
	!	.lm +5
	!	The ^*Freight\* field enters the freight charges related
	!	to the Order-Invoice.
	!	.lm -5
	!
	! Index:
	!	.x Freight
	!	.x Ticket>Freight
	!
	!--
			OE_ORDERJOUR::FREIGHT = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"3;18", TEMP$, OE_ORDERJOUR::FREIGHT, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 13%
	!++
	! Abstract:FLD013
	!	.ts 55
	!	^*(13) Handling\*
	!	.b
	!	.lm +5
	!	The ^*Handling\* field enters the handling
	!	charges related to the Order-Invoice.
	!	.lm -5
	!
	! Index:
	!	.x Handling
	!	.x Ticket>Handling
	!
	!--
			OE_ORDERJOUR::HANDLING = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"4;18", TEMP$, OE_ORDERJOUR::HANDLING, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 14%
	!++
	! Abstract:FLD014
	!	.x Ticket>Discount
	!	^*(14) Order-Invoice Discount Percentage\*
	!	.b
	!	.lm +5
	!	The ^*Order-Invoice Discount\* field
	!	enters a discount percentage which pertains to the entire
	!	document.  If there is no such discount, the field would be
	!	left blank.
	!	.b
	!	As an example, if the discount is to be 10% the entry would be made as 10.00.
	!	.lm -5
	!
	! Index:
	!	.x Discount
	!
	!--
			OE_ORDERJOUR::DISC = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"5;18", TEMP$, OE_ORDERJOUR::DISC, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 15%
	!++
	! Abstract:FLD015
	!	.x Miscellaneous Charges
	!	^*(15) Miscellaneous Charges\*
	!	.b
	!	.lm +5
	!	The ^*Miscellaneous Charges\* field
	!	enters any miscellaneous charges related to this
	!	Order Invoice.
	!	.b
	!	The field will accept a figure as large as 9,999,999.99.
	!	.lm -5
	!
	! Index:
	!	.x Ticket>Miscellaneous Charges
	!
	!--
			OE_ORDERJOUR::MISC = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"6;18", TEMP$, OE_ORDERJOUR::MISC, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 16%
	!++
	! Abstract:FLD016
	!	.x Miscellaneous Charges
	!	^*(16) Miscellaneous Charges Reason Code\*
	!	.b
	!	.lm +5
	!	The ^*Miscellaneous Charges Reason Code\* field enters
	!	the reason code for the miscellaneous charge entered in field (13).
	!	.b
	!	Valid reason codes may be viewed by pressing ^*List Choices\*.
	!	Additional reason codes may be added by pressing ^*F17\*.
	!	.lm -5
	!
	! Index:
	!	.x Reason Code
	!
	!--
			OE_ORDERJOUR::CREASON = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"7;18", TEMP$, OE_ORDERJOUR::CREASON, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(OE_MAIN_CREASON.ID, "V") = 1%
				THEN
					OE_ORDERJOUR::CREASON = &
						OE_CREASON::CREASON
				END IF
				GOTO Reenter
			END IF

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F17)
			THEN
				IF MAIN_WINDOW(OE_MAIN_CREASON.ID, "M") = 1%
				THEN
					OE_ORDERJOUR::CREASON = &
						OE_CREASON::CREASON
				END IF
				GOTO Reenter
			END IF

		CASE 17%
	!++
	! Abstract:FLD017
	!	^*(17) Miscellaneous Account\*
	!	.b
	!	.lm +5
	!	The ^*Miscellaneous Account\* field enters the
	!	account number of the miscellaneous account that will be
	!	affected by this transaction.
	!	.b
	!	If there is no miscellaneous charge, this field may be bypassed.
	!	This account will default to the miscellaneous account in the
	!	reason code table if a reason code has been entered.
	!	.B
	!	Valid miscellaneous accounts may be viewed by pressing ^*List
	!	Choices\*.  Additional miscellaneous accounts may be added by
	!	pressing ^*F17\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::MISC = 0.0

			OE_ORDERJOUR::MISCACCT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"8;18", TEMP$, OE_ORDERJOUR::MISCACCT, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::MISCACCT = GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F17)
			THEN
				IF MAIN_WINDOW(GL_MAIN_CHART.ID, "M") = 1%
				THEN
					OE_ORDERJOUR::MISCACCT = GL_CHART::ACCT
				END IF
				GOTO Reenter
			END IF

		CASE 18%
	!++
	! Abstract:FLD018
	!	.x Tax Code
	!	^*(18) Tax Code\*
	!	.b
	!	.lm +5
	!	The ^*Tax Code\* field identifies the sales tax
	!	jurisdiction relating to a specific customer. This field will default
	!	to the tax code in the customer master file.  To accept the default
	!	code, press ^*Return\*.  To enter a different code, enter the correct
	!	code and press ^*Return\*.
	!	.b
	!	Valid tax codes may be viewed by pressing ^*List Choices\*.
	!	.lm -5
	!
	! Index:
	!	.x Ticket>Tax Code
	!
	!--
			!MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::TAXCODE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"9;18", TEMP$, OE_ORDERJOUR::TAXCODE, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(OE_MAIN_SALESTAX.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::TAXCODE = &
						OE_SALESTAX::TAXCODE
				END IF
				GOTO Reenter

			END SELECT

		CASE 19%
	!++
	! Abstract:FLD019
	!	.x Tax Flag
	!	^*(19) Tax Flag\*
	!	.b
	!	.lm +5
	!	The ^*Tax Flag\* field enters the flag
	!	code which identifies the status of an Order Invoice as it
	!	relates to sales tax.
	!	.b
	!	Valid tax flags are:
	!	.table 3,25
	!	.te
	!	^*1\* - Taxable
	!	.te
	!	^*4\* - Resale
	!	.te
	!	^*5\* - Out of State
	!	.te
	!	^*6\* - Church, School, and Government
	!	.end table
	!	Valid tax flags may be viewed by pressing ^*List Choices\*.  This field will
	!	default to the tax flag entered in a customer master file.
	!	To accept the default, press ^*Return\*.  To override the default, enter
	!	the correct code and press ^*Return\*.
	!	.lm -5
	!
	! Index:
	!	.x Ticket>Tax Flag
	!
	!--
			!MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::TAXFLAG = ENTR_3STRINGLIST(SCOPE, &
				SMG_WINDOW::LWINDOW, "10;18", TEMP$, &
				OE_ORDERJOUR::TAXFLAG, MFLAG, "!", MVALUE, &
				TAXFLAG$(), TAXFLAG_T$, "005")

		CASE 20%
	!++
	! Abstract:FLD020
	!	.x Sales Tax
	!	^*(20) Sales Tax\*
	!	.b
	!	.lm +5
	!	The ^*Sales Tax\* field enters the percentage of sales tax
	!	that will be charged on a specific Order-Invoice.
	!	.b
	!	This field will default to the value in the customer master file if the
	!	tax flag field contains a 1.  If the tax flag field is a 4, 5, or 6,
	!	this field should be zero.
	!	.lm -5
	!
	! Index:
	!
	!--
			!MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::SALESTAX  = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"11;18", TEMP$, OE_ORDERJOUR::SALESTAX, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 21%
	!++
	! Abstract:FLD021
	!	.x Sales Type
	!	^*(21) Sales Type\*
	!	.b
	!	.lm +5
	!	The ^*Sales Type\* field enters a code
	!	which identifies the type of the Order-Invoice to be entered.
	!	.b
	!	Valid sales types may be viewed by pressing ^*List Choices\*.
	!	Additional sales types may be added by pressing ^*F17\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::ORDTYPE = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"12;18", TEMP$, OE_ORDERJOUR::ORDTYPE, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(OE_MAIN_ORDERTYPE.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::ORDTYPE = &
						OE_ORDERTYPE::ORDTYPE
				END IF
				GOTO Reenter

			CASE SMG$K_TRM_F17
				V% = MAIN_WINDOW(OE_MAIN_ORDERTYPE.ID, "M")
					OE_ORDERJOUR::ORDTYPE = &
					OE_ORDERTYPE::ORDTYPE
				GOTO ReEnter

			END SELECT

		CASE 22%
	!++
	! Abstract:FLD022
	!	.x Salesman
	!	^*(22) Salesman\*
	!	.b
	!	.lm +5
	!	The ^*Salesman\* field enters the code for the salesman
	!	or broker assigned to the specific customer related to the sales being entered.
	!	.b
	!	This field will default to the salesman code in the customer
	!	master file. To accept the default, press ^*Return\*. To override the
	!	default, enter the correct salesman code and press ^*Return\*.
	!	.lm -5
	!
	! Index:
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::SALESMAN = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"1;67", TEMP$, OE_ORDERJOUR::SALESMAN, &
				MFLAG, "'E", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_F14

				IF MAIN_WINDOW(SA_MAIN_SALESMAN.ID, "VX") = 1%
				THEN
					OE_ORDERJOUR::SALESMAN = &
						SA_SALESMAN::SALESMAN
				END IF
				GOTO Reenter

			END SELECT

		CASE 23%
	!++
	! Abstract:FLD023
	!	^*(23) Commission\*
	!	.b
	!	.lm +5
	!	The ^*Commission\* field enters
	!	the commission percentage for the related salesman or broker. This field will
	!	default to the commission percentage in the salesman master file. The default
	!	may be overridden by entering the correct percentage and pressing ^*Return\*.
	!	.lm -5
	!
	! Index:
	!	.x Sales Commission
	!
	!--
			MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::SALCOMM  = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"2;67", TEMP$, OE_ORDERJOUR::SALCOMM, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 24%
	!++
	! Abstract:FLD024
	!	^*(24) Amount Paid\*
	!	.b
	!	.lm +5
	!	The ^*Amount Paid\* field may be used in the event that a
	!	customer makes a payment at the time an Order-Invoice is made.
	!	This is memo information only.  Entering an amount in this
	!	field does not negate the necessity of making an entry in
	!	the cash receipts journal.
	!	.lm -5
	!
	! Index:
	!	.x Amount Paid
	!
	!--
			!MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::AMTPAID  = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"3;67", TEMP$, OE_ORDERJOUR::AMTPAID, &
				MFLAG, TRM$(FRM$(MLOOP)), MVALUE)

		CASE 25%
	!++
	! Abstract:FLD025
	!	^*(25) Check Number\*
	!	.b
	!	.lm +5
	!	The ^*Check Number\* field may be used to enter the check
	!	number in the event a customer makes payment at the time the
	!	Order-Invoice is placed.  This entry is for informational
	!	purposes only and entering the information does not negate the
	!	necessity of entering the payment in the cash receipts journal.
	!	.lm -5
	!
	! Index:
	!	.x Check Number
	!
	!--
			!MFLAG = MFLAG OR 1% IF OE_ORDERJOUR::REG_FLAG <> ""

			OE_ORDERJOUR::CHECK = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"4;67", TEMP$, OE_ORDERJOUR::CHECK, &
				MFLAG, "'E", MVALUE)

		CASE 26%
	!++
	! Abstract:FLD026
	!	.x Invoice AR Date
	!	^*(26) Invoice AR Date\*
	!	.b
	!	.lm +5
	!	The ^*Invoice AR Date\* field indicates
	!	the date for the first invoice record in the AR ledger.
	!	.B
	!	The format for entry is MMDDYYYY or MMDDYY.
	!	.lm -5
	!
	! Index:
	!
	!--
			OE_ORDERJOUR::TRANDATE = &
				ENTR_3DATE(SCOPE, SMG_WINDOW::LWINDOW, &
				"5;67", TEMP$, OE_ORDERJOUR::TRANDATE, &
				MFLAG, "6", MVALUE)

		CASE 27%
	!++
	! Abstract:FLD027
	!	^*(27) Invoice Number\*
	!	.b
	!	.lm +5
	!	The ^*Invoice Number\* field contains the invoice number of
	!	the document.  This invoice number field normally is automatically
	!	entered and updated when invoices are printed.
	!	.lm -5
	!
	! Index:
	!	.x Invoice Number
	!
	!--
			OE_ORDERJOUR::INVNUM = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"6;67", TEMP$, OE_ORDERJOUR::INVNUM, &
				MFLAG, "'E", MVALUE)

		CASE 28%
	!++
	! Abstract:FLD028
	!	^*(28) Number of Payments\*
	!	.b
	!	.lm +5
	!	The ^*Number of Payments\* field determines the number of
	!	installment payments, per invoice, that are in the monthly
	!	payment arrangements.
	!	.lm -5
	!
	! Index:
	!	.x Number of Payments
	!
	!--
			OE_ORDERJOUR::PAYMNT = &
				ENTR_3NUMBER(SCOPE, SMG_WINDOW::LWINDOW, &
				"7;67", TEMP$, OE_ORDERJOUR::PAYMNT * 1.0, &
				MFLAG, "#", MVALUE)

		CASE 29%
	!++
	! Abstract:FLD029
	!	^*(29) Notes\*
	!	.b
	!	.lm +5
	!	The ^*Notes\* field enters any free formatted notes
	!	relative to the Order-Invoice.
	!	.lm -5
	!
	! Index:
	!	.x Notes
	!
	!--
 FirstNote:
			OE_ORDERJOUR::NOTES(0%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"13;18", TEMP$, OE_ORDERJOUR::NOTES(0%), &
				MFLAG, "'LLLLLLLLLLLLLLLLLLLLLLLLL", MVALUE)

			GOTO BypassNotes IF OE_ORDERJOUR::NOTES(0%) = "" &
				AND OE_ORDERJOUR::NOTES(1%) = "" &
				AND OE_ORDERJOUR::NOTES(2%) = ""

 SecondNote:
			OE_ORDERJOUR::NOTES(1%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"14;18", TEMP$, OE_ORDERJOUR::NOTES(1%), &
				MFLAG, "'LLLLLLLLLLLLLLLLLLLLLLLLL", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO FirstNote

			END SELECT

			OE_ORDERJOUR::NOTES(2%) = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"15;18", TEMP$, OE_ORDERJOUR::NOTES(2%), &
				MFLAG, "'LLLLLLLLLLLLLLLLLLLLLLLLL", MVALUE)

			SELECT SCOPE::SCOPE_EXIT

			CASE SMG$K_TRM_UP
				GOTO SecondNote

			END SELECT
 BypassNotes:

		CASE 30%
	!++
	! Abstract:FLD030
	!	^*(28) Deposit Number\*
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!	.x Deposit
	!
	!--
			OE_ORDERJOUR::DEPOSIT = &
				ENTR_3STRING(SCOPE, SMG_WINDOW::LWINDOW, &
				"8;67", TEMP$, OE_ORDERJOUR::DEPOSIT, &
				MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP$

	!
	! Test the Entry.
	!
20300	CASE OPT_TESTENTRY

		PS_MAIN_TICKETJOUR = 0%

		SELECT MLOOP

		CASE 1%

			!
			! Must have an order number
			!
			IF OE_ORDERJOUR::ORDNUM = ""
			THEN
				IF MVALUE = "ADD"
				THEN
					GOSUB GetRec
					SMG_STATUS% = SMG$PUT_CHARS( &
						SMG_WINDOW::WNUMBER, &
						EDIT$(OE_ORDERJOUR::ORDNUM, -1%), &
						1%, 17%, , SMG$M_BOLD)
					GOTO 32767
				END IF
			END IF

			!
			! See if order already exists in journal
			!
			IF MVALUE = "ADD"
			THEN
				WHEN ERROR IN
					GET #OE_ORDERJOUR.CH%, &
						KEY #0% EQ OE_ORDERJOUR::ORDNUM + "", &
						REGARDLESS
				USE
					CONTINUE 20350 IF ERR = 155%
					EXIT HANDLER
				END WHEN

				PS_MAIN_TICKETJOUR = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Record Already Exists", 1%)
				GOTO 32767
			END IF

			!
			! See if order already exists in journal
			!
			IF MVALUE = "CHANGE" AND OE_ORDERJOUR::REG_FLAG = "Y"
			THEN
				PS_MAIN_TICKETJOUR = 2%
				CALL ENTR_3MESSAGE(SCOPE, &
					"Can't change document number", 1%)
				GOTO 32767
			END IF

20350			!
			! Check for this Ticket number already in register file
			!
			IF MVALUE = "ADD"
			THEN
				IF OE_READ_REGHEADER(OE_ORDERJOUR::ORDNUM, &
					OE_REGHEADER_READ) <> CMC$_NORMAL
				THEN
					GOTO 32767
				ELSE
					CALL ENTR_3MESSAGE(SCOPE, &
						"Ticket Already Exists in register", 1%)

					!
					! Initialize record with register fields
					!
					OE_ORDERJOUR::REG_FLAG	= "Y"
					OE_ORDERJOUR::ORDNUM	= OE_REGHEADER_READ::ORDNUM
					OE_ORDERJOUR::ORDDATE	= OE_REGHEADER_READ::ORDDATE
					OE_ORDERJOUR::ORDTYPE	= OE_REGHEADER_READ::ORDTYPE
					OE_ORDERJOUR::ORDCAT	= OE_REGHEADER_READ::ORDCAT
					OE_ORDERJOUR::CUSNUM	= OE_REGHEADER_READ::CUSNUM
					OE_ORDERJOUR::DISC	= OE_REGHEADER_READ::DISC
					OE_ORDERJOUR::SHIPNAM	= OE_REGHEADER_READ::SHIPNAM
					OE_ORDERJOUR::ADD1	= OE_REGHEADER_READ::ADD1
					OE_ORDERJOUR::ADD2	= OE_REGHEADER_READ::ADD2
					OE_ORDERJOUR::ADD3	= OE_REGHEADER_READ::ADD3
					OE_ORDERJOUR::CITY	= OE_REGHEADER_READ::CITY
					OE_ORDERJOUR::STATE	= OE_REGHEADER_READ::STATE
					OE_ORDERJOUR::ZIP	= OE_REGHEADER_READ::ZIP
					OE_ORDERJOUR::COUNTRY	= OE_REGHEADER_READ::COUNTRY
					OE_ORDERJOUR::CUSTPO	= OE_REGHEADER_READ::CUSTPO
					OE_ORDERJOUR::SHIPVIA	= OE_REGHEADER_READ::SHIPVIA
					OE_ORDERJOUR::TERMS	= OE_REGHEADER_READ::TERMS
					OE_ORDERJOUR::LOCATION	= OE_REGHEADER_READ::LOCATION
					OE_ORDERJOUR::COMMAMT	= OE_REGHEADER_READ::COMMAMT
					OE_ORDERJOUR::SALESMAN	= OE_REGHEADER_READ::SALESMAN
					OE_ORDERJOUR::SALCOMM	= OE_REGHEADER_READ::SALCOMM
					OE_ORDERJOUR::NOTES(0%)	= OE_REGHEADER_READ::NOTES(0%)
					OE_ORDERJOUR::NOTES(1%)	= OE_REGHEADER_READ::NOTES(1%)
					OE_ORDERJOUR::TRANDATE	= DATE_TODAY
					OE_ORDERJOUR::TRANTIME	= TIME_NOW
					OE_ORDERJOUR::TAXCODE	= OE_REGHEADER_READ::TAXCODE
					OE_ORDERJOUR::TAXFLAG	= OE_REGHEADER_READ::TAXFLAG
					OE_ORDERJOUR::SHIPLIN	= OE_REGHEADER_READ::SHIPLIN
				END IF
			END IF

		CASE 3%
			!
			! check operator
			!
			IF OE_ORDERJOUR::OPERATOR <> ""
			THEN
				PS_MAIN_TICKETJOUR = &
					FUNC_TESTENTRY(SMG_WINDOW, &
					"S" + OE_ORDERJOUR::OPERATOR, &
					SA_SALESMAN::DESCR, &
					"OE", MLOOP, "PROG", &
					"Operator Code ", SA_MAIN_SALESMAN.ID)
			END IF

		CASE 4%
			!
			! Display the descriptions for category
			!
			IF OE_ORDERJOUR::ORDCAT <> ""
			THEN
				PS_MAIN_TICKETJOUR = &
					FUNC_TESTENTRY(SMG_WINDOW, &
					OE_ORDERJOUR::ORDCAT, &
					OE_CATEGORY::DESCRIPTION, &
					"OE", MLOOP, "PROG", &
					"Category", OE_MAIN_CATEGORY.ID)
			ELSE
				OE_CATEGORY::DESCRIPTION = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				OE_CATEGORY::DESCRIPTION, &
				4%, 23%, , SMG$M_BOLD)

		CASE 5%
			AR_35CUSTOM::CUSNAM = ""
			AR_35CUSTOM::ADD1 = ""
			AR_35CUSTOM::ADD2 = ""
			AR_35CUSTOM::CITY = ""
			AR_35CUSTOM::STATE = ""
			AR_35CUSTOM::ZIP = ""
			AR_35CUSTOM::TTYPE = ""
			AR_35CUSTOM::CATEGORY = ""
			AR_35CUSTOM::DISCOUNT  = 0.0
			AR_35CUSTOM::CREDITLIM = 0.0
			AR_35CUSTOM::LOCATION = ""
			AR_35CUSTOM::TERMS = ""
			AR_35CUSTOM::SALESMAN = ""
			IF OE_ORDERJOUR::CUSNUM <> ""
			THEN
				!
				! Display the descriptions for customer name
				!
				PS_MAIN_TICKETJOUR = &
					FUNC_TESTENTRY(SMG_WINDOW, &
					OE_ORDERJOUR::CUSNUM, &
					AR_35CUSTOM::CUSNUM, &
					"OE", MLOOP, "PROG", &
					"Customer",AR_MAIN_35CUSTOM.ID)

				IF OE_ORDERJOUR::REG_FLAG <> "Y"
				THEN
					OE_ORDERJOUR::DISC     = AR_35CUSTOM::DISCOUNT
					OE_ORDERJOUR::LOCATION = AR_35CUSTOM::LOCATION
					OE_ORDERJOUR::TERMS    = AR_35CUSTOM::TERMS
					OE_ORDERJOUR::SALESMAN = AR_35CUSTOM::SALESMAN
				END IF

			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::CUSNAM, 5%, 28%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::ADD1, 6%, 28%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::ADD2, 7%, 28%, , SMG$M_BOLD)

			TEXT$ = LEFT(EDIT$(EDIT$( &
				AR_35CUSTOM::CITY, 128%) + "  " + &
				AR_35CUSTOM::STATE      + " "  + &
				AR_35CUSTOM::ZIP, 16%)  + &
				SPACE$(30%), 30%)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				TEXT$, 8%, 28%, , SMG$M_BOLD)

			GOTO ExitFunction IF OE_ORDERJOUR::CUSNUM = "" OR &
				(AR_35CUSTOM::SSTATUS = "C")

			!
			! Zero balances
			!
			CUSBAL(J%) = 0.0 FOR J% = 0% TO 4%
			SRVCHG = 0.0

			IF AR_FUNC_AGE(AR_35CUSTOM::CUSNUM, &
				AR_35CUSTOM::METHOD, &
				OE_ORDERJOUR::ORDDATE, "", &
				NUM_ACCT%, ARRAY_CUSBAL()) = 0%
			THEN

				!
				! Accumulate aging information
				!
				FOR LOOP% = 1% TO NUM_ACCT%
					!
					! Customer total
					!
					CUSBAL(J%) = CUSBAL(J%) + &
						ARRAY_CUSBAL(LOOP%)::AGING(J%) &
						FOR J% = 0% TO 4%

					SRVCHG = SRVCHG + &
						ARRAY_CUSBAL(LOOP%)::CHARGE

				NEXT LOOP%

				TEXT$ = "   Cur    "
				DAYS% = 1%

				FOR I% = 1% TO 4%
					DAYS% = DAYS%+AR_CONTROL::AGEPER(I%)
					TEXT$ = TEXT$ + &
						FORMAT$(DAYS%, "###") + "    "
				NEXT I%

				TEXT$ = TEXT$ + " Bal CrLimit"

				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::WNUMBER, &
					TEXT$, 1%, 28%, , SMG$M_REVERSE)

				!
				! Accumulate balance
				!
				BALANCE = &
					FUNC_ROUND(CUSBAL(0%) + &
					CUSBAL(1%) + &
					CUSBAL(2%) + &
					CUSBAL(3%) + &
					CUSBAL(4%) + &
					SRVCHG, 2%)
			END IF

			TEXT$ = FORMAT$(CUSBAL(0%), " ######") + &
				FORMAT$(CUSBAL(1%), " ######") + &
				FORMAT$(CUSBAL(2%), " ######") + &
				FORMAT$(CUSBAL(3%), " ######") + &
				FORMAT$(CUSBAL(4%), " ######") + &
				FORMAT$(BALANCE, " #######")   + &
				FORMAT$(AR_35CUSTOM::CREDITLIM, " #######")

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				TEXT$, 2%, 27%, ,)

		CASE 6%
			!
			! Fix OE_ORDERJOUR::ORDNUM
			!
			IF OE_ORDERJOUR::SHIPLIN = "" AND &
				OE_ORDERJOUR::CUSNUM = ""
			THEN
				PS_MAIN_TICKETJOUR = 1%
				GOTO 32767
			END IF

		CASE 8%
			!
			! Display the descriptions for location name
			!
			PS_MAIN_TICKETJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_ORDERJOUR::LOCATION, &
				UTL_LOCATION::LOCNAME, &
				"OE", MLOOP, "PROG", &
				"Location", UTL_MAIN_LOCATION.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 16%, 34%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				SPACE$(51%), 1%, 27%, ,)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				SPACE$(51%), 2%, 27%, ,)

		CASE 10%
			!
			! Display the descriptions for carrier
			!
			IF OE_ORDERJOUR::SHIPVIA <> ""
			THEN
				PS_MAIN_TICKETJOUR = &
					FUNC_TESTENTRY(SMG_WINDOW, &
					OE_ORDERJOUR::SHIPVIA, &
					UTL_CARRIER::DESCR, &
					"OE", MLOOP, "PROG", &
					"Carrier", UT_MAIN_CARRIER.ID)
			ELSE
				UTL_CARRIER::DESCR = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				LEFT(UTL_CARRIER::DESCR, 30%), 1%, 21%, , &
				SMG$M_BOLD)

		CASE 11%
			!
			! Display the descriptions for terms
			!
			IF OE_ORDERJOUR::TERMS <> ""
			THEN
				PS_MAIN_TICKETJOUR = &
					FUNC_TESTENTRY(SMG_WINDOW, &
					OE_ORDERJOUR::TERMS, &
					UTL_TERMS::DESCR, &
					"OE", MLOOP, "PROG", &
					"Terms", UT_MAIN_TERMS.ID)
			ELSE
				UTL_TERMS::DESCR = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				LEFT(UTL_TERMS::DESCR, 30%), 2%, 21%, , &
				SMG$M_BOLD)

		CASE 16%
			IF OE_ORDERJOUR::MISC <> 0.0 OR &
				OE_ORDERJOUR::CREASON <> ""
			THEN
				OE_CREASON::DESCR   = ""

				PS_MAIN_TICKETJOUR = &
					FUNC_TESTENTRY(SMG_WINDOW, &
					OE_ORDERJOUR::CREASON, &
					OE_CREASON::DESCR, &
					"OE", MLOOP, "PROG", &
					"Reason Code ", OE_MAIN_CREASON.ID)

				SMG_STATUS% = &
					FUNC_TESTENTRY(SMG_WINDOW, &
					OE_ORDERJOUR::CREASON + &
						OE_ORDERJOUR::LOCATION, &
					OE_REASONACCT::ACCOUNT, &
					"OE", MLOOP, "PROG", &
					"Reason Account ", &
					OE_MAIN_REASONACCT.ID)

				OE_ORDERJOUR::MISCACCT = OE_REASONACCT::ACCOUNT

				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::LWINDOW, &
					LEFT(OE_CREASON::DESCR, 30%), &
					7%, 21%, , &
					SMG$M_BOLD)

				SMG_STATUS% = SMG$PUT_CHARS( &
					SMG_WINDOW::LWINDOW, &
					OE_REASONACCT::ACCOUNT, 8%, 18%, , &
					SMG$M_BOLD)
			END IF

		CASE 17%
			GL_CHART::DESCR = ""

			IF OE_ORDERJOUR::MISCACCT <> ""
			THEN
				OE_MAIN_ORDERJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
					OE_ORDERJOUR::MISCACCT, &
					GL_CHART::DESCR, &
					"OE", MLOOP, "PROG", &
					"Misc Account", GL_MAIN_CHART.ID)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 9%, 40%, , SMG$M_BOLD)

		CASE 19%
 !			IF (OE_ORDERJOUR::TAXFLAG = "1")
 !			THEN
				!
				! Use function to find out tax percentages and
				! total them up.
				!
				V% = OE_READ_SALESTAX(AR_35CUSTOM::TAXCODE, &
					OE_ORDERJOUR::TAXFLAG, OE_SALESTAX_READ)

				OE_ORDERJOUR::SALESTAX = &
					OE_SALESTAX_READ::STATETAX + &
					OE_SALESTAX_READ::COUNTYTAX + &
					OE_SALESTAX_READ::CITYTAX
 !			ELSE
 !				OE_ORDERJOUR::SALESTAX = 0.0
 !			END IF

		CASE 21%
			!
			! Display the descriptions for Ticket type
			!
			PS_MAIN_TICKETJOUR = FUNC_TESTENTRY(SMG_WINDOW, &
				OE_ORDERJOUR::ORDTYPE, &
				OE_ORDERTYPE::DESCRIPTION, &
				"OE", MLOOP, "PROG", &
				"Ticket Type", OE_MAIN_ORDERTYPE.ID)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				LEFT(OE_ORDERTYPE::DESCRIPTION, 25%), &
				12%, 21%, , SMG$M_BOLD)

			GOSUB DispNetLine

		CASE 22%
			!
			! Get default salesman's commission % on adding
			!
			IF OE_ORDERJOUR::SALESMAN <> ""
			THEN
				V% = SA_EXAM_SALESMAN(OE_ORDERJOUR::SALESMAN, &
					SA_SALESMAN_EXAM, SB_SUBACCOUNT_EXAM)

				IF SA_SALESMAN::SALESMAN = &
					OE_ORDERJOUR::SALESMAN
				THEN
					OE_ORDERJOUR::SALCOMM = &
						SA_SALESMAN::COMMPER
				ELSE
					PS_MAIN_TICKETJOUR = 1%
					CALL HELP_34MESSAGE(SCOPE, &
						"Invalid Salesman Number", &
						"W", "SA_EXAM_SALESMAN", "", &
						"INVSAL")
				END IF
			END IF

		CASE 28%
			IF OE_ORDERJOUR::PAYMNT <= 0%
			THEN
				CALL HELP_34MESSAGE(SCOPE, &
					"invalid number of payments", &
					"W", SCOPE::PRG_PROGRAM, "", "INVPAY")

				PS_MAIN_TICKETJOUR = 1%
			END IF

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(4%) AND 2%) = 0%
		THEN
			OE_CATEGORY::DESCRIPTION = &
				STRING$(LEN(OE_CATEGORY::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(OE_MAIN_CATEGORY.ID, &
				"Q0" + OE_ORDERJOUR::ORDCAT) <> 1%

			OE_CATEGORY::DESCRIPTION = "" &
				IF OE_ORDERJOUR::ORDCAT = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(OE_CATEGORY::DESCRIPTION, 33%), &
				4%, 23%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(5%) AND 2%) = 0%
		THEN
			TEXT$ = LEFT(EDIT$(EDIT$( &
				AR_35CUSTOM::CITY, 128%) + ", " + &
				AR_35CUSTOM::STATE + " "  + &
				AR_35CUSTOM::ZIP, 16%)  + &
				SPACE$(30%), 30%)

			IF MAIN_WINDOW(AR_MAIN_35CUSTOM.ID, &
				"Q0" + OE_ORDERJOUR::CUSNUM) <> 1%
			THEN
				IF OE_ORDERJOUR::CUSNUM <> ""
				THEN
					AR_35CUSTOM::CUSNAM = &
						STRING$(LEN(AR_35CUSTOM::CUSNAM), A"?"B)

					AR_35CUSTOM::ADD1 = &
						STRING$(LEN(AR_35CUSTOM::ADD1), A"?"B)

					AR_35CUSTOM::ADD2 = &
						STRING$(LEN(AR_35CUSTOM::ADD2), A"?"B)

					TEXT$ = STRING$(LEN(TEXT$), A"?"B)
				ELSE
					AR_35CUSTOM::CUSNAM = ""
					AR_35CUSTOM::ADD1 = ""
					AR_35CUSTOM::ADD2 = ""
					TEXT$ = SPACE$(LEN(TEXT$))
				END IF
			ELSE
				TEXT$ = LEFT(EDIT$(EDIT$( &
					AR_35CUSTOM::CITY, 128%) + ", " + &
					AR_35CUSTOM::STATE + " "  + &
					AR_35CUSTOM::ZIP, 16%)  + &
					SPACE$(30%), 30%)
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::CUSNAM, 5%, 28%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::ADD1, 6%, 28%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				AR_35CUSTOM::ADD2, 7%, 28%, , SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				TEXT$, 8%, 28%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(8%) AND 2%) = 0%
		THEN
			UTL_LOCATION::LOCNAME = &
				STRING$(LEN(UTL_LOCATION::LOCNAME), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
				"Q0" + OE_ORDERJOUR::LOCATION) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				UTL_LOCATION::LOCNAME, 16%, 34%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(10%) AND 2%) = 0%
		THEN
			UTL_CARRIER::DESCR = &
				STRING$(LEN(UTL_CARRIER::DESCR), A"?"B) &
				IF MAIN_WINDOW(UT_MAIN_CARRIER.ID, &
				"Q0" + OE_ORDERJOUR::SHIPVIA) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				LEFT(UTL_CARRIER::DESCR, 30%), 1%, 21%, , &
				SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(11%) AND 2%) = 0%
		THEN
			UTL_TERMS::DESCR = &
				STRING$(LEN(UTL_TERMS::DESCR), A"?"B) &
				IF MAIN_WINDOW(UT_MAIN_TERMS.ID, &
				"Q0" + OE_ORDERJOUR::TERMS) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				LEFT(UTL_TERMS::DESCR, 30%), 2%, 21%, , &
				SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(16%) AND 2%) = 0%
		THEN
			IF OE_ORDERJOUR::CREASON <> ""
			THEN
				IF MAIN_WINDOW (OE_MAIN_CREASON.ID, &
					"Q0" + OE_ORDERJOUR::CREASON) <> 1%
				THEN
					OE_CREASON::DESCR = &
						STRING$(LEN(OE_CREASON::DESCR), &
						A"?"B)
				END IF
			ELSE
				OE_CREASON::DESCR = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				LEFT(OE_CREASON::DESCR, 30%), 7%, 21%, , &
				SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(17%) AND 2%) = 0%
		THEN
			GL_CHART::DESCR = &
				STRING$(LEN(GL_CHART::DESCR), A"?"B) &
				IF MAIN_WINDOW (GL_MAIN_CHART.ID, "Q0" + &
					OE_ORDERJOUR::MISCACCT) <> 1%

			GL_CHART::DESCR = "" IF OE_ORDERJOUR::MISCACCT = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				GL_CHART::DESCR, 9%, 40%, , SMG$M_BOLD)
		END IF

		IF (SMG_WINDOW::HFLAG(21%) AND 2%) = 0%
		THEN
			OE_ORDERTYPE::DESCRIPTION = &
				STRING$(LEN(OE_ORDERTYPE::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(OE_MAIN_ORDERTYPE.ID, &
				"Q0" + OE_ORDERJOUR::ORDTYPE) <> 1%

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
				LEFT(OE_ORDERTYPE::DESCRIPTION, 25%), &
				12%, 21%, , SMG$M_BOLD)
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			SPACE$(51%), 1%, 27%, ,)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
			SPACE$(51%), 2%, 27%, ,)

		GOSUB DispNetLine

	!
	! Set OE_ORDERJOUR_OLD value
	!
20500	CASE OPT_SETOLD
		OE_ORDERJOUR_OLD = OE_ORDERJOUR

	!
	! Restore OE_ORDERJOUR_OLD value
	!
	CASE OPT_RESETOLD
		OE_ORDERJOUR = OE_ORDERJOUR_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		OE_ORDERJOUR_DEF = OE_ORDERJOUR
		OE_ORDERJOUR_DEF::DEPOSIT = ""

		IF MFLAG = 1%
		THEN
			SELECT MLOOP

			CASE 0%
				FRM$(12%) = "###,###.##"
				FRM$(13%) = "###,###.##"
				FRM$(14%) = "###.##%"
				FRM$(15%) = "###,###.##"
				FRM$(20%) = "###.##%"
				FRM$(23%) = "###.##%"
				FRM$(24%) = "#,###,###.##"

			CASE ELSE
				FRM$(MLOOP) = MVALUE
			END SELECT

		END IF

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		OE_ORDERJOUR = OE_ORDERJOUR_DEF
		OE_ORDERJOUR::REG_FLAG = ""
		OE_ORDERJOUR::DEPOSIT = ""

		IF MFLAG = 1%
		THEN
			OE_ORDERJOUR::ORDDATE = DATE_TODAY IF &
				OE_ORDERJOUR::ORDDATE = ""

			OE_ORDERJOUR::SHIPDATE = DATE_TODAY IF &
				OE_ORDERJOUR::SHIPDATE = ""

			OE_ORDERJOUR::PAYMNT = 1% IF &
				OE_ORDERJOUR::PAYMNT = 0%

			OE_ORDERJOUR::TRANDATE = DATE_TODAY IF &
				OE_ORDERJOUR::TRANDATE = ""

		END IF

		OE_ORDERJOUR::TRANTIME = TIME_NOW
		OE_ORDERJOUR::INVNUM = ""

	!
	! View the Record.
	!
	CASE OPT_VIEW

		SELECT MLOOP

		CASE 1%
			MVALUE = "  DocNumber  Date       Type Category " + &
				"CusNumber  ShipName"

		CASE 2%
			MVALUE = "013,024,029,038,049"

		CASE 3%
			MVALUE = CONV_STRING(OE_ORDERJOUR::ORDNUM, CMC$_LEFT) + " "     + &
				PRNT_DATE(OE_ORDERJOUR::ORDDATE, 8%) + " " + &
				OE_ORDERJOUR::ORDTYPE + "   " + &
				OE_ORDERJOUR::ORDCAT + "     " + &
				OE_ORDERJOUR::CUSNUM + " " + &
				OE_ORDERJOUR::SHIPNAM

		END SELECT

	!
	! Find the Ticket Number.
	!
	CASE OPT_FIND

		SELECT MLOOP

		CASE 0%
			FIND #OE_ORDERJOUR.CH%, &
				KEY #0% GE OE_ORDERJOUR::ORDNUM + "", &
				REGARDLESS

		CASE 1%
			FIND #OE_ORDERJOUR.CH%, &
				KEY #1% GE OE_ORDERJOUR::ORDTYPE + &
				OE_ORDERJOUR::ORDNUM, REGARDLESS

		CASE 2%
			FIND #OE_ORDERJOUR.CH%, &
				KEY #2% GE OE_ORDERJOUR::CUSNUM + &
				OE_ORDERJOUR::ORDNUM, REGARDLESS

		CASE 3%
			FIND #OE_ORDERJOUR.CH%, &
				KEY #3% GE OE_ORDERJOUR::ORDCAT + &
				OE_ORDERJOUR::ORDNUM, REGARDLESS

		END SELECT

	END SELECT


 ExitFunction:
	EXIT FUNCTION

 LoadShip:
	IF EDIT$(OE_ORDERJOUR::SHIPLIN, -1%) <> ""
	THEN
		IF OE_ORDERJOUR::CUSNUM <> ""
		THEN
			V% = MAIN_WINDOW(OE_MAIN_SHIPTO.ID, &
				"Q0" + OE_ORDERJOUR::CUSNUM + &
				OE_ORDERJOUR::SHIPLIN)
		ELSE
			V% = MAIN_WINDOW(UTL_MAIN_LOCATION.ID, &
				"Q0" + OE_ORDERJOUR::SHIPLIN)

		END IF

		IF V% = 1%
		THEN
			IF OE_ORDERJOUR::CUSNUM = ""
			THEN
				OE_ORDERJOUR::SHIPNAM  = UTL_LOCATION::LOCNAME
				OE_ORDERJOUR::ADD1     = UTL_LOCATION::ADDRESS1
				OE_ORDERJOUR::ADD2     = UTL_LOCATION::ADDRESS2
				OE_ORDERJOUR::ADD3     = ""
				OE_ORDERJOUR::CITY     = UTL_LOCATION::CITY
				OE_ORDERJOUR::STATE    = UTL_LOCATION::STATE
				OE_ORDERJOUR::ZIP      = UTL_LOCATION::ZIP
				OE_ORDERJOUR::COUNTRY  = UTL_LOCATION::COUNTRY
				OE_ORDERJOUR::TAXFLAG  = "4"
			ELSE
				OE_ORDERJOUR::SHIPNAM  = OE_SHIPTO::SHIPNAM
				OE_ORDERJOUR::ADD1     = OE_SHIPTO::ADD1
				OE_ORDERJOUR::ADD2     = OE_SHIPTO::ADD2
				OE_ORDERJOUR::ADD3     = OE_SHIPTO::ADD3
				OE_ORDERJOUR::CITY     = OE_SHIPTO::CITY
				OE_ORDERJOUR::STATE    = OE_SHIPTO::STATE
				OE_ORDERJOUR::ZIP      = OE_SHIPTO::ZIP
				OE_ORDERJOUR::COUNTRY  = OE_SHIPTO::COUNTRY
				OE_ORDERJOUR::TAXCODE  = OE_SHIPTO::TAXCODE
				OE_ORDERJOUR::TAXFLAG  = AR_35CUSTOM::TAXFLAG
				OE_ORDERJOUR::SHIPVIA  = OE_SHIPTO::SHIPVIA
				OE_ORDERJOUR::LOCATION = OE_SHIPTO::LOCATION
			END IF
		ELSE
				OE_ORDERJOUR::SHIPNAM = ""
				OE_ORDERJOUR::ADD1 = ""
				OE_ORDERJOUR::ADD2 = ""
				OE_ORDERJOUR::ADD3 = ""
				OE_ORDERJOUR::CITY = ""
				OE_ORDERJOUR::STATE = ""
				OE_ORDERJOUR::ZIP = ""
				OE_ORDERJOUR::COUNTRY = ""
		END IF
	ELSE
			V% = 1%
			OE_ORDERJOUR::SHIPNAM  = AR_35CUSTOM::CUSNAM
			OE_ORDERJOUR::ADD1     = AR_35CUSTOM::ADD1
			OE_ORDERJOUR::ADD2     = AR_35CUSTOM::ADD2
			OE_ORDERJOUR::ADD3     = AR_35CUSTOM::ADD3
			OE_ORDERJOUR::CITY     = AR_35CUSTOM::CITY
			OE_ORDERJOUR::STATE    = AR_35CUSTOM::STATE
			OE_ORDERJOUR::ZIP      = AR_35CUSTOM::ZIP
			OE_ORDERJOUR::COUNTRY  = AR_35CUSTOM::COUNTRY

			OE_ORDERJOUR::TAXCODE  = AR_35CUSTOM::TAXCODE &
				IF OE_ORDERJOUR::TAXCODE = ""

			OE_ORDERJOUR::TAXFLAG  = AR_35CUSTOM::TAXFLAG &
				IF OE_ORDERJOUR::TAXFLAG = ""

			OE_ORDERJOUR::SHIPVIA  = AR_35CUSTOM::CARRIER &
				IF OE_ORDERJOUR::SHIPVIA = ""

			OE_ORDERJOUR::LOCATION = AR_35CUSTOM::LOCATION &
				IF OE_ORDERJOUR::LOCATION = ""
	END IF

	RETURN

 DispNetLine:
	!
	! Read Net line total
	!
	V% = OE_READ_ORDERTOTAL(OE_ORDERJOUR::ORDNUM, BATCH_NO$, TOT())

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
		"====order=====  =====invoice====", 10%, 46%, , SMG$M_REVERSE)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
		FORMAT$(TOT(1%), "#,###,###.##") + &
		FORMAT$(TOT(2%), " NetLin #,###,###.##"), &
		11%, 46%, , SMG$M_BOLD)

	DISC1 = FUNC_ROUND(TOT(1%) * OE_ORDERJOUR::DISC / 100.0, 2%)
	DISC2 = FUNC_ROUND(TOT(2%) * OE_ORDERJOUR::DISC / 100.0, 2%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
		FORMAT$(-DISC1, "#,###,###.##") + &
		" Disc   " + FORMAT$(-DISC2, "#,###,###.##"), &
		12%, 46%, , SMG$M_BOLD)

	SALTAX1 = FUNC_ROUND((TOT(1%) - TOT(3%) - DISC1) * &
		OE_ORDERJOUR::SALESTAX / 100.0, 2%)
	SALTAX2 = FUNC_ROUND((TOT(2%) - TOT(4%) - DISC2) * &
		OE_ORDERJOUR::SALESTAX / 100.0, 2%)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
		FORMAT$(SALTAX1, "#,###,###.##") + &
		FORMAT$(SALTAX2, " STax   #,###,###.##"), &
		13%, 46%, , SMG$M_BOLD)

	OTHER = OE_ORDERJOUR::HANDLING + OE_ORDERJOUR::FREIGHT + &
		OE_ORDERJOUR::MISC

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
		FORMAT$(OTHER, "#,###,###.##") + &
		FORMAT$(OTHER, " Other  #,###,###.##"), &
		14%, 46%, , SMG$M_BOLD)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::LWINDOW, &
		FORMAT$(TOT(1%) - DISC1 + SALTAX1 + OTHER, &
			"#,###,###.##") + &
		FORMAT$(TOT(2%) - DISC2 + SALTAX2 + OTHER, &
			" Total  #,###,###.##"), &
		15%, 46%, , SMG$M_REVERSE)

	RETURN

 GetRec:
	!
	! Open OE_ORDERJOUR
	!
20800	IF PS_CONTROL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PS.OPEN]PS_CONTROL.CRE"
		USE
			CONTINUE 20850 IF ERR = 5%
			EXIT HANDLER
		END WHEN
	END IF

	!
	! Get the PS_CONTROL record
	!
20810	WHEN ERROR IN
		GET #PS_CONTROL.CH%, RECORD 1%
		V% = FUNC_INCREMENT(PS_CONTROL::LAST_TICKET)
		UPDATE #PS_CONTROL.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 20850 IF ERR = 9% OR ERR = 155%
		EXIT HANDLER
	END WHEN

	GOTO AssgNumber

20850	PS_CONTROL::LAST_TICKET = "         1"
	PS_CONTROL::PURGDATE = ""
	PUT #PS_CONTROL.CH%

 AssgNumber:
	OE_ORDERJOUR::ORDNUM = PS_CONTROL::LAST_TICKET
	UNLOCK #PS_CONTROL.CH%

	RETURN

	%PAGE

29000	!*******************************************************************
	!	Help Errors
	!*******************************************************************

	!
	! Resume to display untrapped error
	!
	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:LINE
	!	^*Line\*
	!	.b
	!	.lm +5
	!	The ^*Line\* function
	!	enters a line of data for each inventory item
	!	listed on the ticket.
	!	.b
	!	The fields in the line item screen include:
	!	.table 3,25
	!	.te
	!	(01) Product [Number]
	!	.te
	!	(02) Quantity Ticketed
	!	.te
	!	(03) Quantity to Ship
	!	.te
	!	(04) Unit Price
	!	.te
	!	(05) Discount %
	!	.te
	!	(06) Unit Cost
	!	.te
	!	(07) Request Date
	!	.end table
	!	There are two fields which are calculated by the system that also appear
	!	on the screen.  They are:
	!	.table 3,25
	!	.te
	!	Quantity on BackTicket
	!	.te
	!	Extended Price
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Line Item>Ticket Entry Journal
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD01
	!	.x Sort by
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\*
	!	field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*D\* - Document Number
	!	.te
	!	^*T\* - Sales Type
	!	.te
	!	^*N\* - Customer Number
	!	.te
	!	^*C\* - Order Category
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--
