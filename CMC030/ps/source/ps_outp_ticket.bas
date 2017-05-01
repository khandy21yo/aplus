1	%TITLE "Print Ticket Form from the Ticket Journal"
	%SBTTL "PS_OUTP_TICKET"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PS_OUTP_TICKET(STRING TICKET, STRING BATCH, LONG FLAG)

	!
	! COPYRIGHT (C) 1991 BY
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
	! ID:PSTICK
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints a ticket form for the POS Ticket Journal
	!	from a print request, in the command menu, in the journal entry process.
	!	.lm -5
	!
	! Index:
	!	.x Print>Ticket Form
	!	.x Ticket Form
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PS_SOURCE:PS_OUTP_TICKET/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN PS_OUTP_TICKET
	!	$ DELETE PS_OUTP_TICKET.OBJ;*
	!
	! Author:
	!
	!	11/04/91 - Frank F. Starman
	!
	! Modification history:
	!
	!	02/05/92 - Kevin Handy
	!		Cleaned out junk (check)
	!
	!	02/24/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	02/25/92 - Kevin Handy
	!		Modified to use OUTP_NEWPAGE to skip to the bottom
	!		of the-form.
	!
	!	03/10/92 - Kevin Handy
	!		Modified to use OUTP_INITIALIZE when not using
	!		OUTP_SETTINGS.
	!
	!	03/12/92 - Kevin Handy
	!		Cleaned up (check)
	!
	!	04/08/92 - Frank F. Starman
	!		Remove OUTP_INITIALIZE, because it didn't work.
	!		Inform OUTP_SETTINGS about FORM_DIRECT.
	!
	!	04/16/92 - Frank F. Starman
	!		Added OE_CREASON file.
	!
	!	04/22/92 - Frank F. Starman
	!		Added OE_CATEGORY and OE_ORDERTYPE files.
	!
	!	04/28/92 - Kevin Handy
	!		Clean up (check)
	!
	!	06/12/92 - Kevin Handy
	!		Clean up (check)
	!
	!	09/04/92 - Kevin Handy
	!		Changed sub FORM_LOADVAR to PS_OUTP_TICKET_LOADVAR,
	!		due to Frank's not believing that you cannot have
	!		two functions in library with the exact same name.
	!
	!	10/29/92 - Dan Perkins
	!		Added IC_BINMAP junk so we can get the bin map
	!		location.
	!
	!	01/29/93 - Dan Perkins
	!		Added ability to print PICK lists from this function.
	!
	!	02/04/93 - Dan Perkins
	!		Modified program to accomodate changes in file layouts.
	!
	!	03/03/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/30/93 - Dan Perkins
	!		Fixed problem at line 18660.  Getting MO_ORDERLINE
	!		quantities instead of MO_ORDERLINEOPT quantities.
	!
	!	04/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/04/93 - Dan Perkins
	!		Reversed CUSTOM_NUM$ and TICKET lines so we find
	!		the customer number before dropping it by playing
	!		with TICKET.
	!
	!	06/17/93 - Dan Perkins
	!		Separated MO_ORDERLINE notes and OE_ORDERLINE notes
	!		so a second line is not printed if there is no second
	!		note.
	!
	!	06/20/93 - Frank F. Starman
	!		Calculate due days based on the UTL_TERMS file.
	!
	!	06/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/23/93 - Kevin Handy
	!		Modified to check for need of paging more often
	!		due to too many forms overflowing the page
	!		at Robisons.
	!
	!	06/24/93 - Frank F. Starman
	!		Added TIMESLEEP% variable.
	!
	!	07/21/93 - Frank F. Starman
	!		Added IDNUM field.
	!
	!	08/30/93 - Frank F. Starman
	!		Added SUNSHIPQTY variable.
	!
	!	10/04/93 - Kevin Handy
	!		Changed "=>" to ">=".
	!
	!	06/17/94 - Kevin Handy
	!		Added code foe ::MISCH2.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	04/26/95 - Kevin Handy
	!		Change SMG_BLANK to SMG_BLANK%.
	!
	!	05/31/95 - Kevin Handy
	!		Lose unused variables LINE_DISC, SHIP_NET, and
	!		DISCOUNT. They were assigned but never used.
	!		Lose unecessary externals.
	!
	!	06/01/95 - Kevin Handy
	!		Fixed calculation of sales tax to calculate
	!		ORDER_SDISC, and SHIP_SDISC which were never
	!		calculated.
	!
	!	06/01/95 - Kevin Handy
	!		Modified so that shipping date is not
	!		forced to todays date if it is blank.
	!
	!	06/01/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	06/01/95 - Kevin Handy
	!		Make line 18295 a complete subroutine, so that
	!		is wasn't used as a subroutine some places, and
	!		fell through from 18290.
	!
	!	06/01/95 - Kevin Handy
	!		Modified to handle miscelanous when it is not
	!		sales taxable.
	!
	!	06/02/95 - Kevin Handy
	!		Modified to handle non-state sales taxable
	!		product categories.
	!
	!	06/29/95 - Kevin Handy
	!		Modifications to use OUTP_INITFORM instead of the
	!		massive chunk of code that was slapped into the
	!		source.
	!
	!	07/03/95 - Kevin Handy
	!		Force user to select Combine Orders option
	!		each time.
	!
	!	07/11/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/08/95 - Kevin Handy
	!		Modifications for file layout changes re
	!		OE_REGLINE::NOTES, OE_ORDERLINE::NOTES,
	!		OE_REGLINE::SUBACCT, OE_ORDERLINE::SUBACCT.
	!
	!	01/29/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in
	!		several places.
	!
	!	05/07/96 - Kevin Handy
	!		Reformat source code.
	!
	!	10/25/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	11/21/96 - Kevin Handy
	!		Round discount amounts to 2 digits instead of
	!		three, so that this matches the posting.
	!
	!	05/09/97 - Kevin Handy
	!		Lose pop of SMG_BLANK% display
	!		Lose commented out code.
	!
	!	08/26/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	09/09/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/16/98 - Kevin Handy
	!		Added ::DEPOSIT fild.
	!
	!	12/21/98 - Kevin Handy
	!		Free the OE_ORDERLINE.TMP% channel that is
	!		allocated sometimes.
	!
	!	12/21/98 - Kevin Handy
	!		Leave PS_CASHREG open all the time, to see if
	!		it will help slow down the process crashing.
	!
	!	12/07/99 - Kevin Handy
	!		Add UTL_LOCATION fields to available to select
	!		in the-form.
	!
	!	01/20/2000 - Kevin Handy
	!		Work on the salesman name problems.
	!
	!	10/12/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	11/03/2000 - Kevin Handy
	!		Use A"x"B
	!
	!	10/03/2001 - Kevin Handy
	!		Wrap SCOPE::PRG_PROGRAM so correct help messages
	!		are displayed.
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
	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR
	DECLARE			OE_ORDERJOUR_CDD	OE_ORDERJOUR_NEW, &
							OE_ORDERJOUR_OLD

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.HB"
	MAP (OE_ORDERLINE)	OE_ORDERLINE_CDD	OE_ORDERLINE
	DECLARE OE_ORDERLINE_CDD	OE_ORDERLINE_TEMP

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.HB"
	MAP (MO_ORDERLINE)	MO_ORDERLINE_CDD	MO_ORDERLINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.HB"
	MAP (MO_ORDERLINEOPT)	MO_ORDERLINEOPT_CDD	MO_ORDERLINEOPT

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	COM (AR_35CUSTOM_EXAM)	AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[MO.OPEN]MO_OPTION.HB"
	MAP (MO_OPTION)		MO_OPTION_CDD		MO_OPTION

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKETYPE.HB"
	MAP (MO_MAKETYPE)	MO_MAKETYPE_CDD		MO_MAKETYPE

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKESIZE.HB"
	MAP (MO_MAKESIZE)	MO_MAKESIZE_CDD		MO_MAKESIZE

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODELCODE.HB"
	MAP (MO_MODELCODE)	MO_MODELCODE_CDD	MO_MODELCODE

	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.HB"
	DECLARE			MO_REGLINE_CDD		MO_REGLINE_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_CATEGORY.HB"
	MAP (OE_CATEGORY)	OE_CATEGORY_CDD		OE_CATEGORY

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREASON.HB"
	MAP (OE_CREASON)	OE_CREASON_CDD		OE_CREASON

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERTYPE.HB"
	MAP (OE_ORDERTYPE)	OE_ORDERTYPE_CDD	OE_ORDERTYPE

	%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.HB"
	COM (OE_PROMO_READ)	OE_PROMO_CDD		OE_PROMO_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	DECLARE			OE_REGLINE_CDD		OE_REGLINE_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPTO.HB"
	MAP (OE_SHIPTO)		OE_SHIPTO_CDD		OE_SHIPTO

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	COM (PD_PRODUCT_EXAM)	PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[PS.OPEN]PS_CASHREG.HB"
	MAP (PS_CASHREG)	PS_CASHREG_CDD		PS_CASHREG

	%INCLUDE "SOURCE:[PS.OPEN]PS_CONTROL.HB"
	MAP (PS_CONTROL)	PS_CONTROL_CDD		PS_CONTROL

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.HB"
	MAP (UTL_CARRIER)	UTL_CARRIER_CDD		UTL_CARRIER

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	COM (UTL_LOCATION_EXAM)	UTL_LOCATION_CDD	UTL_LOCATION_EXAM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS)		UTL_TERMS_CDD		UTL_TERMS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT)	UTL_REPORT_CDD		UTL_REPORT
	DECLARE			UTL_REPORT_CDD		UTL_REPORT_SYS

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM			FORM_GROUP_CDD		FORM_GROUP(15%)

	MAP (JOUR_FORM) &
		OE_ORDERJOUR.ADDLINE$(4%) = 50%, &
		AR_CUSTOM.ADDLINE$(4%) = 50%, &
		UTL_LOCATION_EXAM.ADDLINE$(4%) = 50%, &
		LIST_PRICE, &
		REAL_PRICE, &
		BALANCE, &
		ORIGORD_QTY, &
		ORD_QTY, &
		SUMSHPQTY, &
		ORDERLINE, &
		ORDER_TOTAL, &
		ORDER_DISC, &
		ORDER_TAX, &
		SHIP_QTY, &
		SHIPLINE, &
		SHIP_TOTAL, &
		SHIP_DISC, &
		SHIP_TAX, &
		XOUTASSIGN$ = 30%, &
		PAGE_NUMBER%, &
		LINE_NUM%, &
		DUEDAYS$(10%) = 14%, &
		NOTENUMBER%, &
		ORDER_NONSALTAX

	COM (CH_IC_BINMAP)		IC_BINMAP.CH%

	COM (CH_MO_MAKESIZE)		MO_MAKESIZE.CH%
	COM (CH_MO_MAKETYPE)		MO_MAKETYPE.CH%
	COM (CH_MO_MODELCODE)		MO_MODELCODE.CH%
	COM (CH_MO_OPTION)		MO_OPTION.CH%
	COM (CH_MO_ORDERLINE)		MO_ORDERLINE.CH%
	COM (CH_MO_ORDERLINEOPT)	MO_ORDERLINEOPT.CH%

	COM (CH_OE_CATEGORY)		OE_CATEGORY.CH%
	COM (CH_OE_CREASON)		OE_CREASON.CH%
	COM (CH_OE_ORDERLINE)		OE_ORDERLINE.CH%
	COM (CH_OE_ORDERTYPE)		OE_ORDERTYPE.CH%
	COM (CH_OE_SHIPTO)		OE_SHIPTO.CH%

	COM (CH_PS_CONTROL)		PS_CONTROL.CH%
	COM (CH_PS_CASHREG)		PS_CASHREG.CH%

	COM (CH_UTL_CARRIER)		UTL_CARRIER.CH%
	COM (CH_UTL_TERMS)		UTL_TERMS.CH%

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION AR_EXAM_CUSTOM
	EXTERNAL LONG   FUNCTION MO_READ_REGLINE
	EXTERNAL LONG   FUNCTION OE_READ_PROMO
	EXTERNAL LONG   FUNCTION OE_READ_REGLINE
	EXTERNAL LONG	FUNCTION OUTP_INITFORM
	EXTERNAL LONG   FUNCTION OUTP_FORMINIT
	EXTERNAL LONG   FUNCTION OUTP_FORMPRINT
	EXTERNAL LONG   FUNCTION PD_EXAM_PRODUCT
	EXTERNAL LONG   FUNCTION SB_EXAM_SUBACCOUNT
	EXTERNAL LONG   FUNCTION UTL_EXAM_LOCATION

	EXTERNAL REAL   FUNCTION PC_READ_PRICE

	!
	! Declare variables
	!
	DECLARE STRING INV_ARRAY(50%)

	%PAGE

	ON ERROR GOTO 19000

	CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

	!*******************************************************************
	! Initilize maintainence
	!*******************************************************************

	SELECT FLAG

	CASE 0%, 1%
		REPORT$ = "PSTICK"

	CASE 2%, 3%
		REPORT$ = "PSPICK"

	CASE 4%, 5%
		REPORT$ = "PSORDR"

	END SELECT

	!
	! Look up device
	!
	CALL READ_DEVICE("PS_FORM", PS_FORM.DEV$, STAT%)

	!***************************************************************
	! Open Report files
	!***************************************************************

	FIXSET$ = "03"

	CUSTOM_NUM$ = RIGHT(TICKET, LEN(OE_ORDERJOUR::ORDNUM) + 1%)
	TICKET = CONV_STRING(LEFT(TICKET, LEN(OE_ORDERJOUR::ORDNUM)), CMC$_LEFT)

	IF FLAG >= 4%
	THEN
		FIXSET$ = FIXSET$ + ",05N,06N"
	END IF


	FIXSET$ = FIXSET$ + ",00D,01" + &
		TICKET + ",02" + &
		TICKET + ",06N"

	!
	! Ask user to change settings
	!
	IF FLAG AND 1%
	THEN
		FIXSET$ = FIXSET$ + ",DIRECT"
	END IF

370	!
	! Get Report
	!
	TEMP_PRG$ = SCOPE::PRG_PROGRAM
	SCOPE::PRG_PROGRAM = "PS_OUTP_TICKET"

	IF OUTP_INITFORM(UTL_REPORTX, REPORT$, FIXSET$) <> CMC$_NORMAL
	THEN
		SCOPE::PRG_PROGRAM = TEMP_PRG$
		GOTO ExitProgram
	END IF

	SCOPE::PRG_PROGRAM = TEMP_PRG$

	!
	! Plug the passed order number and batch number into the report file
	! also default the sortby sequence to Order number "O"
	!
	BATCH_NO$ = BATCH

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	.x Ticket Form>Sort by
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field enters the order
	!	in which the report is to print.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*D\* - Document Number
	!	.te
	!	^*T\* - Sales Type
	!	.te
	!	^*N\* - Customer Number
	!	.te
	!	^*C\* - Sales Category
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the
	!	printing to begin with a particular
	!	item. The value must be in agreement with the value
	!	entered in field (01) Sort by.
	!	.b
	!	A blank field will cause the statement to start with
	!	the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x Ticket Form>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the
	!	printing to end with a particular
	!	item in the file. The value must be in agreement with
	!	field (01) Sort by.
	!	.b
	!	A blank field causes the statement to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x Ticket Form>To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated
	!	items to be printed by entering a "wildcard" for Wildcarding Technique.
	!	.lm -5
	!
	! Index:
	!
	!--

	REPORT$ = REPORT$ + "$" + TRM$(UTL_REPORTX::OPTDEF(4%))
	FORM$ = TRM$(UTL_REPORTX::OPTDEF(4%))

	!++
	! Abstract:FLD05
	!	^*(05) Form\*
	!	.b
	!	.lm +5
	!	The ^*Forms\* option enters the form number for
	!	printing.  See Forms Controlling under Utility Section.
	!	.lm -5
	!
	! Index:
	!
	!--

	PRINT_BACK$ = TRM$(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) Print Backorders\*
	!	.b
	!	.lm +5
	!	The ^*Print Backorders\* option indicates if
	!	the backorder line is to be printed on the invoice form.
	!	.b
	!	Valid entries are:
	!	.table 3,25
	!	.te
	!	^*Y\* = Yes
	!	.te
	!	^*N\* = No
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	!--

	COMBINE_ORDERS$ = TRM$(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*(07) Combine Customer Orders\*
	!	.b
	!	.lm +5
	!	The ^*Combine Customer Orders\* optionally
	!	combines more than one customer order into a single invoice.
	!	If "Y" is selected for this option, the forms will be sorted
	!	according to customer number. All header information for
	!	subsequent orders will be the same as the
	!	header to which the orders are appended.
	!	.lm -5
	!
	! Index:
	!
	!--

	SORTBY$ = "N" IF COMBINE_ORDERS$ = "Y"

	SELECT SORTBY$

	CASE "D"
		K_NUM% = 0%
		FROM_ITEM$ = SPACE$(LEN(TICKET) - LEN(FROM_ITEM$)) + FROM_ITEM$
		TO_ITEM$ = SPACE$(LEN(TICKET) - LEN(TO_ITEM$)) + TO_ITEM$

	CASE "T"
		K_NUM% = 1%

	CASE "N"
		K_NUM% = 2%

	CASE "C"
		K_NUM% = 3%

	END SELECT

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!***************************************************************
	! Open all other files
	!***************************************************************

600	CALL ASSG_CHANNEL(OE_ORDERLINE.TMP%, STAT%)
	OPEN_FLAG% = 0%

	!
	! Open order journal header file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.CRE"
	USE
		IF ERR = 138%	! Locked FILE
		THEN
			SLEEP 5%
			RETRY
		END IF

		FILENAME$ = "OE_ORDERJOUR_" + BATCH_NO$
		CONTINUE HelpError
	END WHEN

605	!
	! Open control file
	!
	IF PS_CONTROL.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PS.OPEN]PS_CONTROL.CRE"
		USE
			IF ERR = 138%	! Locked FILE
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "PS_CONTROL"
			CONTINUE HelpError
		END WHEN
	END IF

	WHEN ERROR IN
		GET #PS_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "PS_CONTROL"
		CONTINUE HelpError
	END WHEN

610	!
	! Open order journal line file
	!
	IF OE_ORDERLINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.OPN"
		USE
			IF ERR = 138%	! Locked FILE
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 615 IF ERR = 5%
			FILENAME$ = "OE_ORDERLINE_" + BATCH_NO$
			CONTINUE HelpError
		END WHEN
	END IF

615	!
	! Open order category file
	!
	IF OE_CATEGORY.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_CATEGORY.CRE"
		USE
			IF ERR = 138%	! Locked FILE
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 620 IF ERR = 5%
			FILENAME$ = "OE_CATEGORY"
			CONTINUE HelpError
		END WHEN
	END IF

620	!
	! Open order type file
	!
	IF OE_ORDERTYPE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERTYPE.CRE"
		USE
			IF ERR = 138%	! Locked FILE
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 625 IF ERR = 5%
			FILENAME$ = "OE_ORDERTYPE"
			CONTINUE HelpError
		END WHEN
	END IF

625	!
	! Open cash register file
	!
	IF PS_CASHREG.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PS.OPEN]PS_CASHREG.CRE"
		USE
			IF ERR = 138%	! Locked FILE
			THEN
				SLEEP 5%
				RETRY
			END IF

			FILENAME$ = "PS_CASHREG"
			CONTINUE HelpError
		END WHEN
	END IF

	UNDER% = INSTR(1%, BATCH, "_")

 GetCash:
	WHEN ERROR IN
		GET #PS_CASHREG.CH%, &
			KEY #0% EQ LEFT(BATCH, UNDER% - 1%)
		UNLOCK #PS_CASHREG.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 630 IF ERR = 155%
		FILENAME$ = "PS_CASHREG"
		CONTINUE HelpError
	END WHEN

	GOTO 635

630	PS_CASHREG::CASHREG     = LEFT(BATCH, UNDER% - 1%)
	PS_CASHREG::DESCR       = ""
	PS_CASHREG::LAST_INVNUM = "0"
	PS_CASHREG::LOCATION    = ""
	PS_CASHREG::NOTES(0%)   = ""
	PS_CASHREG::NOTES(1%)   = ""

	WHEN ERROR IN
		PUT #PS_CASHREG.CH%
	USE
		FILENAME$ = "PS_CASHREG"
		CONTINUE HelpError
	END WHEN

 !	UNLOCK #PS_CASHREG.CH%

	GOTO GetCash

635	!
	! Open reason code file
	!
	IF OE_CREASON.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_CREASON.CRE"
		USE
			IF ERR = 138%	! Locked FILE
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 640 IF ERR = 5%
			FILENAME$ = "OE_CREASON"
			CONTINUE HelpError
		END WHEN
	END IF

640	!
	! Open carrier file
	!
	IF UTL_CARRIER.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.CRE"
		USE
			IF ERR = 138%	! Locked FILE
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 645 IF ERR = 5%
			FILENAME$ = "UTL_CARRIER"
			CONTINUE HelpError
		END WHEN
	END IF

645	!
	! Open shipto file
	!
	IF OE_SHIPTO.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPTO.CRE"
		USE
			IF ERR = 138%	! Locked FILE
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 650 IF ERR = 5%
			FILENAME$ = "OE_SHIPTO"
			CONTINUE HelpError
		END WHEN
	END IF

650	!
	! Open accounts receivable customer file
	!
	IF UTL_TERMS.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.CRE"
		USE
			IF ERR = 138%	! Locked FILE
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 655 IF ERR = 5%
			FILENAME$ = "UTL_TERMS"
			CONTINUE HelpError
		END WHEN
	END IF

655	!
	! Open binmap file
	!
	IF IC_BINMAP.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.OPN"
		USE
			IF ERR = 138%	! Locked FILE
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 660 IF ERR = 5%
			FILENAME$ = "IC_BINMAP"
			CONTINUE HelpError
		END WHEN
	END IF

660	!
	! Open MO_ORDERLINE file
	!
	IF MO_ORDERLINE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.OPN"
		USE
			IF ERR = 138%	! Locked FILE
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 665 IF ERR = 5%
			FILENAME$ = "MO_ORDERLINE_" + BATCH_NO$
			CONTINUE HelpError
		END WHEN
	END IF

665	!
	! Open MO_ORDERLINEOPT file
	!
	IF MO_ORDERLINEOPT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.OPN"
		USE
			IF ERR = 138%	! Locked FILE
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 670 IF ERR = 5%
			FILENAME$ = "MO_ORDERLINEOPT_" + BATCH_NO$
			CONTINUE HelpError
		END WHEN
	END IF

670	!
	! Open Make Size Desciption file
	!
	IF MO_MAKESIZE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_MAKESIZE.OPN"
		USE
			IF ERR = 138%	! Locked FILE
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 675 IF ERR = 5%
			FILENAME$ = "MO_MAKESIZE"
			CONTINUE HelpError
		END WHEN
	END IF

675	!
	! Open Make type Desciption file
	!
	IF MO_MAKETYPE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_MAKETYPE.OPN"
		USE
			IF ERR = 138%	! Locked FILE
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 680 IF ERR = 5%
			FILENAME$ = "MO_MAKETYPE"
			CONTINUE HelpError
		END WHEN
	END IF

680	!
	! Open Model Code Desciption file
	!
	IF MO_MODELCODE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_MODELCODE.OPN"
		USE
			IF ERR = 138%	! Locked FILE
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 685 IF ERR = 5%
			FILENAME$ = "MO_MODELCODE"
			CONTINUE HelpError
		END WHEN
	END IF

685	!
	! Open Option file
	!
	IF MO_OPTION.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[MO.OPEN]MO_OPTION.OPN"
		USE
			IF ERR = 138%	! Locked FILE
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE EndOpen IF ERR = 5%
			FILENAME$ = "MO_OPTION"
			CONTINUE HelpError
		END WHEN
	END IF

 EndOpen:
	!
	! Restore original values for the help message
	!
	SCOPE::PRG_IDENT = TEMP_IDENT$
	SCOPE::PRG_PROGRAM = TEMP_PROGRAM$

	!
	! Load In The Form
	!
	GOSUB LoadForm

	!
	! Go check out aligment routine
	!
	GOSUB Alignment IF (FLAG AND 1%) = 0%

	%PAGE

2000	!*******************************************************************
	! Read through header file
	!*******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #OE_ORDERJOUR.CH%, KEY #K_NUM%
		ELSE
			FIND #OE_ORDERJOUR.CH%, KEY #K_NUM% GE FROM_ITEM$
		END IF
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 3000 IF ERR = 155%
		FILENAME$ = "OE_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
2010	!
	! Get next record
	!
	WHEN ERROR IN
		GET #OE_ORDERJOUR.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 3000 IF ERR = 11%
		FILENAME$ = "OE_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

	IF COMBINE_ORDERS$ = "Y"
	THEN
		IF OE_ORDERJOUR::CUSNUM <> TESTCUSNUM$ AND TESTCUSNUM$ <> ""
		THEN
			!
			! Finish bottom of the form
			!
			OE_ORDERJOUR_NEW = OE_ORDERJOUR
			OE_ORDERJOUR     = OE_ORDERJOUR_OLD

			GOSUB PrintCombineLines
			GOSUB 18295

			OE_ORDERJOUR  = OE_ORDERJOUR_NEW
			TESTCUSNUM$   = ""
			TEMP_CREASON$ = ""
			TEMP_SHIPVIA$ = ""
			TEMP_TERMS$   = ""

			IF FLAG_INVOICE%
			THEN
				WHEN ERROR IN
					FIND #OE_ORDERJOUR.CH%, &
						KEY #2% EQ OE_ORDERJOUR_OLD::CUSNUM, &
						REGARDLESS
				USE
					FILENAME$ = "OE_ORDERJOUR"
					CONTINUE HelpError
				END WHEN

				FLAG_INVOICE%        = 0%
				LOOP_INV%            = LOOP_INV% + 1%
				INV_ARRAY(LOOP_INV%) = TESTINVOICE$

				GOTO GetNextRec
			END IF

			LOOP_INV% = 0%
		END IF

	END IF

	!
	! Check for end item
	!
	SELECT SORTBY$

	CASE "D"
		GOTO 3000 IF (OE_ORDERJOUR::ORDNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(OE_ORDERJOUR::ORDNUM, -1%), &
			WLDCRD$) = 0%

	CASE "T"
		GOTO 3000 IF (OE_ORDERJOUR::ORDTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(OE_ORDERJOUR::ORDTYPE, -1%), &
			WLDCRD$) = 0%

	CASE "C"
		GOTO 3000 IF (OE_ORDERJOUR::ORDCAT > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(OE_ORDERJOUR::ORDCAT, -1%), &
			WLDCRD$) = 0%

	CASE "N"
		GOTO 3000 IF (OE_ORDERJOUR::CUSNUM > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_ARRAY(EDIT$(OE_ORDERJOUR::CUSNUM, -1%), &
			WLDCRD$) = 0%

	END SELECT

 !	GOTO GetNextRec IF OE_ORDERJOUR::INVNUM <> ""

 !	TEST_KEY$ = OE_ORDERJOUR::ORDNUM

	IF COMBINE_ORDERS$ = "Y"
	THEN
		!
		! Test invoice array and go to GetNextRec if there
		! is already one in array
		!
		GOTO GetNextRec IF OE_ORDERJOUR::INVNUM = INV_ARRAY(I%) &
			FOR I% = 1% TO LOOP_INV%

		!
		! Test invoice number
		!
		IF OE_ORDERJOUR::INVNUM <> TESTINVOICE$ AND &
			TESTCUSNUM$ <> "" AND &
			OE_ORDERJOUR::INVNUM <> ""
		THEN
			FLAG_INVOICE% = -1%
			GOTO GetNextRec
		END IF

		!
		! See if we are on the same shipto address
		!
		IF TESTCUSNUM$ <> "" AND &
			EDIT$(OE_ORDERJOUR::SHIPNAM + OE_ORDERJOUR::ADD1 + &
			OE_ORDERJOUR::ADD2 + OE_ORDERJOUR::ADD3, -1%) <> &
			EDIT$(OE_ORDERJOUR_OLD::SHIPNAM + &
			OE_ORDERJOUR_OLD::ADD1 + &
			OE_ORDERJOUR_OLD::ADD2 + OE_ORDERJOUR_OLD::ADD3, -1%)
		THEN

	!++
	! Error:DIFFSHIP
	!	.ts 55
	!	.lm +5
	!	An error here means that different Ship To addresses exist
	!	for the indicated Customer.  The Ship To address must be the
	!	same for each customer order, in order to combine invoices.
	!	.lm -5
	!--
			CALL HELP_34MESSAGE(SCOPE, &
				"different ship to addresses for customer " + &
				OE_ORDERJOUR::CUSNUM + ", " + &
				OE_ORDERJOUR::ORDNUM, "E", &
				SCOPE::PRG_PROGRAM, "", "DIFFSHIP")

			GOTO ExitProgram
		END IF

		!
		! Test and set some ORDERJOUR fields
		!
		IF OE_ORDERJOUR::CREASON <> ""
		THEN
			TEMP_CREASON$ = OE_ORDERJOUR::CREASON
		ELSE
			OE_ORDERJOUR::CREASON = TEMP_CREASON$
		END IF

		IF OE_ORDERJOUR::SHIPVIA <> ""
		THEN
			TEMP_SHIPVIA$ = OE_ORDERJOUR::SHIPVIA
		ELSE
			OE_ORDERJOUR::SHIPVIA = TEMP_SHIPVIA$
		END IF

		IF OE_ORDERJOUR::TERMS <> ""
		THEN
			TEMP_TERMS$ = OE_ORDERJOUR::TERMS
		ELSE
			OE_ORDERJOUR::TERMS = TEMP_TERMS$
		END IF
	END IF

	!
	! Create an shipto address line format that reduces white space
	!
	I% = 0%

	IF EDIT$(OE_ORDERJOUR::ADD1, -1%) <> ""
	THEN
		I% = I% + 1%
		OE_ORDERJOUR.ADDLINE$(I%) = &
			EDIT$(OE_ORDERJOUR::ADD1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(OE_ORDERJOUR::ADD2, -1%) <> ""
	THEN
		I% = I% + 1%
		OE_ORDERJOUR.ADDLINE$(I%) = &
			EDIT$(OE_ORDERJOUR::ADD2, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(OE_ORDERJOUR::ADD3, -1%) <> ""
	THEN
		I% = I% + 1%
		OE_ORDERJOUR.ADDLINE$(I%) = &
			EDIT$(OE_ORDERJOUR::ADD3, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%
	OE_ORDERJOUR.ADDLINE$(I%) = &
		EDIT$(EDIT$(OE_ORDERJOUR::CITY, 128%) + ", " + &
		OE_ORDERJOUR::STATE + " " + OE_ORDERJOUR::ZIP + " " + &
		OE_ORDERJOUR::COUNTRY, 8% + 16% + 32% + 128%)

	! Blank 'em out
	OE_ORDERJOUR.ADDLINE$(LOOP%) = "" FOR LOOP% = I% + 1% TO 4%

	!
	! Get the records that match in the tables
	!
	! Account Receivable Customer File
	!
	IF OE_ORDERJOUR::CUSNUM <> ""
	THEN
		V% = AR_EXAM_CUSTOM(OE_ORDERJOUR::CUSNUM, AR_35CUSTOM_EXAM)
	ELSE
 !		V% = UTL_EXAM_LOCATION(OE_ORDERJOUR::SHIPLIN, UTL_LOCATION_EXAM)

		AR_35CUSTOM_EXAM::CUSNUM  = OE_ORDERJOUR::SHIPLIN
		AR_35CUSTOM_EXAM::CUSNAM  = OE_ORDERJOUR::SHIPNAM
		AR_35CUSTOM_EXAM::ADD1    = OE_ORDERJOUR::ADD1
		AR_35CUSTOM_EXAM::ADD2    = OE_ORDERJOUR::ADD2
		AR_35CUSTOM_EXAM::ADD3    = OE_ORDERJOUR::ADD3
		AR_35CUSTOM_EXAM::CITY    = OE_ORDERJOUR::CITY
		AR_35CUSTOM_EXAM::STATE   = OE_ORDERJOUR::STATE
		AR_35CUSTOM_EXAM::ZIP     = OE_ORDERJOUR::ZIP
		AR_35CUSTOM_EXAM::COUNTRY = OE_ORDERJOUR::COUNTRY

	END IF

	!
	! Create an address line format that reduces white space
	!
	I% = 0%

	IF EDIT$(AR_35CUSTOM_EXAM::ADD1, -1%) <> ""
	THEN
		I% = I% + 1%
		AR_CUSTOM.ADDLINE$(I%) = &
			EDIT$(AR_35CUSTOM_EXAM::ADD1, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(AR_35CUSTOM_EXAM::ADD2, -1%) <> ""
	THEN
		I% = I% + 1%
		AR_CUSTOM.ADDLINE$(I%) = &
			EDIT$(AR_35CUSTOM_EXAM::ADD2, 8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(AR_35CUSTOM_EXAM::ADD3, -1%) <> ""
	THEN
		I% = I% + 1%
		AR_CUSTOM.ADDLINE$(I%) = &
			EDIT$(AR_35CUSTOM_EXAM::ADD3, 8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%
	AR_CUSTOM.ADDLINE$(I%) = &
		EDIT$(EDIT$(AR_35CUSTOM_EXAM::CITY, 128%) + ", " + &
		AR_35CUSTOM_EXAM::STATE + " " + AR_35CUSTOM_EXAM::ZIP + " " + &
		AR_35CUSTOM_EXAM::COUNTRY, 8% + 16% + 32% + 128%)

	! Blank 'em out
	AR_CUSTOM.ADDLINE$(LOOP%) = "" FOR LOOP% = I% + 1% TO 4%

	!
	! Find info about the first salesman
	!
	IF SB_EXAM_SUBACCOUNT("S", OE_ORDERJOUR::SALESMAN, &
		SA_SALESMAN) <> CMC$_NORMAL
	THEN
		SA_SALESMAN::DESCR = OE_ORDERJOUR::SALESMAN
	END IF

	!
	! Get the company information in case they want it for the header
	!
	V% = UTL_EXAM_LOCATION(OE_ORDERJOUR::LOCATION, UTL_LOCATION_EXAM)

	!
	! Create an shipto address line format that reduces white space
	!
	I% = 0%

	IF EDIT$(UTL_LOCATION_EXAM::ADDRESS1, -1%) <> ""
	THEN
		I% = I% + 1%
		UTL_LOCATION_EXAM.ADDLINE$(I%) = &
			EDIT$(UTL_LOCATION_EXAM::ADDRESS1, &
			8% + 16% + 32% + 128%)
	END IF

	IF EDIT$(UTL_LOCATION_EXAM::ADDRESS2, -1%) <> ""
	THEN
		I% = I% + 1%
		UTL_LOCATION_EXAM.ADDLINE$(I%) = &
			EDIT$(UTL_LOCATION_EXAM::ADDRESS2, &
			8% + 16% + 32% + 128%)
	END IF

	I% = I% + 1%
	UTL_LOCATION_EXAM.ADDLINE$(I%) = &
		EDIT$(EDIT$(UTL_LOCATION_EXAM::CITY, 128%) + ", " + &
		UTL_LOCATION_EXAM::STATE + " " + UTL_LOCATION_EXAM::ZIP, &
		8% + 16% + 32% + 128%)

	! Blank 'em out
	UTL_LOCATION_EXAM$(LOOP%) = "" FOR LOOP% = I% + 1% TO 4%


2105	!
	! Reason codes description file
	!
	WHEN ERROR IN
		GET #OE_CREASON.CH%, &
			KEY #0% EQ OE_ORDERJOUR::CREASON, &
			REGARDLESS
	USE
		OE_CREASON::DESCR = ""

		CONTINUE 2110 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "OE_CREASON"
		CONTINUE HelpError
	END WHEN

2110	!
	! Utility Carrier Description File
	!
	WHEN ERROR IN
		GET #UTL_CARRIER.CH%, &
			KEY #0% EQ OE_ORDERJOUR::SHIPVIA, &
			REGARDLESS
	USE
		UTL_CARRIER::DESCR = ""

		CONTINUE 2120 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "UTL_CARRIER"
		CONTINUE HelpError
	END WHEN

2120	!
	! Utility Terms Description File
	!
	DISCDAYS% = 30%
	DUEDAYS%  = 30%
	DUEDATE$  = ""
	DISCDATE$ = ""

	WHEN ERROR IN
		GET #UTL_TERMS.CH%, &
			KEY #0% EQ OE_ORDERJOUR::TERMS, &
			REGARDLESS
	USE
		UTL_TERMS::DESCR    = ""
		UTL_TERMS::DISCOUNT = 0.0

		CONTINUE 2130 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "UTL_TERMS"
		CONTINUE HelpError
	END WHEN

	IF EDIT$(UTL_TERMS::DUEDATE, -1%) <> ""
	THEN
		DUEDATE$  = UTL_TERMS::DUEDATE
		DUE_DATE$  = OE_ORDERJOUR::TRANDATE
	ELSE
		DUEDAYS%  = UTL_TERMS::DUEDAYS
	END IF

	IF EDIT$(UTL_TERMS::DISCOUNTDATE, -1%) <> ""
	THEN
		DISCDATE$ = UTL_TERMS::DISCOUNTDATE
		DISC_DATE$ = OE_ORDERJOUR::TRANDATE
	ELSE
		DISCDAYS% = UTL_TERMS::DISCOUNTDAYS
	END IF

2130	!
	! Shipto file
	!
	OE_SHIPTO::NOTES(I%) = "" &
		FOR I% = 0% TO 2%

	GOTO 2140 IF OE_ORDERJOUR::CUSNUM = ""

	WHEN ERROR IN
		GET #OE_SHIPTO.CH%, &
			KEY #0% EQ OE_ORDERJOUR::CUSNUM + &
			OE_ORDERJOUR::SHIPLIN, &
			REGARDLESS
	USE
		CONTINUE 2140 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "OE_SHIPTO"
		CONTINUE HelpError
	END WHEN

2140	!
	! Order category description file
	!
	WHEN ERROR IN
		GET #OE_CATEGORY.CH%, &
			KEY #0% EQ OE_ORDERJOUR::ORDCAT, &
			REGARDLESS
	USE
		OE_CATEGORY::DESCRIPTION = ""

		CONTINUE 2145 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "OE_CATEGORY"
		CONTINUE HelpError
	END WHEN

2145	!
	! Order type description file
	!
	WHEN ERROR IN
		GET #OE_ORDERTYPE.CH%, &
			KEY #0% EQ OE_ORDERJOUR::ORDTYPE, &
			REGARDLESS
	USE
		OE_ORDERTYPE::DESCRIPTION = ""

		CONTINUE 2150 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "OE_ORDERTYPE"
		CONTINUE HelpError
	END WHEN

2150	!
	! Set up for Xing out pre-printed invoice numbers on form
	!
	XOUTASSIGN$ = ""

	IF OE_ORDERJOUR::INVNUM = "" AND FLAG < 2%
	THEN
		IF COMBINE_ORDERS$ = "N" OR OE_ORDERJOUR::CUSNUM <> TESTCUSNUM$
		THEN
			INVOICE% = -1%
			V% = FUNC_INCREMENT(PS_CASHREG::LAST_INVNUM)
		END IF

		INVOICE$ = PS_CASHREG::LAST_INVNUM

		!
		! Put the last invoice number into the CASHREG file.
		!
		GET #PS_CASHREG.CH%, &
			KEY #0% EQ LEFT(BATCH, UNDER% - 1%)

		PS_CASHREG::LAST_INVNUM = INVOICE$

		UPDATE #PS_CASHREG.CH%
		UNLOCK #PS_CASHREG.CH%

		!
		! Handle invoice number here and post back into the
		!
		OE_ORDERJOUR::INVNUM = PS_CASHREG::LAST_INVNUM

		UPDATE #OE_ORDERJOUR.CH%
		UNLOCK #OE_ORDERJOUR.CH%
	ELSE
		INVOICE% = 0%

		OE_ORDERJOUR::TRANDATE = OE_ORDERJOUR::SHIPDATE &
			IF OE_ORDERJOUR::TRANDATE = ""

		OE_ORDERJOUR::INVNUM = CONV_STRING(OE_ORDERJOUR::ORDNUM, &
			CMC$_LEFT) &
			IF OE_ORDERJOUR::INVNUM = ""
	END IF

	DUEDAYS$(PAY%) = "" FOR PAY% = 1% TO 10%

	FOR PAY% = 1% TO OE_ORDERJOUR::PAYMNT

		IF DUEDATE$ = ""
		THEN
			DUE_DATE$ = DATE_INVDCODE(DATE_DAYCODE( &
				OE_ORDERJOUR::TRANDATE) + DUEDAYS% * PAY%)
		ELSE
			MON% = VAL%(MID(DUE_DATE$, 5%, 2%)) + 1%
			IF MON% = 13%
			THEN
				DUE_DATE$ = FORMAT$(VAL%( &
					LEFT(DUE_DATE$, 4%)) + 1%, "<0>###") + &
					"01" + DUEDATE$
			ELSE
				DUE_DATE$ = LEFT(DUE_DATE$, 4%) + &
					FORMAT$(MON%, "<0>#") + DUEDATE$
			END IF
		END IF

		DUEDAYS$(PAY%) = NUM1$(PAY%) + " Due " + &
			PRNT_DATE(DUE_DATE$, 6%)

	NEXT PAY%

	DUEDAYS$(1%) = "" IF OE_ORDERJOUR::PAYMNT = 1%

	GOSUB PrintStmt

	TESTCUSNUM$  = OE_ORDERJOUR::CUSNUM
	TESTINVOICE$ = OE_ORDERJOUR::INVNUM

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Go get the next  record
	!
	GOTO GetNextRec

3000	!*******************************************************************
	! Found the end of the order journal file
	!*******************************************************************

	!
	! See if we need to print the bottom of the form
	!
	IF COMBINE_ORDERS$ = "Y" AND TESTCUSNUM$ <> ""
	THEN
		!
		! Finish bottom of the form
		!
		OE_ORDERJOUR = OE_ORDERJOUR_OLD

		GOSUB PrintCombineLines
		GOSUB 18295

		TESTCUSNUM$   = ""
		TEMP_CREASON$ = ""
		TEMP_SHIPVIA$ = ""
		TEMP_TERMS$   = ""

		IF FLAG_INVOICE%
		THEN
			WHEN ERROR IN
				FIND #OE_ORDERJOUR.CH%, &
					KEY #2% EQ OE_ORDERJOUR_OLD::CUSNUM, &
					REGARDLESS
			USE
				FILENAME$ = "OE_ORDERJOUR"
				CONTINUE HelpError
			END WHEN

			FLAG_INVOICE%        = 0%
			LOOP_INV%            = LOOP_INV% + 1%
			INV_ARRAY(LOOP_INV%) = TESTINVOICE$

			GOTO GetNextRec
		END IF

		LOOP_INV% = 0%
	END IF

	%PAGE

 ExitProgram:
4000	!******************************************************************
	! Exit the program
	!******************************************************************
	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

	CLOSE OE_ORDERJOUR.CH%
	CALL ASSG_FREECHANNEL(OE_ORDERJOUR.CH%)

 !	CLOSE PS_CASHREG.CH%
 !	CALL ASSG_FREECHANNEL(PS_CASHREG.CH%)

	CALL ASSG_FREECHANNEL(OE_ORDERLINE.TMP%)

	EXIT FUNCTION

	%PAGE

 PrintStmt:
18000	!***************************************************************
	! Print the FORM now
	!***************************************************************

	!
	! If we are on a subsequent order we don't need to print the header
	!
	IF COMBINE_ORDERS$ = "Y"
	THEN
 !		OE_ORDERJOUR::ORDNUM = ""

		GOTO 18100 IF OE_ORDERJOUR::CUSNUM = TESTCUSNUM$
	END IF

	LINE_COUNT%  = 0%
	BODY_COUNT%  = 0%
	PAGE_NUMBER% = 1%

	ORD_QTY, SHIP_QTY = 0.0
	ORDER_TOTAL, SHIP_TOTAL = 0.0
	ORDER_DISC, SHIP_DISC = 0.0
	ORDER_TAX, SHIP_TAX = 0.0
	SUMSHPQTY = 0.0

	!
	! Print the top of statement
	!
	LINE_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

18100	!
	! Play the game with the MO stuff if there are MO lines.
	!
	REGLIN$ = "    "
	LINE_NUM% = 0%
	SHIP_STOTAL, ORDER_STOTAL = 0.0
	SHIP_NONSALTAX, ORDER_NONSALTAX = 0.0

	!
	! Check MO_REGLINE file for any lines
	!
 ReadMORegLine:
	GOTO 18150 IF MO_READ_REGLINE(OE_ORDERJOUR::ORDNUM, REGLIN$, "GT", &
		MO_REGLINE_READ, QTY()) <> CMC$_NORMAL

	REGLIN$   = MO_REGLINE_READ::LIN
	LINE_NUM% = VAL%(MO_REGLINE_READ::LIN)

	WHEN ERROR IN
		GET #MO_ORDERLINE.CH%, &
			KEY #0% EQ OE_ORDERJOUR::ORDNUM + MO_REGLINE_READ::LIN, &
			REGARDLESS
	USE
		CONTINUE TestMOBackOrder IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	BALANCE = QTY(1%) - QTY(2%) - QTY(3%) - &
		MO_ORDERLINE::SHPQTY

	BALANCE = 0.0 IF BALANCE <= 0.0

	ORIGORD_QTY = QTY(1%)
	ORD_QTY = QTY(1%) - QTY(2%) - QTY(3%) - ABS(MO_ORDERLINE::BCKQTY)

	ORD_QTY = 0.0 IF ORD_QTY <= 0.0

	GOSUB DumpMOLines
	GOTO ReadMORegLine

 TestMOBackOrder:
	IF QTY(0%) <> 0.0 AND PRINT_BACK$ = "Y"
	THEN
		!
		! Set ORDERLINE variables equal to REGLINE variables
		!
		MO_ORDERLINE::ORDNUM	= MO_REGLINE_READ::ORDNUM
		MO_ORDERLINE::LIN	= MO_REGLINE_READ::LIN
		MO_ORDERLINE::MAKE	= MO_REGLINE_READ::MAKE
		MO_ORDERLINE::YEAR	= MO_REGLINE_READ::YEAR
		MO_ORDERLINE::MTYPE	= MO_REGLINE_READ::MTYPE
		MO_ORDERLINE::MSIZE	= MO_REGLINE_READ::MSIZE
		MO_ORDERLINE::MODELCODE	= MO_REGLINE_READ::MODELCODE
		MO_ORDERLINE::PRODUCT	= MO_REGLINE_READ::PRODUCT
		MO_ORDERLINE::ORDQTY	= QTY(1%)
		MO_ORDERLINE::PRICE     = MO_REGLINE_READ::PRICE
		MO_ORDERLINE::COST	= MO_REGLINE_READ::COST
		MO_ORDERLINE::REQDATE	= MO_REGLINE_READ::TDATE
		MO_ORDERLINE::SHPQTY	= 0.0
		MO_ORDERLINE::BCKQTY	= QTY(0%)
		MO_ORDERLINE::NOTES(0%)	= MO_REGLINE_READ::NOTES(0%)
		MO_ORDERLINE::NOTES(1%)	= MO_REGLINE_READ::NOTES(1%)
		MO_ORDERLINE::DISCOUNT  = MO_REGLINE_READ::DISCOUNT

		BALANCE = QTY(1%) - QTY(2%) - QTY(3%)

		BALANCE = 0.0 IF BALANCE <= 0.0

		ORIGORD_QTY = QTY(1%)
		ORD_QTY = QTY(1%)

		ORD_QTY = 0.0 IF ORD_QTY <= 0.0

		GOSUB DumpMOLines
	END IF

	GOTO ReadMORegLine

	!
	! Check the MO_ORDERLINE file for any new lines
	!
18150	WHEN ERROR IN
		FIND #MO_ORDERLINE.CH%, &
			KEY #0% EQ OE_ORDERJOUR::ORDNUM + "NEWL", &
			REGARDLESS
	USE
		CONTINUE 18200 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

 ReadMOOrderLine:
18160	WHEN ERROR IN
		GET #MO_ORDERLINE.CH%, REGARDLESS
	USE
		CONTINUE 18200 IF ERR = 11%
		FILENAME$ = "MO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	GOTO 18200 IF MO_ORDERLINE::ORDNUM <> OE_ORDERJOUR::ORDNUM

	LINE_NUM% = LINE_NUM% + 1%

	BALANCE = MO_ORDERLINE::ORDQTY - MO_ORDERLINE::SHPQTY - &
		MO_ORDERLINE::BCKQTY

	BALANCE = 0.0 IF BALANCE <= 0.0

	ORIGORD_QTY = MO_ORDERLINE::ORDQTY

	ORD_QTY = MO_ORDERLINE::ORDQTY - MO_ORDERLINE::SHPQTY - &
		MO_ORDERLINE::BCKQTY

	ORD_QTY = 0.0 IF ORD_QTY <= 0.0

	GOSUB DumpMOLines
	GOTO ReadMOOrderLine

	!
	! Gonna play the games with the OE_ORDERLINE stuff, if any.
	!
18200	REGLIN$ = "    "
	LINE_NUM% = 0%

	!
	! Check OE_REGLINE file for any lines
	!
 ReadOERegLine:
	GOTO 18250 IF OE_READ_REGLINE(OE_ORDERJOUR::ORDNUM, REGLIN$, "GT", &
		OE_REGLINE_READ, QTY()) <> CMC$_NORMAL

	REGLIN$   = OE_REGLINE_READ::LIN
	LINE_NUM% = VAL%(OE_REGLINE_READ::LIN)

	WHEN ERROR IN
		GET #OE_ORDERLINE.CH%, &
			KEY #0% EQ OE_ORDERJOUR::ORDNUM + &
			OE_REGLINE_READ::LIN, &
			REGARDLESS
	USE
		CONTINUE TestOEBackOrder IF ERR = 155% OR ERR = 9%
		FILENAME$ = "OE_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	BALANCE = QTY(1%) - QTY(2%) - QTY(3%) - &
		OE_ORDERLINE::SHPQTY

	BALANCE = 0.0 IF BALANCE <= 0.0

	ORIGORD_QTY = QTY(1%)
	ORD_QTY = QTY(1%) - QTY(2%) - QTY(3%) - ABS(OE_ORDERLINE::BCKQTY)

	ORD_QTY = 0.0 IF ORD_QTY <= 0.0

	IF COMBINE_ORDERS$ = "Y"
	THEN
		GOSUB SummarizeLines
	ELSE
		GOSUB DumpOELines
	END IF

	GOTO ReadOERegLine

 TestOEBackOrder:
	IF QTY(0%) <> 0.0 AND PRINT_BACK$ = "Y"
	THEN
		!
		! Set ORDERLINE variables equal to REGLINE variables
		!
		OE_ORDERLINE::ORDNUM	= OE_REGLINE_READ::ORDNUM
		OE_ORDERLINE::PRODUCT	= OE_REGLINE_READ::PRODUCT
		OE_ORDERLINE::ORDQTY	= QTY(1%)
		OE_ORDERLINE::SHPQTY	= 0.0
		OE_ORDERLINE::PRICE     = OE_REGLINE_READ::PRICE
		OE_ORDERLINE::DISCOUNT  = OE_REGLINE_READ::DISCOUNT
		OE_ORDERLINE::COST	= OE_REGLINE_READ::COST
		OE_ORDERLINE::REQDATE	= OE_REGLINE_READ::TDATE
		OE_ORDERLINE::PROMO     = OE_REGLINE_READ::PROMO
		OE_ORDERLINE::MISCH	= OE_REGLINE_READ::MISCH
		OE_ORDERLINE::MISCH2	= OE_REGLINE_READ::MISCH2
		OE_ORDERLINE::BCKQTY	= QTY(0%)
		OE_ORDERLINE::NOTES1	= OE_REGLINE_READ::NOTES1
		OE_ORDERLINE::NOTES2	= OE_REGLINE_READ::NOTES2
		OE_ORDERLINE::SUBACCT	= OE_REGLINE_READ::SUBACCT
		OE_ORDERLINE::LIN	= OE_REGLINE_READ::LIN

		BALANCE = QTY(1%) - QTY(2%) - QTY(3%)

		BALANCE = 0.0 IF BALANCE <= 0.0

		ORIGORD_QTY = QTY(1%)
		ORD_QTY = QTY(1%)

		ORD_QTY = 0.0 IF ORD_QTY <= 0.0

		IF COMBINE_ORDERS$ = "Y"
		THEN
			GOSUB SummarizeLines
		ELSE
			GOSUB DumpOELines
		END IF
	END IF

	GOTO ReadOERegLine

	!
	! Check the ORDERLINE file for any new lines
	!
18250	WHEN ERROR IN
		FIND #OE_ORDERLINE.CH%, &
			KEY #0% EQ OE_ORDERJOUR::ORDNUM + "NEWL", &
			REGARDLESS
	USE
		CONTINUE 18290 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "OE_ORDERLINE"
		CONTINUE HelpError
	END WHEN

 ReadOEOrderLine:
18260	WHEN ERROR IN
		GET #OE_ORDERLINE.CH%, REGARDLESS
	USE
		CONTINUE 18290 IF ERR = 11%
		FILENAME$ = "OE_ORDERLINE"
		CONTINUE HelpError
	END WHEN

	GOTO 18290 IF OE_ORDERLINE::ORDNUM <> OE_ORDERJOUR::ORDNUM

	LINE_NUM% = LINE_NUM% + 1%

	BALANCE = OE_ORDERLINE::ORDQTY - OE_ORDERLINE::SHPQTY - &
		OE_ORDERLINE::BCKQTY

	BALANCE = 0.0 IF BALANCE <= 0.0

	ORIGORD_QTY = OE_ORDERLINE::ORDQTY

	ORD_QTY = OE_ORDERLINE::ORDQTY - OE_ORDERLINE::SHPQTY - &
		OE_ORDERLINE::BCKQTY

	ORD_QTY = 0.0 IF ORD_QTY <= 0.0

	IF COMBINE_ORDERS$ = "Y"
	THEN
		GOSUB SummarizeLines
	ELSE
		GOSUB DumpOELines
	END IF

	GOTO ReadOEOrderLine

18290	!
	! Now either summarize into totals for a combine operation,
	! or print the invoice if not.
	!
	IF COMBINE_ORDERS$ = "Y"
	THEN
		ORDER_SDISC = FUNC_ROUND(ORDER_STOTAL * &
			OE_ORDERJOUR::DISC / 100.0, 2%)
		ORDER_DISC = ORDER_DISC + ORDER_SDISC

		ORDER_TAX = ORDER_TAX + &
			FUNC_ROUND((ORDER_STOTAL - ORDER_SDISC - &
			ORDER_NONSALTAX) * &
			OE_ORDERJOUR::SALESTAX / 100.0, 2%)

		SHIP_SDISC = FUNC_ROUND(SHIP_STOTAL * &
			OE_ORDERJOUR::DISC / 100.0, 2%)
		SHIP_DISC = SHIP_DISC + SHIP_SDISC

		SHIP_TAX = SHIP_TAX + &
			FUNC_ROUND((SHIP_STOTAL - SHIP_SDISC - &
			SHIP_NONSALTAX) * &
			OE_ORDERJOUR::SALESTAX / 100.0, 2%)

		HANDLING = HANDLING + OE_ORDERJOUR::HANDLING
		MISC     = MISC     + OE_ORDERJOUR::MISC
		FREIGHT  = FREIGHT  + OE_ORDERJOUR::FREIGHT

		OE_ORDERJOUR_OLD = OE_ORDERJOUR

	ELSE
		ORDER_DISC = FUNC_ROUND(ORDER_TOTAL * &
			OE_ORDERJOUR::DISC / 100.0, 2%)

		ORDER_TAX = FUNC_ROUND((ORDER_TOTAL - ORDER_DISC - &
			ORDER_NONSALTAX) * &
			OE_ORDERJOUR::SALESTAX / 100.0, 2%)

		SHIP_DISC = FUNC_ROUND(SHIP_TOTAL * &
			OE_ORDERJOUR::DISC / 100.0, 2%)

		SHIP_TAX = FUNC_ROUND((SHIP_TOTAL - SHIP_DISC - &
			SHIP_NONSALTAX) * &
			OE_ORDERJOUR::SALESTAX / 100.0, 2%)

		GOSUB 18295
	END IF

	RETURN

18295	!*******************************************************************
	! Print one invoice
	!*******************************************************************

	IF COMBINE_ORDERS$ = "Y"
	THEN
		OE_ORDERJOUR::HANDLING = HANDLING
		OE_ORDERJOUR::MISC     = MISC
		OE_ORDERJOUR::FREIGHT  = FREIGHT
	END IF

	!
	! Print Header Notes
	!
	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_HNOTES%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
		FOR I% = BODY_COUNT% + 1% TO FORM_GROUP(FRM_BODY%)::NUMBER

	LINE_COUNT% = LINE_COUNT% + FORM_GROUP(FRM_BODY%)::NUMBER

	!
	! Print the bottom of statement
	!
	LINE_COUNT% = LINE_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BOTTOM%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	CALL OUTP_NEWPAGE(UTL_REPORTX)

	HANDLING, MISC, FREIGHT = 0.0

18300	!
	! Do the next group
	!
	RETURN

	%PAGE

 DumpMOLines:
18500	!*******************************************************************
	! Dump all collected lines to form
	!*******************************************************************

	!
	! Skip to a new page if necessary
	!
	GOSUB NewPage &
		IF BODY_COUNT% >= FORM_GROUP(FRM_BODY%)::NUMBER + &
		FORM_GROUP(FRM_OPTIONS%)::NUMBER

18510	!
	! Get Model Code Description
	!
	WHEN ERROR IN
		GET #MO_MODELCODE.CH%, &
			KEY #0% EQ MO_ORDERLINE::MODELCODE, &
			REGARDLESS
	USE
		MO_MODELCODE::DESCR = ""

		CONTINUE 18520 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_MODELCODE"
		CONTINUE HelpError
	END WHEN

18520	!
	! Get Make Type Description
	!
	WHEN ERROR IN
		GET #MO_MAKETYPE.CH%, &
			KEY #0% EQ MO_ORDERLINE::MTYPE, &
			REGARDLESS
	USE
		MO_MAKETYPE::DESCR = ""

		CONTINUE 18530 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_MAKETYPE"
		CONTINUE HelpError
	END WHEN

18530	!
	! Get Make Size Description
	!
	WHEN ERROR IN
		GET #MO_MAKESIZE.CH%, &
			KEY #0% EQ MO_ORDERLINE::MSIZE, &
			REGARDLESS
	USE
		MO_MAKESIZE::DESCR = ""

		CONTINUE 18540 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_MAKESIZE"
		CONTINUE HelpError
	END WHEN

18540	!
	! Calculate prices, costs, and totals
	!
	!LIST_PRICE = PC_READ_PRICE(OE_ORDERLINE::PRODUCT, &
	!	OE_ORDERJOUR::LOCATION, PS_CONTROL::LISTCODE, &
	!	OE_ORDERJOUR::SHIPDATE, "", "", "")

	REAL_PRICE = FUNC_ROUND((1 - MO_ORDERLINE::DISCOUNT / 100) * &
		(MO_ORDERLINE::PRICE), 2%)

	ORDERLINE = FUNC_ROUND(MO_ORDERLINE::ORDQTY * MO_ORDERLINE::PRICE * &
		(1 - (MO_ORDERLINE::DISCOUNT / 100)), 2%)

	ORDER_TOTAL = ORDER_TOTAL + ORDERLINE
	ORDER_STOTAL = ORDER_STOTAL + ORDERLINE

	SHIPLINE = FUNC_ROUND(MO_ORDERLINE::SHPQTY * MO_ORDERLINE::PRICE * &
		(1 - (MO_ORDERLINE::DISCOUNT / 100)), 2%)

	SHIP_TOTAL = SHIP_TOTAL + SHIPLINE
	SHIP_STOTAL = SHIP_STOTAL + SHIPLINE

	!
	! Print one line
	!
	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_MBODY%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	!
	! Print Line Notes
	!
	IF TRM$(MO_ORDERLINE::NOTES(0%)) <> ""
	THEN
		!
		! Skip to a new page if necessary
		!
		GOSUB NewPage IF BODY_COUNT% >= FORM_GROUP(FRM_BODY%)::NUMBER

		NOTENUMBER% = 0%
		BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_MNOTES%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)
	END IF

	IF TRM$(MO_ORDERLINE::NOTES(1%)) <> ""
	THEN
		!
		! Skip to a new page if necessary
		!
		GOSUB NewPage IF BODY_COUNT% >= FORM_GROUP(FRM_BODY%)::NUMBER

		NOTENUMBER% = 1%
		BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_MNOTES%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)
	END IF

	!
	! Get the MO Order Line Options
	!
18600	WHEN ERROR IN
		FIND #MO_ORDERLINEOPT.CH%, &
			KEY #0% GE OE_ORDERJOUR::ORDNUM + &
			MO_ORDERLINE::LIN + MO_ORDERLINE::MAKE + &
			MO_ORDERLINE::MODELCODE, &
			REGARDLESS
	USE
		CONTINUE 18690 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_ORDERLINEOPT"
		CONTINUE HelpError
	END WHEN

 ReadMOOptLine:
18610	WHEN ERROR IN
		GET #MO_ORDERLINEOPT.CH%, REGARDLESS
	USE
		CONTINUE 18690 IF ERR = 11%
		FILENAME$ = "MO_ORDERLINEOPT"
		CONTINUE HelpError
	END WHEN

	GOTO 18690 IF MO_ORDERLINEOPT::ORDNUM <> OE_ORDERJOUR::ORDNUM OR &
		MO_ORDERLINEOPT::LIN <> MO_ORDERLINE::LIN OR &
		MO_ORDERLINEOPT::MAKE <> MO_ORDERLINE::MAKE OR &
		MO_ORDERLINEOPT::MODELCODE <> MO_ORDERLINE::MODELCODE

	!
	! Skip to a new page if necessary
	!
	GOSUB NewPage IF BODY_COUNT% >= FORM_GROUP(FRM_BODY%)::NUMBER + &
		FORM_GROUP(FRM_OPTIONS%)::NUMBER

	!
	! Get Option Description
	!
	MO_OPTION::DESCR = ""

18650	WHEN ERROR IN
		GET #MO_OPTION.CH%, &
			KEY #0% EQ MO_ORDERLINEOPT::OPTGROUP + &
			MO_ORDERLINEOPT::OPTN, &
			REGARDLESS
	USE
		CONTINUE 18660 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_ORDERLINEOPT"
		CONTINUE HelpError
	END WHEN

	!
	! Calculate line total
	!
18660	ORDERLINE = FUNC_ROUND(MO_ORDERLINEOPT::PRICE * &
		MO_ORDERLINEOPT::ORDQTY, 2%)

	ORD_QTY = ORD_QTY + MO_ORDERLINEOPT::ORDQTY
	ORDER_TOTAL = ORDER_TOTAL + ORDERLINE
	ORDER_STOTAL = ORDER_STOTAL + ORDERLINE

	SHIPLINE = FUNC_ROUND(MO_ORDERLINEOPT::PRICE * &
		MO_ORDERLINEOPT::SHPQTY, 2%)

	SHIP_QTY = SHIP_QTY + MO_ORDERLINEOPT::SHPQTY
	SHIP_TOTAL = SHIP_TOTAL + SHIPLINE
	SHIP_STOTAL = SHIP_STOTAL + SHIPLINE

	!
	! Print one line
	!
	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_OPTIONS%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	GOTO ReadMOOptLine

18690	RETURN

	%PAGE

 SummarizeLines:
18695	!*******************************************************************
	! Summarize lines for an order
	!*******************************************************************

	!
	! Open work file if necessary
	!
	IF OPEN_FLAG% = 0%
	THEN
		WHEN ERROR IN
			OPEN "OE_ORDERLINE.TMP" AS FILE OE_ORDERLINE.TMP%, &
				ORGANIZATION INDEXED FIXED, &
				MAP OE_ORDERLINE, &
				TEMPORARY, &
				BUFFER 32%, &
				PRIMARY KEY &
				( &
					OE_ORDERLINE::PRODUCT, &
					OE_ORDERLINE::ORDNUM &
				), &
				ACCESS MODIFY, ALLOW NONE
		USE
			CONTINUE 18697 IF ERR = 155% OR ERR = 9%
			FILENAME$ = "OE_ORDERLINE.TMP"
			CONTINUE HelpError
		END WHEN

		OPEN_FLAG% = -1%
	END IF

	!
	! Generate necessary totals
	!
	ORDERLINE = FUNC_ROUND(OE_ORDERLINE::ORDQTY * &
		((OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
		(1 - (OE_ORDERLINE::DISCOUNT / 100)) + &
		OE_ORDERLINE::MISCH + OE_ORDERLINE::MISCH2), 2%)

	ORDER_TOTAL = ORDER_TOTAL + ORDERLINE
	ORDER_STOTAL = ORDER_STOTAL + ORDERLINE

	SHIPLINE = FUNC_ROUND(OE_ORDERLINE::SHPQTY * &
		((OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
		(1 - (OE_ORDERLINE::DISCOUNT / 100)) + &
		OE_ORDERLINE::MISCH + OE_ORDERLINE::MISCH2), 2%)

	SUMSHPQTY = SUMSHPQTY + OE_ORDERLINE::SHPQTY
	SHIP_TOTAL = SHIP_TOTAL + SHIPLINE
	SHIP_STOTAL = SHIP_STOTAL + SHIPLINE

	V% = PD_EXAM_PRODUCT(OE_ORDERLINE::PRODUCT, PD_PRODUCT_EXAM)

	IF TRM$(PS_CONTROL::CUSBAL) <> ""
	THEN
		IF COMP_STRING(TRM$(PD_PRODUCT_EXAM::CATEGORY), &
			TRM$(PS_CONTROL::CUSBAL))
		THEN
			ORDER_NONSALTAX = ORDER_NONSALTAX + &
				FUNC_ROUND(OE_ORDERLINE::ORDQTY * &
				((OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
				(1 - (OE_ORDERLINE::DISCOUNT / 100))), 2%)

			SHIP_NONSALTAX = SHIP_NONSALTAX + &
				FUNC_ROUND(OE_ORDERLINE::SHPQTY * &
				((OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
				(1 - (OE_ORDERLINE::DISCOUNT / 100))), 2%)
		END IF
	END IF

	IF PS_CONTROL::MISCTAXABLE = "N"
	THEN
		ORDER_NONSALTAX = ORDER_NONSALTAX + &
			FUNC_ROUND(OE_ORDERLINE::MISCH * &
			OE_ORDERLINE::ORDQTY, 2%)

		SHIP_NONSALTAX = SHIP_NONSALTAX + &
			FUNC_ROUND(OE_ORDERLINE::MISCH * &
			OE_ORDERLINE::ORDQTY, 2%)
	END IF

	IF PS_CONTROL::MISC2TAXABLE = "N"
	THEN
		ORDER_NONSALTAX = ORDER_NONSALTAX + &
			FUNC_ROUND(OE_ORDERLINE::MISCH2 * &
			OE_ORDERLINE::ORDQTY, 2%)

		SHIP_NONSALTAX = SHIP_NONSALTAX + &
			FUNC_ROUND(OE_ORDERLINE::MISCH2 * &
			OE_ORDERLINE::SHPQTY, 2%)
	END IF

	!
	! Put in temp file
	!
	OE_ORDERLINE_TEMP = OE_ORDERLINE

	GET #OE_ORDERLINE.TMP%, &
		KEY #0% EQ OE_ORDERLINE::PRODUCT

	OE_ORDERLINE::ORDQTY = OE_ORDERLINE::ORDQTY + OE_ORDERLINE_TEMP::ORDQTY
	OE_ORDERLINE::SHPQTY = OE_ORDERLINE::SHPQTY + OE_ORDERLINE_TEMP::SHPQTY
	OE_ORDERLINE::BCKQTY = OE_ORDERLINE::BCKQTY + OE_ORDERLINE_TEMP::BCKQTY

	UPDATE #OE_ORDERLINE.TMP%
	GOTO EndSummary

18697	PUT #OE_ORDERLINE.TMP%

  EndSummary:
	RETURN

 DumpOELines:
18700	!*******************************************************************
	! Dump all collected lines to form
	!*******************************************************************

	!
	! Skip to a new page if necessary
	!
	GOSUB NewPage IF BODY_COUNT% >= FORM_GROUP(FRM_BODY%)::NUMBER

	!
	! Get a description for this product
	!
	V% = PD_EXAM_PRODUCT(OE_ORDERLINE::PRODUCT, PD_PRODUCT_EXAM)

	!
	! See if we can find a bin location for this product
	!
	IC_BINMAP::BIN(I%) = "" FOR I% = 0% TO 3%

18750	WHEN ERROR IN
		GET #IC_BINMAP.CH%, &
			KEY #0% EQ OE_ORDERLINE::PRODUCT + &
			OE_ORDERJOUR::LOCATION, &
			REGARDLESS
	USE
		CONTINUE BinmapError IF ERR = 155% OR ERR = 9%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

 BinmapError:
	LIST_PRICE = PC_READ_PRICE(OE_ORDERLINE::PRODUCT, &
		OE_ORDERJOUR::LOCATION, PS_CONTROL::LISTCODE, &
		OE_ORDERJOUR::SHIPDATE, "", "", "")

	REAL_PRICE = FUNC_ROUND((1 - OE_ORDERLINE::DISCOUNT / 100) * &
		(OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO), 2%)

	ORDERLINE = FUNC_ROUND(OE_ORDERLINE::ORDQTY * &
		((OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
		(1 - (OE_ORDERLINE::DISCOUNT / 100)) + &
		OE_ORDERLINE::MISCH + OE_ORDERLINE::MISCH2), 2%)

	ORDER_TOTAL = ORDER_TOTAL   + ORDERLINE
	ORDER_STOTAL = ORDER_STOTAL  + ORDERLINE

	SHIPLINE = FUNC_ROUND(OE_ORDERLINE::SHPQTY * &
		((OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
		(1 - (OE_ORDERLINE::DISCOUNT / 100)) + &
		OE_ORDERLINE::MISCH + OE_ORDERLINE::MISCH2), 2%)

	SUMSHPQTY = SUMSHPQTY + OE_ORDERLINE::SHPQTY
	SHIP_TOTAL = SHIP_TOTAL + SHIPLINE
	SHIP_STOTAL = SHIP_STOTAL + SHIPLINE

	PROMO = FUNC_ROUND(OE_ORDERLINE::SHPQTY * OE_ORDERLINE::PROMO, 2%)

	IF TRM$(PS_CONTROL::CUSBAL) <> ""
	THEN
		IF COMP_STRING(TRM$(PD_PRODUCT_EXAM::CATEGORY), &
			TRM$(PS_CONTROL::CUSBAL))
		THEN
			ORDER_NONSALTAX = ORDER_NONSALTAX + &
				FUNC_ROUND(OE_ORDERLINE::ORDQTY * &
				((OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
				(1 - (OE_ORDERLINE::DISCOUNT / 100))), 2%)

			SHIP_NONSALTAX = SHIP_NONSALTAX + &
				FUNC_ROUND(OE_ORDERLINE::SHPQTY * &
				((OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
				(1 - (OE_ORDERLINE::DISCOUNT / 100))), 2%)
		END IF
	END IF

	IF PS_CONTROL::MISCTAXABLE = "N"
	THEN
		ORDER_NONSALTAX = ORDER_NONSALTAX + &
			FUNC_ROUND(OE_ORDERLINE::MISCH * &
			OE_ORDERLINE::ORDQTY, 2%)

		SHIP_NONSALTAX = SHIP_NONSALTAX + &
			FUNC_ROUND(OE_ORDERLINE::MISCH * &
			OE_ORDERLINE::ORDQTY, 2%)
	END IF

	IF PS_CONTROL::MISC2TAXABLE = "N"
	THEN
		ORDER_NONSALTAX = ORDER_NONSALTAX + &
			FUNC_ROUND(OE_ORDERLINE::MISCH2 * &
			OE_ORDERLINE::ORDQTY, 2%)

		SHIP_NONSALTAX = SHIP_NONSALTAX + &
			FUNC_ROUND(OE_ORDERLINE::MISCH2 * &
			OE_ORDERLINE::SHPQTY, 2%)
	END IF

	!
	! Print one line
	!
	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BODY%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	!
	! Read Promo if any
	!
	IF OE_READ_PROMO(OE_ORDERLINE::PRODUCT, OE_ORDERLINE::REQDATE, &
		OE_ORDERJOUR::CUSNUM, OE_PROMO_READ, 0.0, 0.0) = CMC$_NORMAL &
		AND OE_ORDERLINE::PROMO <> 0.0
	THEN
		!
		! Page if necessary
		!
		GOSUB NewPage IF BODY_COUNT% >= FORM_GROUP(FRM_BODY%)::NUMBER

		BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_PNOTES%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)
	END IF

	!
	! Print Line Notes
	!
	IF TRM$(OE_ORDERLINE::NOTES1) <> ""
	THEN
		!
		! Page if necessary
		!
		GOSUB NewPage IF BODY_COUNT% >= FORM_GROUP(FRM_BODY%)::NUMBER

		NOTENUMBER% = 0%
		BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_LNOTES%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)
	END IF

	IF TRM$(OE_ORDERLINE::NOTES2) <> ""
	THEN
		!
		! Page if necessary
		!
		GOSUB NewPage IF BODY_COUNT% >= FORM_GROUP(FRM_BODY%)::NUMBER

		NOTENUMBER% = 1%
		BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
			FRM_LNOTES%, &
			FORM_TEXT$, &
			FORM_GROUPS%, &
			FORM_GROUP(), &
			0%)
	END IF

18790	RETURN

	%PAGE

 PrintCombineLines:
18795	WHEN ERROR IN
		RESET #OE_ORDERLINE.TMP%
	USE
		CONTINUE 18797 IF ERR = 155% OR ERR = 9% OR ERR = 131%
		FILENAME$ = "OE_ORDERLINE.TMP"
		CONTINUE HelpError
	END WHEN

 NextCombine:
	WHEN ERROR IN
		GET #OE_ORDERLINE.TMP%
	USE
		CONTINUE 18797 IF ERR = 155% OR ERR = 9% OR ERR = 131% &
			OR ERR = 11%
		FILENAME$ = "OE_ORDERLINE.TMP"
		CONTINUE HelpError
	END WHEN

	GOSUB DumpOELines

	GOTO NextCombine

18797	CLOSE OE_ORDERLINE.TMP%
	OPEN_FLAG% = 0%
	RETURN

 NewPage:
18800	CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
		FOR I% = BODY_COUNT% + 1% TO FORM_GROUP(FRM_BODY%)::NUMBER

	LINE_COUNT% = LINE_COUNT% + FORM_GROUP(FRM_BODY%)::NUMBER

	!
	! Print the bottom of statement
	!
	LINE_COUNT% = LINE_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_SUBBOTTOM%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	CALL OUTP_NEWPAGE(UTL_REPORTX)

	BODY_COUNT%  = 0%
	PAGE_NUMBER% = PAGE_NUMBER% + 1%

	IF FRM_ASSIGN% = 1%
	THEN
		IF INVOICE% = 0%
		THEN
			V% = FUNC_INCREMENT(PS_CASHREG::LAST_INVNUM)

			!
			! Put last INVOICE in CASHREG file
			!
			WHEN ERROR IN
				GET #PS_CASHREG.CH%, &
					KEY #0% EQ LEFT(BATCH, UNDER% - 1%)

				UPDATE #PS_CASHREG.CH%
				UNLOCK #PS_CASHREG.CH%
			USE
				IF ERR = 154%	! Locked Block
				THEN
					SLEEP 5%
					RETRY
				END IF

				FILENAME$ = "PS_CASHREG"
				CONTINUE HelpError
			END WHEN
		END IF

		XOUTASSIGN$ = STRING$(30%, A"X"B)
	END IF

	!
	! Print the top of statement
	!
	LINE_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		0%)

	RETURN

	%PAGE

 LoadForm:
	!*******************************************************************
	! Initilize Statement form
	!*******************************************************************

	!
	! Get Form from the PS Form Library
	!
	SMG_STATUS% = OUTP_FORMINIT( &
		PS_FORM.DEV$ + "PS_FORM", REPORT$, &
		FORM_TEXT$, FORM_GROUP%, FORM_GROUP())

	!
	! Was there an error?
	!
	IF SMG_STATUS% <> 0%
	THEN
		!
		! Get generic form from the PS Form Library
		!
		SMG_STATUS% = OUTP_FORMINIT( &
			PS_FORM.DEV$ + "PS_FORM", FORM$, &
			FORM_TEXT$, FORM_GROUP%, FORM_GROUP())

		IF SMG_STATUS% <> 0%
		THEN
			CALL HELP_34MESSAGE(SCOPE, "form is missing", "E", &
				SCOPE::PRG_PROGRAM, REPORT$, NUM1$(SMG_STATUS%))

			GOTO ExitProgram
		END IF
	END IF

	!
	! Search for the desired parts of the form
	!
	FRM_TOP% = 0%
	FRM_BODY% = 0%
	FRM_MBODY% = 0%
	FRM_OPTIONS% = 0%
	FRM_BOTTOM% = 0%
	FRM_SUBBOTTOM% = 0%
	FRM_HNOTES% = 0%
	FRM_LNOTES% = 0%
	FRM_MNOTES% = 0%
	FRM_PNOTES% = 0%
	FRM_ASSIGN% = 0%

	FOR I% = 1% TO FORM_GROUP%

		SELECT FORM_GROUP(I%)::FGROUP

		CASE "FRM-TOP"			! Top of the forms
			FRM_TOP% = I%

		CASE "FRM-BODY"			! Normal lines
			FRM_BODY% = I%

		CASE "FRM-MBODY"		! MO lines
			FRM_MBODY% = I%

		CASE "FRM-OPTIONS"		! MO Option lines
			FRM_OPTIONS% = I%

		CASE "FRM-BOTTOM"		! Bottom of the form
			FRM_BOTTOM% = I%

		CASE "FRM-HNOTES"		! Header Notes
			FRM_HNOTES% = I%

		CASE "FRM-LNOTES"		! Line Notes
			FRM_LNOTES% = I%

		CASE "FRM-MNOTES"		! MO Notes
			FRM_MNOTES% = I%

		CASE "FRM-PNOTES"		! Promo notes
			FRM_PNOTES% = I%

		CASE "FRM-SUBBOTTOM"		! Bottom when paging
			FRM_SUBBOTTOM% = I%

		CASE "FRM-ASSIGN"		! Assign invoice numbers
			FRM_ASSIGN% = 1%

		END SELECT

	NEXT I%

	IF FRM_SUBBOTTOM% = 0%
	THEN
		FRM_SUBBOTTOM% = FRM_BOTTOM%
	END IF

	RETURN

	%PAGE

 Alignment:
	!*******************************************************************
	! Print alignment form, if desireable
	!*******************************************************************

	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	PAGE_NUMBER% = 1%
	LINE_COUNT%  = 0%
	BODY_COUNT%  = 0%

	UTL_REPORTX::LINENO = 0%
	UTL_REPORTX::PAGENO = 0%

	SCOPE::PRG_ITEM = "ALIGNMENT"

	!++
	! Abstract:ALIGNMENT
	!
	!
	! Index:
	!
	!
	!--
	JUNK$ = ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
		"Do you want an alignment form?  Confirm then press <Do> ", &
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

	!
	! Print the top of statement
	!
	LINE_COUNT% = OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_TOP%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%)

	!
	! Print three lines of MO_ORDERLINE body
	!
	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_MBODY%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%) &
		FOR I% = 1% TO 3%

	!
	! Print three lines of MO_ORDERLINEOPT
	!
	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_OPTIONS%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%) &
		FOR I% = 1% TO 3%

	!
	! Print three lines of OE_ORDERLINE body
	!
	BODY_COUNT% = BODY_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BODY%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%) &
		FOR I% = 1% TO 3%

	!
	! Print down to the bottom of the form
	!
	CALL OUTP_LINENOTITLE("", UTL_REPORTX, "", 0%) &
		FOR I% = BODY_COUNT% + 1% TO FORM_GROUP(FRM_BODY%)::NUMBER

	LINE_COUNT% = LINE_COUNT% + FORM_GROUP(FRM_BODY%)::NUMBER

	!
	! Display BOTTOM
	!
	LINE_COUNT% = LINE_COUNT% + OUTP_FORMPRINT(UTL_REPORTX, &
		FRM_BOTTOM%, &
		FORM_TEXT$, &
		FORM_GROUPS%, &
		FORM_GROUP(), &
		1%)

	CALL OUTP_NEWPAGE(UTL_REPORTX)

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
	END FUNCTION

20000	SUB PS_OUTP_TICKET_LOADVAR(VARNAME$, REALVALUE, TEXTVALUE$)

	!*******************************************************************
	! This function is used to return the value of a field back to the
	! form printer.
	!*******************************************************************

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	!
	! Define maps
	!
	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.HB"
	MAP (OE_ORDERLINE)	OE_ORDERLINE_CDD	OE_ORDERLINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINE.HB"
	MAP (MO_ORDERLINE)	MO_ORDERLINE_CDD	MO_ORDERLINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_ORDERLINEOPT.HB"
	MAP (MO_ORDERLINEOPT)	MO_ORDERLINEOPT_CDD	MO_ORDERLINEOPT

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	COM (AR_35CUSTOM_EXAM)	AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[MO.OPEN]MO_OPTION.HB"
	MAP (MO_OPTION)		MO_OPTION_CDD		MO_OPTION

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKETYPE.HB"
	MAP (MO_MAKETYPE)	MO_MAKETYPE_CDD		MO_MAKETYPE

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKESIZE.HB"
	MAP (MO_MAKESIZE)	MO_MAKESIZE_CDD		MO_MAKESIZE

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODELCODE.HB"
	MAP (MO_MODELCODE)	MO_MODELCODE_CDD	MO_MODELCODE

	%INCLUDE "SOURCE:[OE.OPEN]OE_CATEGORY.HB"
	MAP (OE_CATEGORY)	OE_CATEGORY_CDD		OE_CATEGORY

	%INCLUDE "SOURCE:[OE.OPEN]OE_CREASON.HB"
	MAP (OE_CREASON)	OE_CREASON_CDD		OE_CREASON

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERTYPE.HB"
	MAP (OE_ORDERTYPE)	OE_ORDERTYPE_CDD	OE_ORDERTYPE

	%INCLUDE "SOURCE:[OE.OPEN]OE_PROMO.HB"
	COM (OE_PROMO_READ)	OE_PROMO_CDD		OE_PROMO_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	DECLARE			OE_REGLINE_CDD		OE_REGLINE_READ

	%INCLUDE "SOURCE:[OE.OPEN]OE_SHIPTO.HB"
	MAP (OE_SHIPTO)		OE_SHIPTO_CDD		OE_SHIPTO

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	COM (PD_PRODUCT_EXAM)	PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[PS.OPEN]PS_CASHREG.HB"
	MAP (PS_CASHREG)	PS_CASHREG_CDD		PS_CASHREG

	%INCLUDE "SOURCE:[PS.OPEN]PS_CONTROL.HB"
	MAP (PS_CONTROL)	PS_CONTROL_CDD		PS_CONTROL

	%INCLUDE "SOURCE:[SA.OPEN]SA_SALESMAN.HB"
	MAP (SB_SUBACCOUNT)	SA_SALESMAN_CDD		SA_SALESMAN

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_CARRIER.HB"
	MAP (UTL_CARRIER)	UTL_CARRIER_CDD		UTL_CARRIER

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	COM (UTL_LOCATION_EXAM)	UTL_LOCATION_CDD	UTL_LOCATION_EXAM

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TERMS.HB"
	MAP (UTL_TERMS)		UTL_TERMS_CDD		UTL_TERMS

	MAP (JOUR_FORM) &
		OE_ORDERJOUR.ADDLINE$(4%) = 50%, &
		AR_CUSTOM.ADDLINE$(4%) = 50%, &
		UTL_LOCATION_EXAM.ADDLINE$(4%) = 50%, &
		LIST_PRICE, &
		REAL_PRICE, &
		BALANCE, &
		ORIGORD_QTY, &
		ORD_QTY, &
		SUMSHPQTY, &
		ORDERLINE, &
		ORDER_TOTAL, &
		ORDER_DISC, &
		ORDER_TAX, &
		SHIP_QTY, &
		SHIPLINE, &
		SHIP_TOTAL, &
		SHIP_DISC, &
		SHIP_TAX, &
		XOUTASSIGN$ = 30%, &
		PAGE_NUMBER%, &
		LINE_NUM%, &
		DUEDAYS$(10%) = 14%, &
		NOTENUMBER%, &
		ORDER_NONSALTAX

	%PAGE

	!
	! Set up default values
	!
	REALVALUE = 0.0
	TEXTVALUE$ = STRING$(40%, A"?"B)

	!
	! Pick by variable
	!
	SELECT VARNAME$

	!************************************************************
	! Fields for the Order Entery Order Journal Header file
	!************************************************************

	CASE "OE_ORDERJOUR::ORDNUM"
		TEXTVALUE$ = OE_ORDERJOUR::ORDNUM

	CASE "OE_ORDERJOUR::ORDDATE"
		TEXTVALUE$ = PRNT_DATE(OE_ORDERJOUR::ORDDATE, 8%)

	CASE "OE_ORDERJOUR::ORDDATE6"
		TEXTVALUE$ = PRNT_DATE(OE_ORDERJOUR::ORDDATE, 6%)

	CASE "OE_ORDERJOUR::ORDTYPE"
		TEXTVALUE$ = OE_ORDERJOUR::ORDTYPE

	CASE "OE_ORDERJOUR::ORDCAT"
		TEXTVALUE$ = OE_ORDERJOUR::ORDCAT

	CASE "OE_ORDERJOUR::CUSNUM"
		TEXTVALUE$ = OE_ORDERJOUR::CUSNUM

	CASE "OE_ORDERJOUR::DISC"
		REALVALUE = OE_ORDERJOUR::DISC

	CASE "OE_ORDERJOUR::MISC"
		REALVALUE = OE_ORDERJOUR::MISC

	CASE "OE_ORDERJOUR::SHIPNAM"
		TEXTVALUE$ = OE_ORDERJOUR::SHIPNAM

	CASE "OE_ORDERJOUR::ADD1"
		TEXTVALUE$ = OE_ORDERJOUR::ADD1

	CASE "OE_ORDERJOUR::ADD2"
		TEXTVALUE$ = OE_ORDERJOUR::ADD2

	CASE "OE_ORDERJOUR::ADD3"
		TEXTVALUE$ = OE_ORDERJOUR::ADD3

	CASE "OE_ORDERJOUR::CITY"
		TEXTVALUE$ = OE_ORDERJOUR::CITY

	CASE "OE_ORDERJOUR::STATE"
		TEXTVALUE$ = OE_ORDERJOUR::STATE

	CASE "OE_ORDERJOUR::ZIP"
		TEXTVALUE$ = OE_ORDERJOUR::ZIP

	CASE "OE_ORDERJOUR::COUNTRY"
		TEXTVALUE$ = OE_ORDERJOUR::COUNTRY

	CASE "OE_ORDERJOUR:ADDLINE1"	! Substitute Ship to Address
		TEXTVALUE$ = OE_ORDERJOUR.ADDLINE$(1%)

	CASE "OE_ORDERJOUR:ADDLINE2"	! Substitute Ship to Address
		TEXTVALUE$ = OE_ORDERJOUR.ADDLINE$(2%)

	CASE "OE_ORDERJOUR:ADDLINE3"	! Substitute Ship to Address
		TEXTVALUE$ = OE_ORDERJOUR.ADDLINE$(3%)

	CASE "OE_ORDERJOUR:ADDLINE4"	! Substitute Ship to Address
		TEXTVALUE$ = OE_ORDERJOUR.ADDLINE$(4%)

	CASE "OE_ORDERJOUR::CUSTPO"
		TEXTVALUE$ = OE_ORDERJOUR::CUSTPO

	CASE "OE_ORDERJOUR::SHIPDATE"
		TEXTVALUE$ = PRNT_DATE(OE_ORDERJOUR::SHIPDATE, 8%)

	CASE "OE_ORDERJOUR::SHIPDATE6"
		TEXTVALUE$ = PRNT_DATE(OE_ORDERJOUR::SHIPDATE, 6%)

	CASE "OE_ORDERJOUR::SHIPVIA"
		TEXTVALUE$ = OE_ORDERJOUR::SHIPVIA

	CASE "OE_ORDERJOUR::TERMS"
		TEXTVALUE$ = OE_ORDERJOUR::TERMS

	CASE "OE_ORDERJOUR::SALESTAX"
		REALVALUE = OE_ORDERJOUR::SALESTAX

	CASE "OE_ORDERJOUR::LOCATION"
		TEXTVALUE$ = OE_ORDERJOUR::LOCATION

	CASE "OE_ORDERJOUR::OPERATOR"
		TEXTVALUE$ = OE_ORDERJOUR::OPERATOR

	CASE "OE_ORDERJOUR::COMMAMT"
		REALVALUE = OE_ORDERJOUR::COMMAMT

	CASE "OE_ORDERJOUR::COMMPERC"
		REALVALUE = OE_ORDERJOUR::COMMPERC

	CASE "OE_ORDERJOUR::SALESMAN"
		TEXTVALUE$ = OE_ORDERJOUR::SALESMAN

	CASE "OE_ORDERJOUR::CREASON"
		TEXTVALUE$ = OE_ORDERJOUR::CREASON

	CASE "OE_ORDERJOUR::SALCOMM"
		REALVALUE = OE_ORDERJOUR::SALCOMM

	CASE "OE_ORDERJOUR::HANDLING"
		REALVALUE = OE_ORDERJOUR::HANDLING

	CASE "OE_ORDERJOUR::AMTPAID"
		REALVALUE = OE_ORDERJOUR::AMTPAID

	CASE "OE_ORDERJOUR::CHECK"
		TEXTVALUE$ = OE_ORDERJOUR::CHECK

	CASE "OE_ORDERJOUR::DEPOSIT"
		TEXTVALUE$ = OE_ORDERJOUR::CHECK

	CASE "OE_ORDERJOUR::NOTE1"
		TEXTVALUE$ = OE_ORDERJOUR::NOTES(0%)

	CASE "OE_ORDERJOUR::NOTE2"
		TEXTVALUE$ = OE_ORDERJOUR::NOTES(1%)

	CASE "OE_ORDERJOUR::NOTE3"
		TEXTVALUE$ = OE_ORDERJOUR::NOTES(2%)

	CASE "OE_ORDERJOUR::MISCACCT"
		TEXTVALUE$ = OE_ORDERJOUR::MISCACCT

	CASE "OE_ORDERJOUR::TRANDATE"
		TEXTVALUE$ = PRNT_DATE(OE_ORDERJOUR::TRANDATE, 8%)

	CASE "OE_ORDERJOUR::TRANDATE6"
		TEXTVALUE$ = PRNT_DATE(OE_ORDERJOUR::TRANDATE, 6%)

	CASE "OE_ORDERJOUR::TRANTIME"
		TEXTVALUE$ = PRNT_TIME(OE_ORDERJOUR::TRANTIME, 2%)

	CASE "OE_ORDERJOUR::INVNUM"
		TEXTVALUE$ = OE_ORDERJOUR::INVNUM

	CASE "OE_ORDERJOUR::FREIGHT"
		REALVALUE = OE_ORDERJOUR::FREIGHT

	CASE "OE_ORDERJOUR::TAXCODE"
		TEXTVALUE$ = OE_ORDERJOUR::TAXCODE

	CASE "OE_ORDERJOUR::TAXFLAG"
		TEXTVALUE$ = OE_ORDERJOUR::TAXFLAG

	CASE "OE_ORDERJOUR::SHIPLIN"
		TEXTVALUE$ = OE_ORDERJOUR::SHIPLIN

	CASE "OE_ORDERJOUR::PAYMNT"
		REALVALUE = OE_ORDERJOUR::PAYMNT

	!************************************************************
	! Fields for the Order Entry Order Line file
	!************************************************************

	CASE "OE_ORDERLINE::ORDNUM"
		TEXTVALUE$ = OE_ORDERLINE::ORDNUM

	CASE "OE_ORDERLINE::PRODUCT"
		TEXTVALUE$ = OE_ORDERLINE::PRODUCT

	CASE "OE_ORDERLINE::ORDQTY"
		REALVALUE = OE_ORDERLINE::ORDQTY

	CASE "ORDQTY"
		REALVALUE = ABS(SGN(OE_ORDERLINE::ORDQTY))

	CASE "OE_ORDERLINE::SHPQTY"
		REALVALUE = OE_ORDERLINE::SHPQTY

	CASE "OE_ORDERLINE::PRICE"
		REALVALUE = OE_ORDERLINE::PRICE

	CASE "OE_ORDERLINE::DISCOUNT"
		REALVALUE = OE_ORDERLINE::DISCOUNT

	CASE "OE_ORDERLINE::COST"
		REALVALUE = OE_ORDERLINE::COST

	CASE "OE_ORDERLINE::REQDATE"
		TEXTVALUE$ = PRNT_DATE(OE_ORDERLINE::REQDATE, 8%)

	CASE "OE_ORDERLINE::REQDATE6"
		TEXTVALUE$ = PRNT_DATE(OE_ORDERLINE::REQDATE, 6%)

	CASE "OE_ORDERLINE::PROMO"
		REALVALUE = OE_ORDERLINE::PROMO

	CASE "OE_ORDERLINE::MISCH"
		REALVALUE = OE_ORDERLINE::MISCH

	CASE "OE_ORDERLINE::MISCH2"
		REALVALUE = OE_ORDERLINE::MISCH2

	CASE "MISCH"
		REALVALUE = ABS(SGN(OE_ORDERLINE::MISCH))

	CASE "MISCH2"
		REALVALUE = ABS(SGN(OE_ORDERLINE::MISCH2))

	CASE "OE_ORDERLINE::BCKQTY"
		REALVALUE = OE_ORDERLINE::BCKQTY

	CASE "OE_ORDERLINE::NOTES1"
		TEXTVALUE$ = OE_ORDERLINE::NOTES1

	CASE "OE_ORDERLINE::NOTES2"
		TEXTVALUE$ = OE_ORDERLINE::NOTES2

	CASE "OE_ORDERLINE::SUBACCT"
		TEXTVALUE$ = OE_ORDERLINE::SUBACCT

	CASE "OE_ORDERLINE::NOTES"
		SELECT NOTENUMBER%
		CASE 0%
			TEXTVALUE$ = OE_ORDERLINE::NOTES1
		CASE 1%
			TEXTVALUE$ = OE_ORDERLINE::NOTES2
		END SELECT

	CASE "OE_ORDERLINE::LIN"
		TEXTVALUE$ = OE_ORDERLINE::LIN

	!************************************************************
	! Fields for the MO Order Line file
	!************************************************************

	CASE "MO_ORDERLINE::ORDNUM"
		TEXTVALUE$ = MO_ORDERLINE::ORDNUM

	CASE "MO_ORDERLINE::LIN"
		TEXTVALUE$ = MO_ORDERLINE::LIN

	CASE "MO_ORDERLINE::MAKE"
		TEXTVALUE$ = MO_ORDERLINE::MAKE

	CASE "MO_ORDERLINE::YEAR"
		TEXTVALUE$ = MO_ORDERLINE::YEAR

	CASE "MO_ORDERLINE::MTYPE"
		TEXTVALUE$ = MO_ORDERLINE::MTYPE

	CASE "MO_ORDERLINE::MSIZE"
		TEXTVALUE$ = MO_ORDERLINE::MSIZE

	CASE "MO_ORDERLINE::MODELCODE"
		TEXTVALUE$ = MO_ORDERLINE::MODELCODE

	CASE "MO_ORDERLINE::PRODUCT"
		TEXTVALUE$ = MO_ORDERLINE::PRODUCT

	CASE "MO_ORDERLINE::ORDQTY"
		REALVALUE = MO_ORDERLINE::ORDQTY

	CASE "MO_ORDERLINE::PRICE"
		REALVALUE = MO_ORDERLINE::PRICE

	CASE "MO_ORDERLINE::COST"
		REALVALUE = MO_ORDERLINE::COST

	CASE "MO_ORDERLINE::REQDATE"
		TEXTVALUE$ = PRNT_DATE(MO_ORDERLINE::REQDATE, 8%)

	CASE "MO_ORDERLINE::REQDATE6"
		TEXTVALUE$ = PRNT_DATE(MO_ORDERLINE::REQDATE, 6%)

	CASE "MO_ORDERLINE::SHPQTY"
		REALVALUE = MO_ORDERLINE::SHPQTY

	CASE "MO_ORDERLINE::BCKQTY"
		REALVALUE = MO_ORDERLINE::BCKQTY

	CASE "MO_ORDERLINE::NOTES1"
		TEXTVALUE$ = MO_ORDERLINE::NOTES(0%)

	CASE "MO_ORDERLINE::NOTES2"
		TEXTVALUE$ = MO_ORDERLINE::NOTES(1%)

	CASE "MO_ORDERLINE::NOTES"
		TEXTVALUE$ = MO_ORDERLINE::NOTES(NOTENUMBER%)

	CASE "MO_ORDERLINE::IDNUM"
		TEXTVALUE$ = MO_ORDERLINE::IDNUM

	!************************************************************
	! Fields for the MO Order Line Option file
	!************************************************************

	CASE "MO_ORDERLINEOPT::ORDNUM"
		TEXTVALUE$ = MO_ORDERLINEOPT::ORDNUM

	CASE "MO_ORDERLINEOPT::LIN"
		TEXTVALUE$ = MO_ORDERLINEOPT::LIN

	CASE "MO_ORDERLINEOPT::OPTGROUP"
		TEXTVALUE$ = MO_ORDERLINEOPT::OPTGROUP

	CASE "MO_ORDERLINEOPT::OPTN"
		TEXTVALUE$ = MO_ORDERLINEOPT::OPTN

	CASE "MO_ORDERLINEOPT::ORDQTY"
		REALVALUE = MO_ORDERLINEOPT::ORDQTY

	CASE "MO_ORDERLINEOPT::COST"
		REALVALUE = MO_ORDERLINEOPT::COST

	CASE "MO_ORDERLINEOPT::PRICE"
		REALVALUE = MO_ORDERLINEOPT::PRICE

	CASE "MO_ORDERLINEOPT::PRODUCT"
		TEXTVALUE$ = MO_ORDERLINEOPT::PRODUCT

	CASE "MO_ORDERLINEOPT::OPTDESCR"
		TEXTVALUE$ = MO_ORDERLINEOPT::OPTDESCR

	CASE "MO_ORDERLINEOPT::LINOPT"
		TEXTVALUE$ = MO_ORDERLINEOPT::LINOPT

	CASE "MO_ORDERLINEOPT::SHPQTY"
		REALVALUE = MO_ORDERLINEOPT::SHPQTY

	CASE "MO_ORDERLINEOPT::BCKQTY"
		REALVALUE = MO_ORDERLINEOPT::BCKQTY

	!************************************************************
	! Fields for the Accounts Receivable Ver. 3.5 Customer file
	!************************************************************

	CASE "AR_35CUSTOM::CUSNUM"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::CUSNUM

	CASE "AR_35CUSTOM::CUSNAM"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::CUSNAM

	CASE "AR_35CUSTOM::TTYPE"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::TTYPE

	CASE "AR_35CUSTOM::CATEGORY"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::CATEGORY

	CASE "AR_35CUSTOM::BDATE"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM_EXAM::BDATE, 8%)

	CASE "AR_35CUSTOM::BDATE6"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM_EXAM::BDATE, 6%)

	CASE "AR_35CUSTOM::SSTATUS"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::SSTATUS

	CASE "AR_35CUSTOM::EDATE"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM_EXAM::EDATE, 8%)

	CASE "AR_35CUSTOM::EDATE6"
		TEXTVALUE$ = PRNT_DATE(AR_35CUSTOM_EXAM::EDATE, 6%)

	CASE "AR_35CUSTOM::ADD1"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::ADD1

	CASE "AR_35CUSTOM::ADD2"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::ADD2

	CASE "AR_35CUSTOM::ADD3"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::ADD3

	CASE "AR_35CUSTOM::CITY"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::CITY

	CASE "AR_35CUSTOM::STATE"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::STATE

	CASE "AR_35CUSTOM::ZIP"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::ZIP

	CASE "AR_35CUSTOM::COUNTRY"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::COUNTRY

	CASE "AR_35CUSTOM::COUNTY"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::COUNTY

	CASE "AR_35CUSTOM:ADDLINE1"	! Substitute Customer Address
		TEXTVALUE$ = AR_CUSTOM.ADDLINE$(1%)

	CASE "AR_35CUSTOM:ADDLINE2"	! Substitute Customer Address
		TEXTVALUE$ = AR_CUSTOM.ADDLINE$(2%)

	CASE "AR_35CUSTOM:ADDLINE3"	! Substitute Customer Address
		TEXTVALUE$ = AR_CUSTOM.ADDLINE$(3%)

	CASE "AR_35CUSTOM:ADDLINE4"	! Substitute Customer Address
		TEXTVALUE$ = AR_CUSTOM.ADDLINE$(4%)

	CASE "AR_35CUSTOM::PHONE"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::PHONE

	CASE "AR_35CUSTOM::METHOD"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::METHOD

	CASE "AR_35CUSTOM::STMTFLG"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::STMTFLG

	CASE "AR_35CUSTOM::ALPSRT"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::ALPSRT

	CASE "AR_35CUSTOM::SERCHRG"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::SERCHRG

	CASE "AR_35CUSTOM::TAXCODE"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::TAXCODE

	CASE "AR_35CUSTOM::TAXEXEMP"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::TAXEXEMP

	CASE "AR_35CUSTOM::LOCATION"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::LOCATION

	CASE "AR_35CUSTOM::TERMS"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::TERMS

	CASE "AR_35CUSTOM::CARRIER"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::CARRIER

	CASE "AR_35CUSTOM::SALESMAN"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::SALESMAN

	CASE "AR_35CUSTOM::CREDITLIM"
		REALVALUE = AR_35CUSTOM_EXAM::CREDITLIM

	CASE "AR_35CUSTOM::DISCOUNT"
		REALVALUE = AR_35CUSTOM_EXAM::DISCOUNT

	CASE "AR_35CUSTOM::BACKORDER"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::BACKORDER

	CASE "AR_35CUSTOM::TAXFLAG"
		TEXTVALUE$ = AR_35CUSTOM_EXAM::TAXFLAG

	!************************************************************
	! Fields for the IC_BINMAP file
	!************************************************************

	CASE "IC_BINMAP::PRODUCT"
		TEXTVALUE$ = IC_BINMAP::PRODUCT

	CASE "IC_BINMAP::LOCATION"
		TEXTVALUE$ = IC_BINMAP::LOCATION

	CASE "IC_BINMAP::BIN1"
		TEXTVALUE$ = IC_BINMAP::BIN(0%)

	CASE "IC_BINMAP::BIN2"
		TEXTVALUE$ = IC_BINMAP::BIN(1%)

	CASE "IC_BINMAP::BIN3"
		TEXTVALUE$ = IC_BINMAP::BIN(2%)

	CASE "IC_BINMAP::BIN4"
		TEXTVALUE$ = IC_BINMAP::BIN(3%)

	CASE "IC_BINMAP::SAFETY"
		REALVALUE = IC_BINMAP::SAFETY

	CASE "IC_BINMAP::MAXLEVEL"
		REALVALUE = IC_BINMAP::MAXLEVEL

	CASE "IC_BINMAP::ABC"
		TEXTVALUE$ = IC_BINMAP::ABC

	CASE "IC_BINMAP::CYCLEMAP"
		TEXTVALUE$ = IC_BINMAP::CYCLEMAP

	!************************************************************
	! Fields for the Model Code Description file
	!************************************************************

	CASE "MO_MODELCODE::MODELCODE"
		TEXTVALUE$ = MO_MODELCODE::MODELCODE

	CASE "MO_MODELCODE::DESCR"
		TEXTVALUE$ = MO_MODELCODE::DESCR

	!************************************************************
	! Fields for the Make Size Description file
	!************************************************************

	CASE "MO_MAKESIZE::MSIZE"
		TEXTVALUE$ = MO_MAKESIZE::MSIZE

	CASE "MO_MAKESIZE::DESCR"
		TEXTVALUE$ = MO_MAKESIZE::DESCR

	!************************************************************
	! Fields for the Make Type Description file
	!************************************************************

	CASE "MO_MAKETYPE::MTYPE"
		TEXTVALUE$ = MO_MAKETYPE::MTYPE

	CASE "MO_MAKETYPE::DESCR"
		TEXTVALUE$ = MO_MAKETYPE::DESCR

	!************************************************************
	! Fields for the Option Code Description file
	!************************************************************

	CASE "MO_OPTION::OPTN"
		TEXTVALUE$ = MO_OPTION::OPTN

	CASE "MO_OPTION::DESCR"
		TEXTVALUE$ = MO_OPTION::DESCR

	!************************************************************
	! Fields for the Order Category Description file
	!************************************************************

	CASE "OE_CATEGORY::ORDCAT"
		TEXTVALUE$ = OE_CATEGORY::ORDCAT

	CASE "OE_CATEGORY::DESCRIPTION"
		TEXTVALUE$ = OE_CATEGORY::DESCRIPTION

	!************************************************************
	! Fields for the Reason Codes Description file
	!************************************************************

	CASE "OE_CREASON::CREASON"
		TEXTVALUE$ = OE_CREASON::CREASON

	CASE "OE_CREASON::DESCR"
		TEXTVALUE$ = OE_CREASON::DESCR

	!************************************************************
	! Fields for the Promo Description file
	!************************************************************

	CASE "OE_PROMO::REFPROMO"
		TEXTVALUE$ = OE_PROMO_READ::REFPROMO

	CASE "OE_PROMO::DESCRIPTION"
		TEXTVALUE$ = OE_PROMO_READ::DESCRIPTION

	!************************************************************
	! Fields for the Order Type Description file
	!************************************************************

	CASE "OE_ORDERTYPE::ORDTYPE"
		TEXTVALUE$ = OE_CREASON::CREASON

	CASE "OE_ORDERTYPE::DESCRIPTION"
		TEXTVALUE$ = OE_ORDERTYPE::DESCRIPTION

	!************************************************************
	! Fields for the Shipto file
	!************************************************************

	CASE "OE_SHIPTO::NOTES1"
		TEXTVALUE$ = OE_SHIPTO::NOTES(0%)

	CASE "OE_SHIPTO::NOTES2"
		TEXTVALUE$ = OE_SHIPTO::NOTES(1%)

	CASE "OE_SHIPTO::NOTES3"
		TEXTVALUE$ = OE_SHIPTO::NOTES(2%)

	!************************************************************
	! Fields for the Product Description file
	!************************************************************

	CASE "PD_PRODUCT::PRODUCT_NUM"
		TEXTVALUE$ = PD_PRODUCT_EXAM::PRODUCT_NUM

	CASE "PD_PRODUCT::DESCRIPTION"
		TEXTVALUE$ = PD_PRODUCT_EXAM::DESCRIPTION

	CASE "PD_PRODUCT::PROD_TYPE"
		TEXTVALUE$ = PD_PRODUCT_EXAM::PROD_TYPE

	CASE "PD_PRODUCT::CATEGORY"
		TEXTVALUE$ = PD_PRODUCT_EXAM::CATEGORY

	CASE "PD_PRODUCT::UOM"
		TEXTVALUE$ = PD_PRODUCT_EXAM::UOM

	CASE "PD_PRODUCT::PACK"
		TEXTVALUE$ = PD_PRODUCT_EXAM::PACK

	CASE "PD_PRODUCT::LABEL"
		TEXTVALUE$ = PD_PRODUCT_EXAM::LABEL

	CASE "PD_PRODUCT::METHOD"
		TEXTVALUE$ = PD_PRODUCT_EXAM::METHOD

	CASE "PD_PRODUCT::BDATE"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT_EXAM::BDATE, 8%)

	CASE "PD_PRODUCT::BDATE6"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT_EXAM::BDATE, 6%)

	CASE "PD_PRODUCT::SSTATUS"
		TEXTVALUE$ = PD_PRODUCT_EXAM::SSTATUS

	CASE "PD_PRODUCT::EDATE"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT_EXAM::EDATE, 8%)

	CASE "PD_PRODUCT::EDATE6"
		TEXTVALUE$ = PRNT_DATE(PD_PRODUCT_EXAM::EDATE, 6%)

	CASE "PD_PRODUCT::SECONDARY_CODE"
		TEXTVALUE$ = PD_PRODUCT_EXAM::SECONDARY_CODE

	!************************************************************
	! Fields for the cash register file
	!************************************************************

	CASE "PS_CASHREG::CASHREG"
		TEXTVALUE$ = PS_CASHREG::CASHREG

	CASE "PS_CASHREG::DESCRS"
		TEXTVALUE$ = PS_CASHREG::DESCR

	CASE "PS_CASHREG::LOCATION"
		TEXTVALUE$ = PS_CASHREG::LOCATION

	CASE "PS_CASHREG::NOTES1"
		TEXTVALUE$ = PS_CASHREG::NOTES(0%)

	CASE "PS_CASHREG::NOTES2"
		TEXTVALUE$ = PS_CASHREG::NOTES(1%)

	CASE "PS_CASHREG::LAST_INVNUM"
		TEXTVALUE$ = PS_CASHREG::LAST_INVNUM

	!************************************************************
	! Fields for the Salesman Description file
	!************************************************************

	CASE "SA_SALESMAN::SALNAME"
		TEXTVALUE$ = SA_SALESMAN::DESCR

	!************************************************************
	! Fields for the Utility Carrier Description file
	!************************************************************


		TEXTVALUE$ = UTL_CARRIER::CODE

	CASE "UTL_CARRIER::DESCR"
		TEXTVALUE$ = UTL_CARRIER::DESCR

	!************************************************************
	! Fields for the Utility Terms Description file
	!************************************************************

	CASE "UTL_TERMS::CODE"
		TEXTVALUE$ = UTL_TERMS::CODE

	CASE "UTL_TERMS::DESCR"
		TEXTVALUE$ = UTL_TERMS::DESCR

	CASE "UTL_TERMS::DISCOUNT"
		REALVALUE = UTL_TERMS::DISCOUNT

	!************************************************************
	! Non fielded values
	!************************************************************

	CASE "LIST_PRICE"
		REALVALUE = LIST_PRICE

	CASE "REAL_PRICE"
		REALVALUE = REAL_PRICE

	CASE "BALANCE"
		REALVALUE = BALANCE

	CASE "ORIGORD_QTY"
		REALVALUE = ORIGORD_QTY

	CASE "ORD_QTY"
		REALVALUE = ORD_QTY

	CASE "SUMSHPQTY"
		REALVALUE = SUMSHPQTY

	CASE "ORDERLINE"
		REALVALUE = ORDERLINE

	CASE "ORDER_TOTAL"
		REALVALUE = ORDER_TOTAL

	CASE "ORDER_DISC"
		REALVALUE = ORDER_DISC

	CASE "ORDER_TAX"
		REALVALUE = ORDER_TAX

	CASE "SHIP_QTY"
		REALVALUE = SHIP_QTY

	CASE "SHIPLINE"
		REALVALUE = SHIPLINE

	CASE "SHIP_TOTAL"
		REALVALUE = SHIP_TOTAL

	CASE "SHIP_DISC"
		REALVALUE = SHIP_DISC

	CASE "SHIP_TAX"
		REALVALUE = SHIP_TAX

	CASE "XOUT_ASSIGN"
		TEXTVALUE$ = XOUTASSIGN$

	CASE "PAGE_NUMBER"
		REALVALUE = PAGE_NUMBER%
		TEXTVALUE$ = NUM1$(PAGE_NUMBER%)

	CASE "LINE_NUM"
		REALVALUE = LINE_NUM%
		TEXTVALUE$ = NUM1$(LINE_NUM%)

	CASE "DUEDAY1"
		TEXTVALUE$ = DUEDAYS$(1%)

	CASE "DUEDAY2"
		TEXTVALUE$ = DUEDAYS$(2%)

	CASE "DUEDAY3"
		TEXTVALUE$ = DUEDAYS$(3%)

	CASE "DUEDAY4"
		TEXTVALUE$ = DUEDAYS$(4%)

	CASE "DUEDAY5"
		TEXTVALUE$ = DUEDAYS$(5%)

	CASE "DUEDAY6"
		TEXTVALUE$ = DUEDAYS$(6%)

	CASE "DUEDAY7"
		TEXTVALUE$ = DUEDAYS$(7%)

	CASE "DUEDAY8"
		TEXTVALUE$ = DUEDAYS$(8%)

	CASE "DUEDAY9"
		TEXTVALUE$ = DUEDAYS$(9%)

	CASE "ORDER_NONSALTAX"
		REALVALUE = ORDER_NONSALTAX
		TEXTVALUE$ = NUM1$(ORDER_NONSALTAX)

	!*******************************************************************
	! Location info
	!*******************************************************************

	CASE "UTL_LOCATION::LOCATION"
		TEXTVALUE$ = UTL_LOCATION_EXAM::LOCATION

	CASE "UTL_LOCATION::LOCNAME"
		TEXTVALUE$ = UTL_LOCATION_EXAM::LOCNAME

	CASE "UTL_LOCATION::REGION"
		TEXTVALUE$ = UTL_LOCATION_EXAM::REGION

	CASE "UTL_LOCATION::LOCGROUP"
		TEXTVALUE$ = UTL_LOCATION_EXAM::LOCGROUP

	CASE "UTL_LOCATION::ADDRESS1"
		TEXTVALUE$ = UTL_LOCATION_EXAM::ADDRESS1

	CASE "UTL_LOCATION::ADDRESS2"
		TEXTVALUE$ = UTL_LOCATION_EXAM::ADDRESS2

	CASE "UTL_LOCATION::CITY"
		TEXTVALUE$ = UTL_LOCATION_EXAM::CITY

	CASE "UTL_LOCATION::STATE"
		TEXTVALUE$ = UTL_LOCATION_EXAM::STATE

	CASE "UTL_LOCATION::ZIP"
		TEXTVALUE$ = UTL_LOCATION_EXAM::ZIP

	CASE "UTL_LOCATION::COUNTY"
		TEXTVALUE$ = UTL_LOCATION_EXAM::COUNTY

	CASE "UTL_LOCATION::COUNTRY"
		TEXTVALUE$ = UTL_LOCATION_EXAM::COUNTRY

	CASE "UTL_LOCATION::PHONE"
		TEXTVALUE$ = UTL_LOCATION_EXAM::PHONE

	CASE "UTL_LOCATION::SHPADDRESS1"
		TEXTVALUE$ = UTL_LOCATION_EXAM::SHPADDRESS1

	CASE "UTL_LOCATION::SHPADDRESS2"
		TEXTVALUE$ = UTL_LOCATION_EXAM::SHPADDRESS2

	CASE "UTL_LOCATION::SHPCITY"
		TEXTVALUE$ = UTL_LOCATION_EXAM::SHPCITY

	CASE "UTL_LOCATION::SHPSTATE"
		TEXTVALUE$ = UTL_LOCATION_EXAM::SHPSTATE

	CASE "UTL_LOCATION::SHPZIP"
		TEXTVALUE$ = UTL_LOCATION_EXAM::SHPZIP

	CASE "UTL_LOCATION::SHPCOUNTY"
		TEXTVALUE$ = UTL_LOCATION_EXAM::SHPCOUNTY

	CASE "UTL_LOCATION::SHPCOUNTRY"
		TEXTVALUE$ = UTL_LOCATION_EXAM::SHPCOUNTRY

	CASE "UTL_LOCATION::SHPPHONE"
		TEXTVALUE$ = UTL_LOCATION_EXAM::SHPPHONE

	CASE "UTL_LOCATION:ADDLINE1"	! Substitute Customer Address
		TEXTVALUE$ = UTL_LOCATION_EXAM.ADDLINE$(1%)

	CASE "UTL_LOCATION:ADDLINE2"	! Substitute Customer Address
		TEXTVALUE$ = UTL_LOCATION_EXAM.ADDLINE$(2%)

	CASE "UTL_LOCATION:ADDLINE3"	! Substitute Customer Address
		TEXTVALUE$ = UTL_LOCATION_EXAM.ADDLINE$(4%)

	CASE "UTL_LOCATION:ADDLINE4"	! Substitute Customer Address
		TEXTVALUE$ = UTL_LOCATION_EXAM.ADDLINE$(4%)

	END SELECT

	TEXTVALUE$ = EDIT$(TEXTVALUE$, 8% + 128%)

	END SUB
