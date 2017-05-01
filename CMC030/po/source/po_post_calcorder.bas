1	%TITLE "Post Load Journal"
	%SBTTL "PO_POST_CALCORDER"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1994 BY
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
	! ID:PO004
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This post generates a purchase order journal from the data in the
	!	Purchase Order Load Journal.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_POST_CALCORDER/LINE
	!	$ LINK/EXE=PO_EXE: PO_POST_CALCORDER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_POST_CALCORDER.OBJ;*
	!
	! Author:
	!
	!	05/25/94 - Kevin Handy
	!
	! Modification History:
	!
	!	11/02/94 - Kevin Handy
	!		Fix bug in generation of GL Account number. Error
	!		caused by passing part number instead of product
	!		type.
	!
	!	11/04/94 - Kevin Handy
	!		Modified to adjust running balance.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	09/18/95 - Kevin Handy
	!		Lose extra include of CONSTANTS.INC
	!		Lose unecessary definitions.
	!		Format source closer to 80 columns.
	!
	!	09/09/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/01/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Add more error trapping.
	!
	!	90/18/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PO.OPEN]PO_CALCORDER.HB"
	MAP (PO_CALCORDER) PO_CALCORDER_CDD PO_CALCORDER

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION) UTL_LOCATION_CDD UTL_LOCATION

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR) AP_VENDOR_CDD AP_VENDOR

	%INCLUDE "SOURCE:[PO.OPEN]PO_CONTROL.HB"
	MAP (PO_CONTROL) PO_CONTROL_CDD PO_CONTROL

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERJOUR.HB"
	MAP (PO_ORDERJOUR) PO_ORDERJOUR_CDD PO_ORDERJOUR

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERLINE.HB"
	MAP (PO_ORDERLINE) PO_ORDERLINE_CDD PO_ORDERLINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERSLINE.HB"
	MAP (PO_ORDERSLINE) PO_ORDERSLINE_CDD PO_ORDERSLINE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT) PD_PRODUCT_CDD PD_PRODUCT

	%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.HB"
	MAP (PO_PARTCROSS) PO_PARTCROSS_CDD PO_PARTCROSS

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	DECLARE	PD_ACCOUNT_CDD		PD_ACCOUNT_READ

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION PD_READ_ACCOUNT
	EXTERNAL LONG	FUNCTION IC_WRIT_35BALANCE

	DECLARE STRING	TEXT, &
		WLDCRD, &
		LYT_LINE

	%PAGE

	!
	! Give text description of formula parts
	!
	DEF FNFORM_TEXT$(X$)

		SELECT(X$)
		CASE "A"
			FNFORM_TEXT$ = "   Allocated"
		CASE "H"
			FNFORM_TEXT$ = "     On Hand"
		CASE "O"
			FNFORM_TEXT$ = "    On Order"
		CASE "S"
			FNFORM_TEXT$ = "      Safety"
		CASE ELSE
			FNFORM_TEXT$ = "            "
		END SELECT

	FNEND

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) PO Batch Number\*
	!
	! Index:
	!
	!--


	PODATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(1%))

	!++
	! Abstract:FLD02
	!
	! Index:
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!
	! Index:
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_CALCORDER.OPN"
	USE
		FILENAME$ = "PO_CALCORDER"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_CONTROL.MOD"

		GET #PO_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		FILENAME$ = "PO_CONTROL"
		CONTINUE HelpError
	END WHEN

340	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERJOUR.CRE"
	USE
		FILENAME$ = "PO_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

350	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERLINE.CRE"
	USE
		FILENAME$ = "PO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

360	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERSLINE.CRE"
	USE
		FILENAME$ = "PO_ORDERSLINE"
		CONTINUE HelpError
	END WHEN

370	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

380	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_PARTCROSS.OPN"
	USE
		FILENAME$ = "PO_PARTCROSS"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Purchase  Order  Load  List"
	TITLE$(2%) = "Purchase Order System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TEXT = "Product             Order"
	FOR I% = 1% TO 5%
		TEXT = TEXT + &
			FNFORM_TEXT$(MID(PO_CONTROL::LOAD_FORMULA, I%, 1%))
	NEXT I%

	TITLE$(4%) = TRM$(TEXT)
	TITLE$(5%) = "."

	!
	! Line layouts
	!
	LYT_LINE = "$POType:002,$TypeDescription:053"

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************

	THIS_LOCATION$ = "~~~~~~~~"
	THIS_VENDOR$ = ""
	THIS_LINE% = 0%

	TOTAL_HEADER% = 0%
	TOTAL_LINE% = 0%
	TOTAL_SLINE% = 0%

	WHEN ERROR IN
		RESET #PO_CALCORDER.CH%
	USE
		FILENAME$ = "PO_CALCORDER"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PO_CALCORDER.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PO_CALCORDER"
		CONTINUE HelpError
	END WHEN

	!
	! Print new title if vendor/location changes
	!
	IF (THIS_LOCATION$ <> PO_CALCORDER::LOCATION) OR &
		(THIS_VENDOR$ <> PO_CALCORDER::VENDOR)
	THEN
		GOSUB AddNewHeader
	END IF

	!
	! Process one line
	!
	GOSUB AddNewLine

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17800	TEXT = "Records Added:"
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT, 0%)

	TEXT = "      Orders: " + FORMAT$(TOTAL_HEADER%, "####")
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT, 0%)

	TEXT = "       Lines: " + FORMAT$(TOTAL_LINE%, "####")
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT, 0%)

	TEXT = "   Sub Lines: " + FORMAT$(TOTAL_SLINE%, "####")
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT, 0%)

	CLOSE PO_CALCORDER.CH%

17890 !	WHEN ERROR IN
 !		KILL PO_CALCORDER.NAME$
 !	USE
 !		FILENAME$ = "PO_CALCORDER"
 !		CONTINUE HelpError
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(PO_CALCORDER.NAME$ + ";*")

 ExitProgram:
17900	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 AddNewHeader:
18000	!*******************************************************************
	! Create a new Header
	!*******************************************************************

	IF (UTL_LOCATION::LOCATION <> PO_CALCORDER::LOCATION)
	THEN
		WHEN ERROR IN
			GET #UTL_LOCATION.CH%, &
				KEY #0% EQ PO_CALCORDER::LOCATION, &
				REGARDLESS
		USE
			UTL_LOCATION::LOCNAME = ""
			UTL_LOCATION::LOCATION = ""

			CONTINUE 18010
		END WHEN
	END IF

18010	IF (AP_VENDOR::VENNUM <> PO_CALCORDER::VENDOR)
	THEN
		WHEN ERROR IN
			GET #AP_VENDOR.CH%, &
				KEY #0% EQ PO_CALCORDER::VENDOR, &
				REGARDLESS
		USE
			AP_VENDOR::VENNUM = ""
			AP_VENDOR::VENNAM = ""

			CONTINUE 18020
		END WHEN
	END IF

18020	!
	! Allocate a PO Number from the control file
	!
	GET #PO_CONTROL.CH%, RECORD 1%

	V% = FUNC_INCREMENT(PO_CONTROL::LAST_PO)

	UPDATE #PO_CONTROL.CH%

18080	!
	! Fill in a PO Header
	!
	PO_ORDERJOUR::PO	= PO_CONTROL::LAST_PO
	PO_ORDERJOUR::POTYPE	= ""
	PO_ORDERJOUR::PODATE	= PODATE$
	PO_ORDERJOUR::VENDOR	= PO_CALCORDER::VENDOR
	PO_ORDERJOUR::BUYER	= ""
	PO_ORDERJOUR::SALESMAN	= ""
	PO_ORDERJOUR::TERMS	= ""
	PO_ORDERJOUR::CARRIER	= ""
	PO_ORDERJOUR::FOB	= ""
	PO_ORDERJOUR::ACKNOW	= ""
	PO_ORDERJOUR::COL_PPD	= ""
	PO_ORDERJOUR::NOTE(I%)	= "" FOR I% = 0% TO 3%
	PO_ORDERJOUR::OPERATOR	= ""
	PO_ORDERJOUR::PRINTFORM	= "N"
	PO_ORDERJOUR::FROMLOCATION = PO_CALCORDER::LOCATION
	PO_ORDERJOUR::TONAME	= UTL_LOCATION::LOCNAME
	PO_ORDERJOUR::TOADD1	= UTL_LOCATION::SHPADDRESS1
	PO_ORDERJOUR::TOADD2	= UTL_LOCATION::SHPADDRESS2
	PO_ORDERJOUR::TOCITY	= UTL_LOCATION::SHPCITY
	PO_ORDERJOUR::TOSTATE	= UTL_LOCATION::SHPSTATE
	PO_ORDERJOUR::TOZIP	= UTL_LOCATION::SHPZIP
	PO_ORDERJOUR::TOCOUNTRY	= UTL_LOCATION::SHPCOUNTRY
	PO_ORDERJOUR::BATCH	= ""
	PO_ORDERJOUR::NEW	= "Y"

	!
	! Add header to file
	!
	WHEN ERROR IN
		PUT #PO_ORDERJOUR.CH%
	USE
		FILENAME$ = "PO_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

	TOTAL_HEADER% = TOTAL_HEADER% + 1%

	!
	! Remember that we created this header already
	!
	THIS_LOCATION$ = PO_CALCORDER::LOCATION
	THIS_VENDOR$ = PO_CALCORDER::VENDOR
	THIS_LINE% = 0%

	RETURN

 AddNewLine:
	!*******************************************************************
	! Add a new set of line records to the PO journal
	!*******************************************************************

	!
	! Increment line count
	!
	THIS_LINE% = THIS_LINE% + 1%

	!
	! Create order line
	!
	PO_ORDERLINE::PO	= PO_ORDERJOUR::PO
	PO_ORDERLINE::PO_LINE	= FORMAT$(THIS_LINE%, "<0>###")
	PO_ORDERLINE::OUR_PRODUCT = PO_CALCORDER::PRODUCT
	PO_ORDERLINE::VEN_PRODUCT = ""
	PO_ORDERLINE::OUR_UOM	= ""
	PO_ORDERLINE::DESCRIPTION = ""
	PO_ORDERLINE::VEN_PRICE	= PO_CALCORDER::COST

	!
	! Create order subline
	!
	PO_ORDERSLINE::PO	= PO_ORDERJOUR::PO
	PO_ORDERSLINE::PO_LINE	= PO_ORDERLINE::PO_LINE
	PO_ORDERSLINE::OUR_QTY	= PO_CALCORDER::ORDER
	PO_ORDERSLINE::RECEIVEDATE = ""
	PO_ORDERSLINE::GL_ACCOUNT = ""
	PO_ORDERSLINE::SUBACCT	= ""
	PO_ORDERSLINE::NOTES(I%) = "" FOR I% = 0% TO 1%

18110	!
	! Try to get information from product file
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ PO_CALCORDER::PRODUCT, &
			REGARDLESS
	USE
		PD_PRODUCT::PROD_TYPE = ""
		CONTINUE 18120
	END WHEN

	!
	! Fill in some more blanks
	!
	PO_ORDERLINE::OUR_UOM = PD_PRODUCT::UOM
	PO_ORDERLINE::DESCRIPTION = PD_PRODUCT::DESCRIPTION

18120	!
	! Try to get info from part cross-reference
	!
	WHEN ERROR IN
		GET #PO_PARTCROSS.CH%, &
			KEY #1% EQ PO_CALCORDER::VENDOR + PO_CALCORDER::PRODUCT, &
			REGARDLESS
	USE
		CONTINUE 18130
	END WHEN

	!
	! Fill in a few more blanks
	!
	PO_ORDERLINE::VEN_PRODUCT = PO_PARTCROSS::VENPROD

	IF PO_PARTCROSS::LEAD <> 0%
	THEN
		PO_ORDERSLINE::RECEIVEDATE = &
			DATE_INVDCODE(DATE_DAYCODE(PODATE$) + &
			PO_PARTCROSS::LEAD)
	END IF

18130	!
	! Look up account numbers
	!
	IF PD_READ_ACCOUNT( &
		PO_CALCORDER::LOCATION, &
		PD_PRODUCT::PROD_TYPE, &
		PD_ACCOUNT_READ) = CMC$_NORMAL
	THEN
		PO_ORDERSLINE::GL_ACCOUNT = PD_ACCOUNT_READ::INVACCT
	END IF

18180	!
	! Add to file
	!
	PUT #PO_ORDERLINE.CH%
	TOTAL_LINE% = TOTAL_LINE% + 1%

18190	!
	! Add to file
	!
	PUT #PO_ORDERSLINE.CH%
	TOTAL_SLINE% = TOTAL_SLINE% + 1%

	!
	! Adjust running inventory balance
	!
	V% = IC_WRIT_35BALANCE (PO_ORDERLINE::OUR_PRODUCT, &
		PO_ORDERJOUR::FROMLOCATION, "PO", &
		PO_ORDERSLINE::OUR_QTY)

	RETURN

 HelpError:
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

	%Page

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
