1	%TITLE "Post Journal"
	%SBTTL "PO_SPEC_RECOVERBATCH"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	!
	! Abstract:HELP
	!
	! Index:
	!	.x Post>Purchase Order Journal
	!	.x Purchase Order>Journal>Post
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PO_SOURCE:PO_SPEC_RECOVERBATCH/LINE
	!	$ LINK/EXE=PO_EXE: PO_SPEC_RECOVERBATCH, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PO_SPEC_RECOVERBATCH.OBJ;*
	!
	! Author:
	!
	!	11/08/96 - Kevin Handy
	!
	! Modification history:
	!
	!	05/22/97 - Kevin Handy
	!		Pull 2nd address out of vendor file.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/05/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include constants and and some functions
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERJOUR.HB"
	MAP (PO_ORDERJOUR)		PO_ORDERJOUR_CDD	PO_ORDERJOUR

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERLINE.HB"
	MAP (PO_ORDERLINE)		PO_ORDERLINE_CDD	PO_ORDERLINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERSLINE.HB"
	MAP (PO_ORDERSLINE)		PO_ORDERSLINE_CDD	PO_ORDERSLINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE)	PO_REG_LINE_CDD		PO_REG_LINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.HB"
	MAP (PO_REG_SUB_LINE)	PO_REG_SUB_LINE_CDD	PO_REG_SUB_LINE

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD		AP_VENDOR

	%PAGE

	!**************************************************************
	! Get some stuff done before we start
	!**************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	PRINT "BATCH NUMBER TO CREATE (XX) ";
	LINPUT BATCH_NO$

	PRINT "POSTING BATCH NUMBER (XXXXXX) ";
	LINPUT POSTED_BATCH$

200	!
	! Open register header
	!
	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.OPN"

210	!
	! Open register header
	!
	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_SUB_LINE.OPN"

300	!
	! Open ORDER JOURNAL
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERJOUR.CRE"
	USE
		FILENAME$ = "PO_ORDERJOUR"
		CONTINUE HelpError
	END WHEN

310	!
	! Open ORDER LINE
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERLINE.CRE"
	USE
		FILENAME$ = "PO_ORDERLINE"
		CONTINUE HelpError
	END WHEN

315	!
	! Open ORDER SUBLINE
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PO.OPEN]PO_ORDERSLINE.CRE"
	USE
		FILENAME$ = "PO_ORDERSLINE"
		CONTINUE HelpError
	END WHEN

320	!
	! Vendor file
	!
	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"

	%PAGE

1000	!*******************************************************************
	! Loop through the register line file
	!*******************************************************************

	FIND #PO_REG_LINE.CH%, KEY #3% GE POSTED_BATCH$

	PRINT "Starting PO_REG_LINE"

1100	WHEN ERROR IN
		GET #PO_REG_LINE.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 2000 IF ERR = 11%
		CONTINUE HelpError
	END WHEN

	GOTO 1100 UNLESS PO_REG_LINE::BATCH = POSTED_BATCH$

	PRINT "   "; PO_REG_LINE::PO; " "; PO_REG_LINE::PO_LINE

1200	!
	! Create an orderline record
	!
	PO_ORDERLINE::PO		= PO_REG_LINE::PO
	PO_ORDERLINE::PO_LINE		= PO_REG_LINE::PO_LINE
	PO_ORDERLINE::OUR_PRODUCT	= PO_REG_LINE::PRODUCT
	PO_ORDERLINE::OUR_UOM		= PO_REG_LINE::UOM
	PO_ORDERLINE::VEN_PRODUCT	= ""
	PO_ORDERLINE::DESCRIPTION	= PO_REG_LINE::DESCRIPTION
	PO_ORDERLINE::VEN_PRICE		= 0.0

	PUT #PO_ORDERLINE.CH%

1300	!
	! Try for a header for this po
	!
	WHEN ERROR IN
		GET #PO_ORDERJOUR.CH%, KEY #0% EQ PO_REG_LINE::PO
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 1400 IF ERR = 155%
		CONTINUE HelpError
	END WHEN

	GOTO 1100

1400	!
	! Create a header
	!
	AP_VENDOR::VENNUM = PO_REG_LINE::VENDOR
	AP_VENDOR::VENNAM = ""
	AP_VENDOR::POADD1 = ""
	AP_VENDOR::POADD2 = ""
	AP_VENDOR::POCITY = ""
	AP_VENDOR::POSTATE = ""
	AP_VENDOR::POZIP = ""
	AP_VENDOR::POCOUNTRY = ""

	WHEN ERROR IN
		GET #AP_VENDOR.CH%, &
			KEY #0% EQ PO_REG_LINE::VENDOR, &
			REGARDLESS
	USE
		CONTINUE 1410
	END WHEN

1410	PO_ORDERJOUR::PO		= PO_REG_LINE::PO
	PO_ORDERJOUR::POTYPE		= PO_REG_LINE::PO_TYPE
	PO_ORDERJOUR::PODATE		= PO_REG_LINE::ORDDATE
	PO_ORDERJOUR::VENDOR		= PO_REG_LINE::VENDOR
	PO_ORDERJOUR::BUYER		= ""
	PO_ORDERJOUR::SALESMAN		= ""
	PO_ORDERJOUR::TERMS		= ""
	PO_ORDERJOUR::CARRIER		= ""
	PO_ORDERJOUR::FOB		= ""
	PO_ORDERJOUR::ACKNOW		= ""
	PO_ORDERJOUR::COL_PPD		= ""
	PO_ORDERJOUR::NOTE(I%)		= "" &
		FOR I% = 0% TO 3%
	PO_ORDERJOUR::OPERATOR		= ""
	PO_ORDERJOUR::PRINTFORM		= ""
	PO_ORDERJOUR::FROMLOCATION	= PO_REG_LINE::FROMLOCATION
	PO_ORDERJOUR::TONAME		= AP_VENDOR::VENNAM
	PO_ORDERJOUR::TOADD1		= AP_VENDOR::POADD1
	PO_ORDERJOUR::TOCITY		= AP_VENDOR::POCITY
	PO_ORDERJOUR::TOSTATE		= AP_VENDOR::POSTATE
	PO_ORDERJOUR::TOZIP		= AP_VENDOR::POZIP
	PO_ORDERJOUR::TOCOUNTRY		= AP_VENDOR::POCOUNTRY
	PO_ORDERJOUR::BATCH		= ""
	PO_ORDERJOUR::NEW		= ""

	PUT #PO_ORDERJOUR.CH%

	PRINT "  Header Created"

	GOTO 1100

	%PAGE

	!*******************************************************************
	! Now scan through the sub-lines
	!*******************************************************************

2000	FIND #PO_REG_SUB_LINE.CH%, KEY #1% GE POSTED_BATCH$

	PRINT
	PRINT "Starting sub-lines"

2100	WHEN ERROR IN
		GET #PO_REG_SUB_LINE.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 32000
	END WHEN

	GOTO 2100 IF PO_REG_SUB_LINE::BATCH <> POSTED_BATCH$

	PRINT PO_REG_SUB_LINE::PO; " "; PO_REG_SUB_LINE::PO_LINE

2200	PO_ORDERSLINE::PO		= PO_REG_SUB_LINE::PO
	PO_ORDERSLINE::PO_LINE		= PO_REG_SUB_LINE::PO_LINE
	PO_ORDERSLINE::OUR_QTY		= PO_REG_SUB_LINE::QTY
	PO_ORDERSLINE::RECEIVEDATE	= PO_REG_SUB_LINE::ACTION_DATE
	PO_ORDERSLINE::GL_ACCOUNT	= PO_REG_SUB_LINE::ACCOUNT
	PO_ORDERSLINE::SUBACCT		= PO_REG_SUB_LINE::SUBACCT
	PO_ORDERSLINE::NOTES(I%)	= "" &
		FOR I% = 0% TO 1%

	PUT #PO_ORDERSLINE.CH%

2300	!
	! Try to recover line info
	!
	WHEN ERROR IN
		GET #PO_ORDERLINE.CH%, KEY #0% EQ PO_REG_SUB_LINE::PO + &
			PO_REG_SUB_LINE::PO_LINE
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 2400
	END WHEN

2350	PO_ORDERLINE::VEN_PRICE		= PO_REG_SUB_LINE::PRICE

	UPDATE #PO_ORDERLINE.CH%

2400	GOTO 2100

	%PAGE

 HelpError:
	PRINT "Error"; ERR; "at line"; ERL; " FILENAME "; FILENAME$
	GOTO 32000

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	!
	! Trap untrapped errors
	!
	PRINT "Error"; ERR; "at line"; ERL; " FILENAME "; FILENAME$

	ON ERROR GOTO 0

32000	!******************************************************************
	! End of posting program PO_SPEC_RECOVERBATCH
	!******************************************************************
	END

