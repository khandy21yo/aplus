1	%TITLE "Convert Pacific Pride LOCBIL File to PP_DAILY"
	%SBTTL "PP_SPEC_CONVERTLOCBIL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho.  83402
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
	! ID:PP
	!
	! Abstract:HELP
	!
	! Index:
	!
	! Option:
	!
	!
	! Author:
	!
	!	02/03/93 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_SPEC_CONVERTLOCBIL/LINE
	!	$ LINK/EXECUTABLE=PP_EXE:*.EXE PP_SPEC_CONVERTLOCBIL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_SPEC_CONVERTLOCBIL.OBJ;*
	!
	! Modification history:
	!
	!	03/03/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/28/93 - Kevin Handy
	!		Removed output of each and every record to
	!		listing file.
	!
	!	06/28/93 - Kevin Handy
	!		Generate Subtotals by sale type.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	09/27/96 - Kevin Handy
	!		Modifications for T3 records.
	!
	!	10/21/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/97 - Kevin Handy
	!		Better error message for 5110.
	!		Fix bug where wasn't storing customer number
	!		after finding it on T2,T3 records.
	!		Open LBxxxx.xxT file as read/only.
	!		Don't mix TR:: with T2:: or T3:: record types.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/17/99 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	03/17/99 - Kevin Handy
	!		Add code for new T4, C4 records.
	!
	!	03/29/99 - Kevin Handy
	!		Finish setting up T4, C4.
	!
	!	06/22/99 - Kevin Handy
	!		Lose eztra blank page on output
	!
	!	06/29/99 - Kevin Handy
	!		Added device file lookup for LB files.
	!
	!	07/23/99 - Kevin Handy
	!		Reformat source code
	!
	!	07/07/2000 - Kevin Handy
	!		Use PP_LOCBIL.DEV$ instead of PP_LOCBIL$
	!
	!	07/20/2000 - Kevin Handy
	!		Spell "PP_LOCBIL" properly in getdev call.
	!
	!	09/18/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!
	!	12/16/2005 - Kevin Handy
	!		Make use of PD_SUBSTITUTE file
	!
	!	01/24/2006 - Kevin Handy
	!		Oregon Tax. Have to handle specially.
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

	%INCLUDE "SOURCE:[PP.OPEN]PP_CONTROL.HB"
	MAP	(PP_CONTROL)	PP_CONTROL_CDD	PP_CONTROL

	%INCLUDE "SOURCE:[PP.OPEN]PP_DAILY.HB"
	MAP	(PP_DAILY)	PP_DAILY_CDD	PP_DAILY

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP	(AR_35CUSTOM)	AR_35CUSTOM_CDD	AR_35CUSTOM

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.HB"
	MAP	(PP_CARD)	PP_CARD_CDD	PP_CARD

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARDEXEMPT.HB"
	MAP	(PP_CARDEXEMPT)	PP_CARDEXEMPT_CDD	PP_CARDEXEMPT

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP	(PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT

	%INCLUDE "SOURCE:[PD.OPEN]PD_SUBSTITUTE.HB"
	MAP	(PD_SUBSTITUTE)	PD_SUBSTITUTE_CDD	PD_SUBSTITUTE

	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.HB"
	MAP	(PP_SITE)	PP_SITE_CDD	PP_SITE

	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE_PRODUCT.HB"
	MAP	(PP_SITE_PRODUCT)	PP_SITE_PRODUCT_CDD	PP_SITE_PRODUCT

	RECORD SUBTOTAL_CDD
		STRING	BUYFRAN = 3%	! Location
		STRING	PRODUCT = 14%	! Product
		LONG	COUNTER		! Number of lines
		REAL	QUANTITY	! Number of gallons
		REAL	PRICE		! Total paid
	END RECORD

	DIM SUBTOTAL_CDD SUBTOTAL(300%)

	!
	! Hard coded rates
	!
	DECLARE GFLOAT CONSTANT OREGON_TAX = 0.24

	!
	! Local bill 'TR' record
	!
	RECORD TR_CDD
		STRING RECCODE = 2%
		STRING SELFRAN = 3%
		STRING SITECODE = 2%
		STRING SITETYPE = 1%
		STRING K = 2%
		STRING STAXRATE = 5%
		STRING CAPDATE = 6%
		STRING CAPTIME = 4%
		STRING ICBDATE = 6%
		STRING POSTBNUM = 4%
		STRING TRANSOURCE = 1%
		STRING EDITACT = 1%
		STRING SELLPRICE = 8%
		STRING TRANCOST = 8%
		STRING TRNTYPE = 2%
		STRING JULIANDAY = 3%
		STRING TRNTIME = 4%
		STRING RSTATION = 1%
		STRING DRIVER = 6%
		STRING VEHICLE = 6%
		STRING BUYFRAN = 3%
		STRING IDENTITY = 6%
		STRING MISCKEYB = 9%
		STRING ODOM = 6%
		STRING TRNNUM = 4%
		STRING PUMP = 2%
		STRING PRDNUM = 2%
		STRING PRDSUB = 3%
		STRING QUANTITY = 8%
		STRING TRANSEQ = 1%
		STRING TRNDATE = 6%
		STRING UNIT = 2%
		STRING K1 = 1%
	END RECORD

	MAP (TRBUFFER) TR_CDD TR
	MAP (TRBUFFER) TRBUFFER$ = 128%

	!
	! Local bill 'T2' record
	!
	RECORD T2_CDD
		STRING RECCODE = 2%
		STRING SELFRAN = 3%
		STRING SITECODE = 2%
		STRING SITETYPE = 1%
		STRING JUNK = 2%
		STRING STAXRATE = 5%
		STRING CAPDATE = 6%
		STRING CAPTIME = 4%
		STRING ICBDATE = 6%
		STRING POSTBNUM = 4%
		STRING TRANSOURCE = 1%
		STRING EDITACT = 1%
		STRING SELLPRICE = 8%
		STRING TRANCOST = 8%
		STRING TRNTYPE = 2%
		STRING JULIANDAY = 3%
		STRING TRNTIME = 4%
		STRING RSTATION = 1%
		STRING DRIVER = 7%
		STRING VEHICLE = 7%
		STRING BUYFRAN = 3%
		STRING IDENTITY = 6%
		STRING MISCKEYB = 9%
		STRING ODOM = 6%
		STRING TRNNUM = 4%
		STRING PUMP = 2%
		STRING PRDNUM = 2%
		STRING PRDSUB = 3%
		STRING QUANTITY = 8%
		STRING TRANSEQ = 1%
		STRING TRNDATE = 6%
		STRING UNIT = 1%
	END RECORD

	!
	! Local bill 'T3' record
	!
	RECORD T3_CDD
		STRING RECCODE = 2%
		STRING SELFRAN = 3%
		STRING SITECODE = 3%	! *Changed From T2
		STRING SITETYPE = 1%
		STRING JUNK = 1%	! *Changed From T2
		STRING STAXRATE = 5%
		STRING CAPDATE = 6%
		STRING CAPTIME = 4%
		STRING ICBDATE = 6%
		STRING POSTBNUM = 4%
		STRING TRANSOURCE = 1%
		STRING EDITACT = 1%
		STRING SELLPRICE = 8%
		STRING TRANCOST = 8%
		STRING TRNTYPE = 2%	! TranCode TranType
		STRING JULIANDAY = 3%
		STRING TRNTIME = 4%
		STRING RSTATION = 1%
		STRING DRIVER = 7%
		STRING VEHICLE = 7%
		STRING BUYFRAN = 3%
		STRING IDENTITY = 6%
		STRING MISCKEYB = 9%
		STRING ODOM = 6%
		STRING TRNNUM = 4%
		STRING PUMP = 2%
		STRING PRDNUM = 2%
		STRING PRDSUB = 3%
		STRING QUANTITY = 8%
		STRING TRANSEQ = 1%
		STRING TRNDATE = 6%
		STRING UNIT = 1%
	END RECORD

	!
	! Local bill 'T4' record
	!
	RECORD T4_CDD
		STRING RECCODE = 2%
		STRING SELFRAN = 3%
		STRING SITECODE = 3%
		STRING SITETYPE = 1%
		STRING TRANSOURCE = 1%	! *Change from T3 starts here
		STRING TRNDATE = 8%	! Format now YYYYMMDD
		STRING TRNTIME = 4%
		STRING CAPDATE = 8%	! Format now YYYYMMDD
		STRING CAPTIME = 4%
		STRING DRIVER = 7%
		STRING VEHICLE = 7%
		STRING BUYFRAN = 3%
		STRING IDENTITY = 6%
		STRING MISCKEYB = 9%
		STRING ODOM = 6%
		STRING TRNNUM = 4%
		STRING TRANSEQ = 1%
		STRING PUMP = 2%
		STRING PRDNUM = 2%
		STRING PRDSUB = 3%
		STRING QUANTITY = 8%
		STRING UNIT = 1%
		STRING SELLPRICE = 8%
		STRING TRANCOST = 8%
		STRING STAXRATE = 5%
		STRING ICBDATE = 8%	! Now YYYYMMDD
		STRING POSTBNUM = 4%
	END RECORD

	MAP (TRBUFFER) T2_CDD T2
	MAP (TRBUFFER) T3_CDD T3
	MAP (TRBUFFER) T4_CDD T4

	!
	! Local bill 'SM' record
	!
	RECORD SM_CDD
		STRING RECCODE = 02%
		STRING XOPTION = 01%
		STRING HOST = 03%
		STRING XTYPE = 01%
		STRING CODE = 02%
		STRING ADDRESS = 35%
		STRING CITY = 20%
		STRING STATE = 02%
	END RECORD

	MAP (TRBUFFER) SM_CDD SM

	!
	! Local bill 'SD' Site Detail record
	!
	RECORD SD_CDD
		STRING RECCODE = 02%
		STRING XOPTION = 01%
		STRING HOST = 03%
		STRING XTYPE = 01%
		STRING CODE = 02%
		STRING EFFDATE = 06%
		STRING PRODUCT = 02%
		STRING FETAX = 05%
		STRING STTAX = 05%
		STRING COTAX = 05%
		STRING CITAX = 05%
		STRING SATAX = 05%
		STRING SATYPE = 01%
	END RECORD

	MAP (TRBUFFER) SD_CDD SD

	!
	! Local bill 'D3' Site Detail record
	!
	RECORD D3_CDD
		STRING RECCODE = 02%		! D3
		STRING XOPTION = 01%		! A)dd C)hange D)elete
		STRING HOST = 03%		! Host number
		STRING XTYPE = 01%		! Site Type
		STRING CODE = 02%		! Site Code
		STRING EFFDATE = 06%		! Effective date
		STRING PRODUCT = 02%		! Product Code
		STRING FETAX = 05%		! Federal tax per unit
		STRING STTAX = 05%		! State exise tax #1 per unit
		STRING COTAX = 05%		! County tax per unit
		STRING CITAX = 05%		! City tax per unit
		STRING SATAX = 05%		! Sales tax rate
		STRING SATYPE = 01%		! Sales tax type
		STRING ST2TAX = 05%		! State exise tax #2 per unit
		STRING FETYPE = 01%		! Federal Tax Type
		STRING STTYPE = 01%		! State tax #1 type
		STRING COTYPE = 01%		! County tax type
		STRING CITYPE = 01%		! City tax type
		STRING ST2TYPE = 01%		! State tax #2 type
	END RECORD

	MAP (TRBUFFER) D3_CDD D3

	!
	! Local bill 'CT' Record
	!
	RECORD CT_CDD
		STRING RECCODE = 02%
		STRING XDATE = 06%
		STRING XTIME = 04%
		STRING TRCOUNT = 06%
		STRING TRQUANT = 11%
		STRING HMCOUNT = 06%
		STRING SMCOUNT = 06%
		STRING SDCOUNT = 06%
		STRING PRCOUNT = 06%
		STRING XPCOUNT = 06%
		STRING CACOUNT = 06%
		STRING CAHASH = 11%
	END RECORD

	!
	! Local bill 'C4' Record
	!
	RECORD C4_CDD
		STRING RECCODE = 02%
		STRING XDATE = 08%	! Format now YYYYMMDD
		STRING XTIME = 04%
		STRING TRCOUNT = 06%
		STRING TRQUANT = 11%
		STRING HMCOUNT = 06%
		STRING SMCOUNT = 06%
		STRING SDCOUNT = 06%
		STRING PRCOUNT = 06%
		STRING XPCOUNT = 06%
		STRING CACOUNT = 06%
		STRING CAHASH = 11%
		STRING TRANDATE = 8%	! New Field
	END RECORD

	MAP (TRBUFFER) CT_CDD CT
	MAP (TRBUFFER) C4_CDD C4

	%PAGE

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(0%))

	!++
	! Abstract:FLD01
	!
	! Index:
	!
	!--


	!
	! Initialize totals
	!
	TRQUANT = 0.0
	TRAMT = 0.0
	TRCOUNT% = 0%
	TRREC% = 0%
	SMCOUNT% = 0%
	SMREC% = 0%
	SDCOUNT% = 0%
	SDREC% = 0%


100	USE1$ = "'LL 'L ' 'LLL 'LLLLLLL " + &
		"'LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL " + &
		"'LLLLLLLLLLLLLLLLLLL 'L #.##### #.##### " + &
		"#.##### #.##### #.#####    '     " &

300	!
	! Open control file and get control record
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_CONTROL.OPN"
	USE
		FILENAME$ = "PP_CONTROL"
		CONTINUE HelpError
	END WHEN

	WHEN ERROR IN
		GET #PP_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		FILENAME$ = "PP_CONTROL"
		CONTINUE HelpError
	END WHEN

	HOST$ = PP_CONTROL::HOST_NUM

	CLOSE #PP_CONTROL.CH%

	CALL ASSG_FREECHANNEL(PP_CONTROL.CH%)

320	!
	! Kill any existing daily file
	!
	CALL READ_DEVICE("PP_DAILY", PP_DAILY.DEV$, STAT%)

 !	WHEN ERROR IN
 !		KILL PP_DAILY.DEV$ + "PP_DAILY_" + BATCH_NO$ + ".JRL"
 !	USE
 !		CONTINUE 330 IF ERR = 5%
 !		FILENAME$ = "PP_DAILY"
 !		CONTINUE HelpError
 !	END WHEN

	SMG_STATUS% = LIB$DELETE_FILE(PP_DAILY.DEV$ + &
		"PP_DAILY_" + BATCH_NO$ + ".JRL;*")

330	!
	! Create new daily file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_DAILY.CRE"
	USE
		FILENAME$ = "PP_DAILY"
		CONTINUE HelpError
	END WHEN

340	!
	! Open customer file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

342	!
	! Open card file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.OPN"
	USE
		FILENAME$ = "PP_CARD"
		CONTINUE HelpError
	END WHEN

344	!
	! Open card exemption file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_CARDEXEMPT.OPN"
	USE
		FILENAME$ = "PP_CARDEXEMPT"
		CONTINUE HelpError
	END WHEN

350	!
	! Inventory Product file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

355	!
	! Inventory Product Substitute file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_SUBSTITUTE.OPN"
	USE
		FILENAME$ = "PD_SUBSTITUTE"
		CONTINUE HelpError
	END WHEN

360	!
	! Site file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.MOD"
	USE
		FILENAME$ = "PP_SITE"
		CONTINUE HelpError
	END WHEN

362	!
	! Site file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_SITE_PRODUCT.MOD"
	USE
		FILENAME$ = "PP_SITE_PRODUCT"
		CONTINUE HelpError
	END WHEN

370	!
	! LOCBIL file
	!
	CALL ASSG_CHANNEL(LOCBIL.CH%, STAT%)
	CALL READ_DEVICE('PP_LOCBIL', PP_LOCBIL.DEV$, STAT%)
	WHEN ERROR IN
		OPEN PP_LOCBIL.DEV$ + "LB" + MID(BATCH_NO$, 5%, 4%) + "." + &
			MID(BATCH_NO$, 3%, 2%) + "T" &
			FOR INPUT AS FILE LOCBIL.CH%, &
			ACCESS READ, ALLOW MODIFY
	USE
		FILENAME$ = "PP_LOCBIL"
		CONTINUE HelpError
	END WHEN

1000	!*******************************************************************
	! Restart point
	!*******************************************************************

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Convert LOCBIL File " + BATCH_NO$
	TITLE$(2%) = ""

	!
	! Display Heading
	!
	TITLE$(3%) = ""

	%PAGE

1030	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************


2000	!*******************************************************************
	! CONVERT
	!*******************************************************************

2005	!
	! Read one line in from source file
	!
	WHEN ERROR IN
		INPUT LINE #LOCBIL.CH%, RAW$
	USE
		CONTINUE ExitTotal
	END WHEN

	TRBUFFER$ = RAW$

	!
	! Handle the various record types
	!
	REC_CODE$ = TR::RECCODE

	SELECT REC_CODE$

	CASE "TR"
		GOSUB 4000

	CASE "T2"
		GOSUB 5000

	CASE "T3"
		GOSUB 5100

	CASE "T4"
		GOSUB 5200

	CASE "SM"
		GOSUB 6000

	CASE "SD"
		GOSUB 7000

	CASE "D3"
		GOSUB 7500

	CASE "CT"
		GOSUB 8000

	CASE "C4"
		GOSUB 8100

	END SELECT

	GOTO 2005

	%PAGE

4000	!*******************************************************************
	! 'TR' TRANSACTIONS - LOCAL BILLING RECORDS
	!
	! 01/19/88
	! FJ Customer # (PP_DAILY::CUSNUM) can be:
	!
	!	'~FSALE' = not a FJ customer but a FOREIGN SALE to
	!		   someone elses customer from a FJ site.
	!
	!	Local Sales and Foreign Purchases will be in customer
	!		number order in the sorted file after the
	!		unknown customer and before the '~FSALE' types.
	!
	!	'IFPPSI' = FJ customer # will generally be letters.
	!		   (unless they convert to numbers?)
	!
	!	The '~' for Foreign Sales will put all Foreign Sales
	!	at the end of the GSMMDD.YY? file when sorted (so they
	!	are out of the way for maintaining the GSMMDD.YY? file).
	!
	TRREC% = TRREC% + 1%

	LSET TR_BUF$ = RAW$

	IF (TR::SELFRAN <> HOST$) AND (TR::BUYFRAN <> HOST$)
	THEN
		TEXT$ = "ANOTHER FRANCHISEE'S TRANSACTION:  " + &
			"BUY FRAN=" + TR::BUYFRAN + " SELL FRAN=" + &
			TR::SELFRAN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

		RETURN
	END IF

	TEXT$ = REC_CODE$ + " " + TR::SELFRAN + " " + TR::SITECODE + " " + &
		TR::SITETYPE
	CALL ENTR_3MESSAGE(SCOPE, TEXT$, 1%)

4005	PP_DAILY::CUSNUM	= "~FSALE"
	PP_DAILY::SLTYPE	= "F"		! Foreign Sale
	PP_DAILY::DISCOUNT	= ""

	!
	! Skip if not local
	!
	GOTO 4010 IF TR::BUYFRAN <> HOST$

	!
	! Local sale
	!
	PP_DAILY::SLTYPE = "P"				! Foreign Purchase
	PP_DAILY::SLTYPE = "L" IF TR::SELFRAN = HOST$	! Local Sale

	WHEN ERROR IN
		GET #PP_CARD.CH%, KEY #1% EQ TR::IDENTITY, REGARDLESS
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 4007 IF ERR = 155%
		FILENAME$ = "PP_CARD"
		CONTINUE HelpError
	END WHEN

4006	PP_DAILY::CUSNUM	= PP_CARD::CUSNUM
	PP_DAILY::DISCOUNT	= PP_CARD::DISCOUNT

	GOTO 4010

4007	!
	! Undefined customer number in previous line
	!
	PP_DAILY::CUSNUM	= ""
	PP_DAILY::DISCOUNT	= ""

	CALL ENTR_3MESSAGE(SCOPE, &
		"NOTE:  Missing Cust# for " + TR::IDENTITY + ".", 0%)

	CUST_MISSING% = -1%

	TEXT$ = "NOTE:  Missing Cust# for " + TR::IDENTITY + "."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	TEXT$ = "Add Customer and restart conversion."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	RETURN

4010	!
	! Look up product
	!
	PP_DAILY::PRODUCT	= ""
	PP_DAILY::FTYPE		= "F"

	WHEN ERROR IN
		GET #PD_SUBSTITUTE.CH%, &
			KEY #1% EQ TR::PRDNUM, &
			REGARDLESS
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ PD_SUBSTITUTE::OUR_PRODUCT, &
			REGARDLESS
	USE
		CONTINUE 4012
	END WHEN

	GOTO 4013

4012	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #4% EQ TR::PRDNUM, &
			REGARDLESS	!+TR::PRDSUB
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 4015 IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

4013	PP_DAILY::PRODUCT	= PD_PRODUCT::PRODUCT_NUM
	PP_DAILY::FTYPE		= PD_PRODUCT::CATEGORY

	GOTO 4020

4015	!
	! Undefined customer number in previous line
	!
	PP_DAILY::PRODUCT	= ""
	PP_DAILY::FTYPE		= ""

	CALL ENTR_3MESSAGE(SCOPE, &
		"NOTE:  Missing Product #" + &
		TR::PRDNUM + "-" + TR::PRDSUB + ".", 0%)

	PROD_MISSING% = -1%

	TEXT$ = "NOTE:  Missing Product #" + TR::PRDNUM + TR::PRDSUB + "."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	TEXT$ = "Add Product and restart conversion."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	RETURN

4020	PP_DAILY::HOST		= TR::SELFRAN
	PP_DAILY::SITE		= TR::SITECODE
	PP_DAILY::STYPE		= TR::SITETYPE
	PP_DAILY::TRNNUM	= TR::TRNNUM + TR::TRANSEQ
	PP_DAILY::STAXRATE	= VAL(TR::STAXRATE) / 100000.0
	PP_DAILY::VEHICLE	= TR::VEHICLE
	PP_DAILY::DRIVER	= TR::DRIVER
	PP_DAILY::TRANDATE	= "20" + TR::TRNDATE
	PP_DAILY::TRANTIME	= TR::TRNTIME
	PP_DAILY::PUMP		= TR::PUMP

	!
	ZZ% = LEN(TR::QUANTITY)
	THESIGN$ = RIGHT(TR::QUANTITY, ZZ%)
	THE_AMT = FUNC_ROUND(VAL(THESIGN$ + &
		LEFT(TR::QUANTITY, ZZ% - 1%)) / 100.0, 2%)
	PP_DAILY::QUANTITY	= THE_AMT
	TRAMT = TRAMT + THE_AMT
	!
	PP_DAILY::ODOM		= VAL(TR::ODOM) / 10.0
	PP_DAILY::BUYFRAN	= TR::BUYFRAN
	PP_DAILY::IDENTITY	= TR::IDENTITY
	!
	ZZ% = LEN(TR::SELLPRICE)
	THESIGN$ = RIGHT(TR::SELLPRICE, ZZ%)
	PP_DAILY::SELLPRICE	= VAL(THESIGN$ + &
		LEFT(TR::SELLPRICE, ZZ% - 1%)) / 100000.0
	!
	ZZ% = LEN(TR::TRANCOST)
	THESIGN$ = RIGHT(TR::TRANCOST, ZZ%)
	PP_DAILY::TRANCOST	= VAL(THESIGN$ + &
		LEFT(TR::TRANCOST, ZZ% - 1%)) / 100000.0
	!
	PP_DAILY::MISCKEYB	= TR::MISCKEYB
	IF TR::UNIT = "G"
	THEN
		PP_DAILY::UOM		= "GA"
	ELSE
		PP_DAILY::UOM		= TR::UNIT
	END IF
	PP_DAILY::TRNTYPE	= TR::TRNTYPE
	PP_DAILY::CAPDATE	= "20" + TR::CAPDATE
	PP_DAILY::CAPTIME	= TR::CAPTIME
	PP_DAILY::ICBDATE	= "20" + TR::ICBDATE
	PP_DAILY::POSTBNUM	= TR::POSTBNUM
	PP_DAILY::TRANSOURCE	= TR::TRANSOURCE
	PP_DAILY::EDITACT	= TR::EDITACT
	PP_DAILY::JULIANDAY	= TR::JULIANDAY
	PP_DAILY::RSTATION	= TR::RSTATION

4030	WHEN ERROR IN
		PUT #PP_DAILY.CH%
	USE
		FILENAME$ = "PP_DAILY"
		CONTINUE HelpError
	END WHEN

	!
	! Subtotal by location
	!
	SUBBUYFRAN$ = PP_DAILY::SLTYPE + "  "
	GOSUB AddSubtotal

	!
	! Grand Subtotal
	!
	SUBBUYFRAN$ = "~~~"
	GOSUB AddSubtotal


4050	RETURN

	%PAGE

5000	!*******************************************************************
	! 'T2' TRANSACTIONS - LOCAL BILLING RECORDS
	!
	! 01/19/88
	! FJ Customer # (PP_DAILY::CUSNUM) can be:
	!
	!	'~FSALE' = not a FJ customer but a FOREIGN SALE to
	!		   someone elses customer from a FJ site.
	!
	!	Local Sales and Foreign Purchases will be in customer
	!		number order in the sorted file after the
	!		unknown customer and before the '~FSALE' types.
	!
	!	'IFPPSI' = FJ customer # will generally be letters.
	!		   (unless they convert to numbers?)
	!
	!	The '~' for Foreign Sales will put all Foreign Sales
	!	at the end of the GSMMDD.YY? file when sorted (so they
	!	are out of the way for maintaining the GSMMDD.YY? file).
	!
	TRREC% = TRREC% + 1%

	IF T2::SELFRAN <> HOST$ AND T2::BUYFRAN<>HOST$
	THEN
		TEXT$ = "ANOTHER FRANCHISEE'S TRANSACTION:  " + &
			"BUY FRAN=" + T2::BUYFRAN + " SELL FRAN=" + &
			T2::SELFRAN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

		RETURN
	END IF

5003	TEXT$ = REC_CODE$ + " " + T2::SELFRAN + " " + T2::SITECODE + " " + &
		T2::SITETYPE
	CALL ENTR_3MESSAGE(SCOPE, TEXT$, 1%)


5005	PP_DAILY::CUSNUM	= "~FSALE"
	PP_DAILY::SLTYPE	= "F"			! Foreign Sale
	PP_DAILY::DISCOUNT	= ""
	!
	GOTO 5010 IF T2::BUYFRAN <> HOST$ ! FORDE JOHNSON'S CUSTOMER

	PP_DAILY::SLTYPE = "P"				! Foreign Purchase
	PP_DAILY::SLTYPE = "L" IF T2::SELFRAN = HOST$	! Local Sale

	WHEN ERROR IN
		GET #PP_CARD.CH%, KEY #1% EQ T2::IDENTITY, REGARDLESS
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 5007 IF ERR = 155%
		FILENAME$ = "PP_CARD"
		CONTINUE HelpError
	END WHEN

	PP_DAILY::CUSNUM	= PP_CARD::CUSNUM
	PP_DAILY::DISCOUNT	= PP_CARD::DISCOUNT

	GOTO 5010

5007	!
	! Undefined customer number in previous line
	!
	PP_DAILY::CUSNUM	= ""
	PP_DAILY::DISCOUNT	= ""

	CALL ENTR_3MESSAGE(SCOPE, &
		"NOTE:  Missing Cust# for " + T2::IDENTITY + ".", 0%)

	CUST_MISSING% = -1%

	TEXT$ = "NOTE:  Missing Cust# for " + T2::IDENTITY + "."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	TEXT$ = "Add Customer and restart conversion."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	RETURN

5010	WHEN ERROR IN
		GET #PD_SUBSTITUTE.CH%, &
			KEY #1% EQ T2::PRDNUM, &
			REGARDLESS
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ PD_SUBSTITUTE::OUR_PRODUCT, &
			REGARDLESS
	USE
		CONTINUE 5012
	END WHEN

	GOTO 5014

5012	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #4% EQ T2::PRDNUM, &
			REGARDLESS	!+T2::PRDSUB
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 5015 IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

5014	PP_DAILY::PRODUCT	= PD_PRODUCT::PRODUCT_NUM
	PP_DAILY::FTYPE		= PD_PRODUCT::CATEGORY
	GOTO 5020

5015	!
	! Undefined customer number in previous line
	!
	PP_DAILY::PRODUCT	= ""
	PP_DAILY::FTYPE		= ""

	CALL ENTR_3MESSAGE(SCOPE, &
		"NOTE:  Missing Product #" + T2::PRDNUM + T2::PRDSUB + ".", 0%)

	PROD_MISSING% = -1%

	TEXT$ = "NOTE:  Missing Product #" + T2::PRDNUM + T2::PRDSUB + "."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	TEXT$ = "Add Product and restart conversion."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	RETURN

5020	PP_DAILY::HOST		= T2::SELFRAN
	PP_DAILY::SITE		= T2::SITECODE
	PP_DAILY::STYPE		= T2::SITETYPE
	PP_DAILY::TRNNUM	= T2::TRNNUM + T2::TRANSEQ
	PP_DAILY::STAXRATE	= VAL(T2::STAXRATE) / 100000.0
	PP_DAILY::VEHICLE	= T2::VEHICLE
	PP_DAILY::DRIVER	= T2::DRIVER
	PP_DAILY::TRANDATE	= "20" + T2::TRNDATE
	PP_DAILY::TRANTIME	= T2::TRNTIME
	PP_DAILY::PUMP		= T2::PUMP
	!
	ZZ% = LEN(T2::QUANTITY)
	THESIGN$ = RIGHT(T2::QUANTITY, ZZ%)
	THE_AMT = FUNC_ROUND(VAL(THESIGN$ + &
		LEFT(T2::QUANTITY, ZZ% - 1%)) / 100.0, 2%)
	PP_DAILY::QUANTITY	= THE_AMT
	TRAMT = TRAMT + THE_AMT
	!
	PP_DAILY::ODOM		= VAL(T2::ODOM) / 10.0
	PP_DAILY::BUYFRAN	= T2::BUYFRAN
	PP_DAILY::IDENTITY	= T2::IDENTITY
	!
	ZZ% = LEN(T2::SELLPRICE)
	THESIGN$ = RIGHT(T2::SELLPRICE, ZZ%)
	PP_DAILY::SELLPRICE	= VAL(THESIGN$ + &
		LEFT(T2::SELLPRICE, ZZ% - 1%)) / 100000.0
	!
	ZZ% = LEN(T2::TRANCOST)
	THESIGN$ = RIGHT(T2::TRANCOST, ZZ%)
	PP_DAILY::TRANCOST	= VAL(THESIGN$ + &
		LEFT(T2::TRANCOST, ZZ% - 1%)) / 100000.0
	!
	PP_DAILY::MISCKEYB	= T2::MISCKEYB
	PP_DAILY::UOM		= T2::UNIT
	PP_DAILY::TRNTYPE	= T2::TRNTYPE
	PP_DAILY::CAPDATE	= "20" + T2::CAPDATE
	PP_DAILY::CAPTIME	= T2::CAPTIME
	PP_DAILY::ICBDATE	= "20" + T2::ICBDATE
	PP_DAILY::POSTBNUM	= T2::POSTBNUM
	PP_DAILY::TRANSOURCE	= T2::TRANSOURCE
	PP_DAILY::EDITACT	= T2::EDITACT
	PP_DAILY::JULIANDAY	= T2::JULIANDAY
	PP_DAILY::RSTATION	= T2::RSTATION

5030	WHEN ERROR IN
		PUT #PP_DAILY.CH%
	USE
		FILENAME$ = "PP_DAILY"
		CONTINUE HelpError
	END WHEN

	!
	! Subtotal by location
	!
	SUBBUYFRAN$ = PP_DAILY::SLTYPE + "  "
	GOSUB AddSubtotal

	!
	! Grand Subtotal
	!
	SUBBUYFRAN$ = "~~~"
	GOSUB AddSubtotal

5050	RETURN

	%PAGE

5100	!*******************************************************************
	! 'T3' TRANSACTIONS - LOCAL BILLING RECORDS
	! Based upon the code for T2 records
	!
	! 09/27/96
	! FJ Customer # (PP_DAILY::CUSNUM) can be:
	!
	!	'~FSALE' = not a FJ customer but a FOREIGN SALE to
	!		   someone elses customer from a FJ site.
	!
	!	Local Sales and Foreign Purchases will be in customer
	!		number order in the sorted file after the
	!		unknown customer and before the '~FSALE' types.
	!
	!	'IFPPSI' = FJ customer # will generally be letters.
	!		   (unless they convert to numbers?)
	!
	!	The '~' for Foreign Sales will put all Foreign Sales
	!	at the end of the GSMMDD.YY? file when sorted (so they
	!	are out of the way for maintaining the GSMMDD.YY? file).
	!
	TRREC% = TRREC% + 1%

	IF T3::SELFRAN <> HOST$ AND T3::BUYFRAN <> HOST$
	THEN
		TEXT$ = "ANOTHER FRANCHISEE'S TRANSACTION:  " + &
			"BUY FRAN=" + T3::BUYFRAN + " SELL FRAN=" + &
			T3::SELFRAN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

		RETURN
	END IF

5103	TEXT$ = REC_CODE$ + " " + T3::SELFRAN + " " + T3::SITECODE + " " + &
		T3::SITETYPE
	CALL ENTR_3MESSAGE(SCOPE, TEXT$, 1%)


5105	PP_DAILY::CUSNUM	= "~FSALE"
	PP_DAILY::SLTYPE	= "F"			! Foreign Sale
	PP_DAILY::DISCOUNT	= ""
	!
	GOTO 5110 IF T3::BUYFRAN <> HOST$ ! FORDE JOHNSON'S CUSTOMER

	PP_DAILY::SLTYPE = "P"				! Foreign Purchase
	PP_DAILY::SLTYPE = "L" IF T3::SELFRAN = HOST$	! Local Sale

	WHEN ERROR IN
		GET #PP_CARD.CH%, KEY #1% EQ T3::IDENTITY, REGARDLESS
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 5107 IF ERR = 155%
		CONTINUE 5110
	END WHEN

	PP_DAILY::CUSNUM	= PP_CARD::CUSNUM
	PP_DAILY::DISCOUNT	= PP_CARD::DISCOUNT

	GOTO 5110

5107	!
	! Undefined customer number in previous line
	!
	PP_DAILY::CUSNUM	= ""
	PP_DAILY::DISCOUNT	= ""

	CALL ENTR_3MESSAGE(SCOPE, &
		"NOTE:  Missing Cust# for " + T3::IDENTITY + ".", 0%)

	CUST_MISSING% = -1%

	TEXT$ = "NOTE:  Missing Cust# for " + T3::IDENTITY + "."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	TEXT$ = "Add Customer and restart conversion."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	RETURN

5110	WHEN ERROR IN
		GET #PD_SUBSTITUTE.CH%, &
			KEY #1% EQ T3::PRDNUM, &
			REGARDLESS
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ PD_SUBSTITUTE::OUR_PRODUCT, &
			REGARDLESS
	USE
		CONTINUE 5112
	END WHEN

	GOTO 5114

5112	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #4% EQ T3::PRDNUM, &
			REGARDLESS	!+T3::PRDSUB
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 5115 IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

5114	PP_DAILY::PRODUCT	= PD_PRODUCT::PRODUCT_NUM
	PP_DAILY::FTYPE		= PD_PRODUCT::CATEGORY
	GOTO 5120

5115	!
	! Undefined customer number in previous line
	!
	PP_DAILY::PRODUCT	= ""
	PP_DAILY::FTYPE		= ""

	CALL ENTR_3MESSAGE(SCOPE, &
		"NOTE:  Missing Product #" + T3::PRDNUM + T3::PRDSUB + ".", 0%)

	PROD_MISSING% = -1%

	TEXT$ = "NOTE:  Missing Product #" + T3::PRDNUM + T3::PRDSUB + "."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	TEXT$ = "Add Product and restart conversion."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	RETURN

5120	PP_DAILY::HOST		= T3::SELFRAN
	PP_DAILY::SITE		= T3::SITECODE
	PP_DAILY::STYPE		= T3::SITETYPE
	PP_DAILY::TRNNUM	= T3::TRNNUM + T3::TRANSEQ
	PP_DAILY::STAXRATE	= VAL(T3::STAXRATE) / 100000.0
	PP_DAILY::VEHICLE	= T3::VEHICLE
	PP_DAILY::DRIVER	= T3::DRIVER
	PP_DAILY::TRANDATE	= "20" + T3::TRNDATE
	PP_DAILY::TRANTIME	= T3::TRNTIME
	PP_DAILY::PUMP		= T3::PUMP
	!
	ZZ% = LEN(T3::QUANTITY)
	THESIGN$ = RIGHT(T3::QUANTITY, ZZ%)
	THE_AMT = FUNC_ROUND(VAL(THESIGN$ + &
		LEFT(T3::QUANTITY, ZZ% - 1%)) / 100.0, 2%)
	PP_DAILY::QUANTITY	= THE_AMT
	TRAMT = TRAMT + THE_AMT
	!
	PP_DAILY::ODOM		= VAL(T3::ODOM) / 10.0
	PP_DAILY::BUYFRAN	= T3::BUYFRAN
	PP_DAILY::IDENTITY	= T3::IDENTITY
	!
	ZZ% = LEN(T3::SELLPRICE)
	THESIGN$ = RIGHT(T3::SELLPRICE, ZZ%)
	PP_DAILY::SELLPRICE	= VAL(THESIGN$ + &
		LEFT(T3::SELLPRICE, ZZ% - 1%)) / 100000.0
	!
	ZZ% = LEN(T3::TRANCOST)
	THESIGN$ = RIGHT(T3::TRANCOST, ZZ%)
	PP_DAILY::TRANCOST	= VAL(THESIGN$ + &
		LEFT(T3::TRANCOST, ZZ% - 1%)) / 100000.0
	!
	PP_DAILY::MISCKEYB	= T3::MISCKEYB
	PP_DAILY::UOM		= T3::UNIT
	PP_DAILY::TRNTYPE	= T3::TRNTYPE
	PP_DAILY::CAPDATE	= "20" + T3::CAPDATE
	PP_DAILY::CAPTIME	= T3::CAPTIME
	PP_DAILY::ICBDATE	= "20" + T3::ICBDATE
	PP_DAILY::POSTBNUM	= T3::POSTBNUM
	PP_DAILY::TRANSOURCE	= T3::TRANSOURCE
	PP_DAILY::EDITACT	= T3::EDITACT
	PP_DAILY::JULIANDAY	= T3::JULIANDAY
	PP_DAILY::RSTATION	= T3::RSTATION

5130	PUT #PP_DAILY.CH%

	!
	! Subtotal by location
	!
	SUBBUYFRAN$ = PP_DAILY::SLTYPE + "  "
	GOSUB AddSubtotal

	!
	! Grand Subtotal
	!
	SUBBUYFRAN$ = "~~~"
	GOSUB AddSubtotal

5150	RETURN

	%PAGE

5200	!*******************************************************************
	! 'T4' TRANSACTIONS - LOCAL BILLING RECORDS
	! Based upon the code for T2 records
	!
	! 09/27/96
	! FJ Customer # (PP_DAILY::CUSNUM) can be:
	!
	!	'~FSALE' = not a FJ customer but a FOREIGN SALE to
	!		   someone elses customer from a FJ site.
	!
	!	Local Sales and Foreign Purchases will be in customer
	!		number order in the sorted file after the
	!		unknown customer and before the '~FSALE' types.
	!
	!	'IFPPSI' = FJ customer # will generally be letters.
	!		   (unless they convert to numbers?)
	!
	!	The '~' for Foreign Sales will put all Foreign Sales
	!	at the end of the GSMMDD.YY? file when sorted (so they
	!	are out of the way for maintaining the GSMMDD.YY? file).
	!
	TRREC% = TRREC% + 1%

	IF T4::SELFRAN <> HOST$ AND T4::BUYFRAN <> HOST$
	THEN
		TEXT$ = "ANOTHER FRANCHISEE'S TRANSACTION:  " + &
			"BUY FRAN=" + T4::BUYFRAN + " SELL FRAN=" + &
			T4::SELFRAN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

		RETURN
	END IF

5203	TEXT$ = REC_CODE$ + " " + T4::SELFRAN + " " + T4::SITECODE + " " + &
		T4::SITETYPE
	CALL ENTR_3MESSAGE(SCOPE, TEXT$, 1%)

5205	PP_DAILY::CUSNUM	= "~FSALE"
	PP_DAILY::SLTYPE	= "F"			! Foreign Sale
	PP_DAILY::DISCOUNT	= ""
	!
	GOTO 5210 IF T4::BUYFRAN <> HOST$ ! FORDE JOHNSON'S CUSTOMER

	PP_DAILY::SLTYPE = "P"				! Foreign Purchase
	PP_DAILY::SLTYPE = "L" IF T4::SELFRAN = HOST$	! Local Sale
	PP_DAILY::CUSNUM	= ""
	PP_DAILY::DISCOUNT	= ""

	WHEN ERROR IN
		GET #PP_CARD.CH%, KEY #1% EQ T4::IDENTITY, REGARDLESS
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 5210
	END WHEN

	PP_DAILY::CUSNUM	= PP_CARD::CUSNUM
	PP_DAILY::DISCOUNT	= PP_CARD::DISCOUNT

	GOTO 5210

5210	WHEN ERROR IN
		GET #PD_SUBSTITUTE.CH%, &
			KEY #1% EQ T4::PRDNUM, &
			REGARDLESS
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ PD_SUBSTITUTE::OUR_PRODUCT, &
			REGARDLESS
	USE
		CONTINUE 5212
	END WHEN

	GOTO 5214

5212	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #4% EQ T4::PRDNUM, &
			REGARDLESS
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 5215 IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

5214	PP_DAILY::PRODUCT	= PD_PRODUCT::PRODUCT_NUM
	PP_DAILY::FTYPE		= PD_PRODUCT::CATEGORY
	GOTO 5220

5215	!
	! Undefined customer number in previous line
	!
	PP_DAILY::PRODUCT	= ""
	PP_DAILY::FTYPE		= ""

	PROD_MISSING% = -1%
	TEXT$ = "NOTE:  Missing Product #" + T4::PRDNUM + T4::PRDSUB + ".  " + &
		"Fran: " + T4::SELFRAN + T4::SITECODE + T4::SITETYPE + "  " + &
		"Date: " + T4::TRNDATE + "  " + &
		"Card: " + T4::DRIVER + "/" + T4::VEHICLE

	CALL ENTR_3MESSAGE(SCOPE, TEXT$, 0%)

 !	TEXT$ = "NOTE:  Missing Product #" + T4::PRDNUM + T4::PRDSUB + "."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	TEXT$ = "Add Product and restart conversion."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	RETURN

5220	PP_DAILY::HOST		= T4::SELFRAN
	PP_DAILY::SITE		= T4::SITECODE
	PP_DAILY::STYPE		= T4::SITETYPE

	!
	! Check location of site
	!
	WHEN ERROR IN
		GET #PP_SITE.CH%, &
			KEY #0% EQ PP_DAILY::HOST + &
				PP_DAILY::SITE + PP_DAILY::STYPE, &
			REGARDLESS
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF

		!
		! Unknown site.
		!
		IF ERR = 155%
		THEN
			PP_SITE::STATE = ""
			CONTINUE 5221
		END IF

		!
		! Die if we don't find the site
		!
		FILENAME$ = "PP_SITE"
		CONTINUE HelpError
	END WHEN

5221	PP_DAILY::TRNNUM	= T4::TRNNUM + T4::TRANSEQ
	PP_DAILY::STAXRATE	= VAL(T4::STAXRATE) / 100000.0
	PP_DAILY::VEHICLE	= T4::VEHICLE
	PP_DAILY::DRIVER	= T4::DRIVER
	PP_DAILY::TRANDATE	= T4::TRNDATE
	PP_DAILY::TRANTIME	= T4::TRNTIME
	PP_DAILY::PUMP		= T4::PUMP

	!
	! Is the customer marked exempt for this product
	!
5222	CUSEXEMPT% = 0%
	WHEN ERROR IN
		GET #PP_CARDEXEMPT.CH%, &
			KEY #0% EQ PP_DAILY::CUSNUM + PP_DAILY::DRIVER + &
				PP_DAILY::PRODUCT + PP_SITE::STATE, &
			REGARDLESS
		CUSEXEMPT% = -1%
	USE
		CONTINUE 5223
	END WHEN

5223	WHEN ERROR IN
		GET #PP_CARDEXEMPT.CH%, &
			KEY #0% EQ PP_DAILY::CUSNUM + PP_DAILY::VEHICLE + &
				PP_DAILY::PRODUCT + PP_SITE::STATE, &
			REGARDLESS
		CUSEXEMPT% = -1%
	USE
		CONTINUE 5224
	END WHEN

5224	!
	ZZ% = LEN(T4::QUANTITY)
	THESIGN$ = RIGHT(T4::QUANTITY, ZZ%)
	THE_AMT = FUNC_ROUND(VAL(THESIGN$ + &
		LEFT(T4::QUANTITY, ZZ% - 1%)) / 100.0, 2%)
	PP_DAILY::QUANTITY	= THE_AMT
	TRAMT = TRAMT + THE_AMT

	!
	PP_DAILY::ODOM		= VAL(T4::ODOM) / 10.0
	PP_DAILY::BUYFRAN	= T4::BUYFRAN
	PP_DAILY::IDENTITY	= T4::IDENTITY

	!
	ZZ% = LEN(T4::SELLPRICE)
	THESIGN$ = RIGHT(T4::SELLPRICE, ZZ%)
	PP_DAILY::SELLPRICE	= VAL(THESIGN$ + &
		LEFT(T4::SELLPRICE, ZZ% - 1%)) / 100000.0

	!
	ZZ% = LEN(T4::TRANCOST)
	THESIGN$ = RIGHT(T4::TRANCOST, ZZ%)
	PP_DAILY::TRANCOST	= VAL(THESIGN$ + &
		LEFT(T4::TRANCOST, ZZ% - 1%)) / 100000.0

	IF ((PP_DAILY::PRODUCT = "04" OR &
		PP_DAILY::PRODUCT = "45" OR &
		PP_DAILY::PRODUCT = "64" OR &
		PP_DAILY::PRODUCT = "65") AND PP_SITE::STATE = "OR")
	THEN
		IF CUSEXEMPT% = 0%
		THEN
 !			PP_DAILY::TRANCOST = PP_DAILY::TRANCOST + OREGON_TAX
			PP_DAILY::STAXRATE = PP_DAILY::STAXRATE + OREGON_TAX
		ELSE
			PP_DAILY::SELLPRICE = PP_DAILY::SELLPRICE - OREGON_TAX
		END IF
	END IF

	!
	PP_DAILY::MISCKEYB	= T4::MISCKEYB
	PP_DAILY::UOM		= T4::UNIT
	PP_DAILY::TRNTYPE	= "TI"
	PP_DAILY::CAPDATE	= T4::CAPDATE
	PP_DAILY::CAPTIME	= T4::CAPTIME
	PP_DAILY::ICBDATE	= T4::ICBDATE
	PP_DAILY::POSTBNUM	= T4::POSTBNUM
	PP_DAILY::TRANSOURCE	= T4::TRANSOURCE
	PP_DAILY::EDITACT	= ""
	PP_DAILY::JULIANDAY	= ""
	PP_DAILY::RSTATION	= ""

5230	PUT #PP_DAILY.CH%

	!
	! Subtotal by location
	!
	SUBBUYFRAN$ = PP_DAILY::SLTYPE + "  "
	GOSUB AddSubtotal

	!
	! Grand Subtotal
	!
	SUBBUYFRAN$ = "~~~"
	GOSUB AddSubtotal

5250	RETURN

	%PAGE

6000	!*******************************************************************
	! 'SM' SITE MAINTENANCE - LOCAL BILLING RECORDS
	!*******************************************************************

	SMREC% = SMREC% + 1%
	OLD.REC$ = SM::HOST+SM::CODE + SM::XTYPE
	!
	TEXT$ = REC_CODE$ + " " + SM::HOST + " " + SM::CODE + " " + &
		SM::XTYPE + SM::STATE
	CALL ENTR_3MESSAGE(SCOPE, TEXT$, 1%)
 !	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

6010	WHEN ERROR IN
		GET #PP_SITE.CH%, KEY #0% EQ OLD.REC$
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 6020 IF ERR = 155%
		FILENAME$ = "PP_SITE"
		CONTINUE HelpError
	END WHEN

	!
	! Record exists in site file
	!
	GOSUB 6100 IF SM::XOPTION = "C" OR SM::XOPTION = "A" ! Change
	GOSUB 6200 IF SM::XOPTION = "D"	! Delete

	RETURN

6020	!
	! Record doesn't exist in site file
	!
	GOSUB 6300 IF SM::XOPTION = "C" OR SM::XOPTION = "A" ! Add

	RETURN

6100	!
	! Change
	!
	TEXT$ = "This record was changed in the gassit file - OLD:"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	TEXT$ = "Site: " + PP_SITE::HOST + &
		" Add:" + TRM$(PP_SITE::ADDRESS) + &
		", " + TRM$(PP_SITE::CITY) + "  " + PP_SITE::STATE
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	!
	PP_SITE::ADDRESS	= SM::ADDRESS
	PP_SITE::CITY		= SM::CITY
	PP_SITE::STATE		= SM::STATE

	WHEN ERROR IN
		UPDATE #PP_SITE.CH%
	USE
		FILENAME$ = "PP_SITE"
		CONTINUE HelpError
	END WHEN

	TEXT$ = "This record was changed in the gassit file - NEW:"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	TEXT$ = "Site: " + PP_SITE::HOST + &
		" Add:" + TRM$(PP_SITE::ADDRESS) + &
		", " + TRM$(PP_SITE::CITY) + "  " + PP_SITE::STATE
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

6190	RETURN

6200	!
	! Delete the record
	!
	TEXT$ = "This record was deleted from the SITE file:"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	TEXT$ = "Site: " + PP_SITE::HOST + &
		" Add:" + TRM$(PP_SITE::ADDRESS) + &
		", " + TRM$(PP_SITE::CITY) + "  " + PP_SITE::STATE
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	WHEN ERROR IN
		DELETE #PP_SITE.CH%
	USE
		FILENAME$ = "PP_SITE"
		CONTINUE HelpError
	END WHEN

6210	WHEN ERROR IN
		GET #PP_SITE_PRODUCT.CH%, &
			KEY #0% EQ PP_SITE::HOST
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 6280 IF ERR = 155%
		FILENAME$ = "PP_SITE_PRODUCT"
		CONTINUE HelpError
	END WHEN

6220	WHILE PP_SITE_PRODUCT::HOST = PP_SITE::HOST

		TEXT$ = "This record was deleted from the SITE PRODUCT file:"
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

		WHEN ERROR IN
			DELETE #PP_SITE_PRODUCT.CH%
		USE
			FILENAME$ = "PP_SITE_PRODUCT"
			CONTINUE HelpError
		END WHEN

		TEXT$ = "Site: " + PP_SITE::SITE + " Product: " + &
			PP_SITE_PRODUCT::PRODUCT
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

		WHEN ERROR IN
			GET #PP_SITE_PRODUCT.CH%
		USE
			IF ERR = 154%
			THEN
				SLEEP 1%
				RETRY
			END IF
			CONTINUE 6280 IF ERR = 11%
			FILENAME$ = "PP_SITE_PRODUCT"
			CONTINUE HelpError
		END WHEN

	NEXT

6280	RETURN

6300	!
	! Add a site record
	!
	PP_SITE::HOST		= SM::HOST
	PP_SITE::SITE		= SM::CODE
	PP_SITE::STYPE		= SM::XTYPE
	PP_SITE::SNAME		= ""
	PP_SITE::ADDRESS	= SM::ADDRESS
	PP_SITE::CITY		= SM::CITY
	PP_SITE::STATE		= SM::STATE
	PP_SITE::LOCSALE	= ""
	PP_SITE::FORSALE	= ""
	PP_SITE::FORPUR		= ""

	IF SM::HOST <> HOST$
	THEN
		PP_SITE::FORPUR		= "99"	! FOREIGN PURCHASE
	ELSE
		IF SM::CODE = "IA"
		THEN
			PP_SITE::LOCSALE	= "06"	! POC LOC SALE
			PP_SITE::FORSALE	= "56"	! POC FOR SALE
		ELSE
			PP_SITE::LOCSALE	= "01"	! I.F. LOCAL SALE
			PP_SITE::FORSALE	= "51"	! I.F. FOREIGN SALE
		END IF
	END IF

	WHEN ERROR IN
		PUT #PP_SITE.CH%
	USE
		FILENAME$ = "PP_SITE"
		CONTINUE HelpError
	END WHEN

	TEXT$ = "This record was added to THE SITE file:"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	TEXT$ = "Site: " + PP_SITE::HOST + &
		" Add:" + TRM$(PP_SITE::ADDRESS) + &
		", " + TRM$(PP_SITE::CITY) + "  " + PP_SITE::STATE
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

6390	RETURN

	%PAGE

7000	!*******************************************************************
	! 'SD' SITE DETAIL - LOCAL BILLING RECORDS
	!*******************************************************************

	SDREC% = SDREC% + 1%

	!
	! Look for our product number
	!
	! NOTE: For some reason this only uses two digits.
	! I don't know why this is different from all others.
	!
	WHEN ERROR IN
		GET #PD_SUBSTITUTE.CH%, &
			KEY #1% EQ SD::PRODUCT, &
			REGARDLESS
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ PD_SUBSTITUTE::OUR_PRODUCT, &
			REGARDLESS
	USE
		CONTINUE 5212
	END WHEN

	GOTO 7002

7001	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #4% EQ SD::PRODUCT, &
			REGARDLESS
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 7003 IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

7002	PP_SITE_PRODUCT::PRODUCT	= PD_PRODUCT::PRODUCT_NUM

	GOTO 7010

7003	!
	! Missing product number
	!
	PP_SITE_PRODUCT::PRODUCT	= ""

	CALL ENTR_3MESSAGE(SCOPE, &
		"NOTE:  Missing Product #" + SD::PRODUCT + ".", 0%)

	PROD_MISSING% = -1%

	TEXT$ = "NOTE:  Missing Product #" + SD::PRODUCT + "."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	TEXT$ = "Add Product and restart conversion."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	RETURN

7010	TEXT$ = REC_CODE$ + " " + SD::HOST + " " + SD::CODE + " " + SD::XTYPE
	CALL ENTR_3MESSAGE(SCOPE, TEXT$, 1%)

	WHEN ERROR IN
		GET #PP_SITE_PRODUCT.CH%, &
			KEY #0% EQ SD::HOST + SD::CODE + SD::XTYPE + &
			PP_SITE_PRODUCT::PRODUCT
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 7020 IF ERR = 155%
		FILENAME$ = "PP_SITE_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Record exists
	!
	GOSUB 7100 IF SD::XOPTION = "C" OR SD::XOPTION = "A" ! Change
	GOSUB 7200 IF SD::XOPTION = "D"	! Delete

	GOTO 7030

7020	!
	! Record doesn't exist
	!
	GOSUB 7300 IF SD::XOPTION = "C" OR SD::XOPTION = "A" ! Add

7030	RETURN

7100	!
	! Change
	!
	TEXT$ = "This record was changed in the SITE PRODUCT  file - old"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Site: " + PP_SITE::HOST + " " + &
		PP_SITE_PRODUCT::SITE + " " + &
		PP_SITE_PRODUCT::STYPE + &
		" Prod: " + PP_SITE_PRODUCT::PRODUCT
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	PP_SITE_PRODUCT::EFFDATE = "20" + SD::EFFDATE
	PP_SITE_PRODUCT::FED_RATE = VAL(SD::FETAX) / 100000.0
	PP_SITE_PRODUCT::STA_RATE = VAL(SD::STTAX) / 100000.0
	PP_SITE_PRODUCT::COU_RATE = VAL(SD::COTAX) / 100000.0
	PP_SITE_PRODUCT::CTY_RATE = VAL(SD::CITAX) / 100000.0
	PP_SITE_PRODUCT::STX_INTP = "Y"
	PP_SITE_PRODUCT::STX_INTP = "N" IF SD::SATYPE <> "I"
	PP_SITE_PRODUCT::STX_RATE = VAL(SD::SATAX) / 100000.0

	WHEN ERROR IN
		UPDATE #PP_SITE_PRODUCT.CH%
	USE
		FILENAME$ = "PP_SITE_PRODUCT"
		CONTINUE HelpError
	END WHEN

7190	RETURN

7200	!
	! Delete the record
	!
	TEXT$ = "This record was deleted in the SITE PRODUCT  file"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Site: " + PP_SITE::HOST + " " + &
		PP_SITE_PRODUCT::SITE + " " + &
		PP_SITE_PRODUCT::STYPE + &
		"Prod: " + PP_SITE_PRODUCT::PRODUCT
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	WHEN ERROR IN
		DELETE #PP_SITE_PRODUCT.CH%
	USE
		FILENAME$ = "PP_SITE_PRODUCT"
		CONTINUE HelpError
	END WHEN

	RETURN

7300	!
	! Add a record
	!
	PP_SITE_PRODUCT::HOST		= SD::HOST
	PP_SITE_PRODUCT::SITE		= SD::CODE
	PP_SITE_PRODUCT::STYPE		= SD::XTYPE
	PP_SITE_PRODUCT::PRODUCT	= PD_PRODUCT::PRODUCT_NUM
	PP_SITE_PRODUCT::EFFDATE	= "20" + SD::EFFDATE
	PP_SITE_PRODUCT::FED_RATE	= VAL(SD::FETAX) / 100000.0
	PP_SITE_PRODUCT::FED_INTP	= "Y"
	PP_SITE_PRODUCT::FED_ACCOUNT	= ""
	PP_SITE_PRODUCT::STA_RATE	= VAL(SD::STTAX) / 100000.0
	PP_SITE_PRODUCT::STA_INTP	= "Y"
	PP_SITE_PRODUCT::STA_ACCOUNT	= ""
	PP_SITE_PRODUCT::COU_RATE	= VAL(SD::COTAX) / 100000.0
	PP_SITE_PRODUCT::COU_INTP	= "Y"
	PP_SITE_PRODUCT::COU_ACCOUNT	= ""
	PP_SITE_PRODUCT::CTY_RATE	= VAL(SD::CITAX) / 100000.0
	PP_SITE_PRODUCT::CTY_INTP	= "Y"
	PP_SITE_PRODUCT::CTY_ACCOUNT	= ""
	PP_SITE_PRODUCT::STX_INTP	= "Y"
	PP_SITE_PRODUCT::STX_INTP	= "N" IF SD::SATYPE <> "I"
	PP_SITE_PRODUCT::STX_RATE	= VAL(SD::SATAX) / 100000.0
	PP_SITE_PRODUCT::STX_ACCOUNT	= ""

	WHEN ERROR IN
		PUT #PP_SITE_PRODUCT.CH%
	USE
		FILENAME$ = "PP_SITE_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	TEXT$ = "this record was added to the GAS PRODUCT file:"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Site: " + PP_SITE::HOST + " " + &
		PP_SITE_PRODUCT::SITE + " " + &
		PP_SITE_PRODUCT::STYPE + &
		"Prod: " + PP_SITE_PRODUCT::PRODUCT
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	RETURN

	%PAGE

7500	!*******************************************************************
	! 'D3' SITE DETAIL - LOCAL BILLING RECORDS
	!*******************************************************************

	SDREC% = SDREC% + 1%

	!
	! Look for our product number
	!
	! NOTE: For some reason this only uses two digits.
	! I don't know why this is different from all others.
	!
	WHEN ERROR IN
		GET #PD_SUBSTITUTE.CH%, &
			KEY #1% EQ D3::PRODUCT, &
			REGARDLESS
		GET #PD_PRODUCT.CH%, &
			KEY #0% EQ PD_SUBSTITUTE::OUR_PRODUCT, &
			REGARDLESS
	USE
		CONTINUE 7501
	END WHEN

	GOTO 7502

7501	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, &
			KEY #4% EQ D3::PRODUCT, &
			REGARDLESS
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 7503 IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

7502	PP_SITE_PRODUCT::PRODUCT	= PD_PRODUCT::PRODUCT_NUM

	GOTO 7510

7503	!
	! Missing product number
	!
	PP_SITE_PRODUCT::PRODUCT	= ""

	CALL ENTR_3MESSAGE(SCOPE, &
		"NOTE:  Missing Product #" + D3::PRODUCT + ".", 0%)

	PROD_MISSING% = -1%

	TEXT$ = "NOTE:  Missing Product #" + D3::PRODUCT + "."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 3%)

	TEXT$ = "Add Product and restart conversion."
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	RETURN

7510	TEXT$ = REC_CODE$ + " " + D3::HOST + " " + D3::CODE + " " + D3::XTYPE
	CALL ENTR_3MESSAGE(SCOPE, TEXT$, 1%)

	WHEN ERROR IN
		GET #PP_SITE_PRODUCT.CH%, &
			KEY #0% EQ D3::HOST + D3::CODE + D3::XTYPE + &
			PP_SITE_PRODUCT::PRODUCT
	USE
		IF ERR = 154%
		THEN
			SLEEP 1%
			RETRY
		END IF
		CONTINUE 7520 IF ERR = 155%
		FILENAME$ = "PP_SITE_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Record exists
	!
	GOSUB 7600 IF D3::XOPTION = "C" OR D3::XOPTION = "A" ! Change
	GOSUB 7700 IF D3::XOPTION = "D"	! Delete

	GOTO 7530

7520	!
	! Record doesn't exist
	!
	GOSUB 7800 IF D3::XOPTION = "C" OR D3::XOPTION = "A" ! Add

7530	RETURN

7600	!
	! Change
	!
	TEXT$ = "This record was changed in the SITE PRODUCT  file - old"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Site: " + PP_SITE::HOST + " " + &
		PP_SITE_PRODUCT::SITE + " " + &
		PP_SITE_PRODUCT::STYPE + &
		" Prod: " + PP_SITE_PRODUCT::PRODUCT
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	PP_SITE_PRODUCT::EFFDATE = "20" + D3::EFFDATE
	PP_SITE_PRODUCT::FED_RATE = VAL(D3::FETAX) / 100000.0
	PP_SITE_PRODUCT::STA_RATE = VAL(D3::STTAX) / 100000.0
	PP_SITE_PRODUCT::COU_RATE = VAL(D3::COTAX) / 100000.0
	PP_SITE_PRODUCT::CTY_RATE = VAL(D3::CITAX) / 100000.0
	PP_SITE_PRODUCT::STX_INTP = "Y"
	PP_SITE_PRODUCT::STX_INTP = "N" IF D3::SATYPE <> "I"
	PP_SITE_PRODUCT::STX_RATE = VAL(D3::SATAX) / 100000.0

	WHEN ERROR IN
		UPDATE #PP_SITE_PRODUCT.CH%
	USE
		FILENAME$ = "PP_SITE_PRODUCT"
		CONTINUE HelpError
	END WHEN

7690	RETURN

7700	!
	! Delete the record
	!
	TEXT$ = "This record was deleted in the SITE PRODUCT  file"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Site: " + PP_SITE::HOST + " " + &
		PP_SITE_PRODUCT::SITE + " " + &
		PP_SITE_PRODUCT::STYPE + &
		"Prod: " + PP_SITE_PRODUCT::PRODUCT
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	WHEN ERROR IN
		DELETE #PP_SITE_PRODUCT.CH%
	USE
		FILENAME$ = "PP_SITE_PRODUCT"
		CONTINUE HelpError
	END WHEN

	RETURN

7800	!
	! Add a record
	!
	PP_SITE_PRODUCT::HOST		= D3::HOST
	PP_SITE_PRODUCT::SITE		= D3::CODE
	PP_SITE_PRODUCT::STYPE		= D3::XTYPE
	PP_SITE_PRODUCT::PRODUCT	= PD_PRODUCT::PRODUCT_NUM
	PP_SITE_PRODUCT::EFFDATE	= "20" + D3::EFFDATE
	PP_SITE_PRODUCT::FED_RATE	= VAL(D3::FETAX) / 100000.0
	PP_SITE_PRODUCT::FED_INTP	= "Y"
	PP_SITE_PRODUCT::FED_ACCOUNT	= ""
	PP_SITE_PRODUCT::STA_RATE	= VAL(D3::STTAX) / 100000.0
	PP_SITE_PRODUCT::STA_INTP	= "Y"
	PP_SITE_PRODUCT::STA_ACCOUNT	= ""
	PP_SITE_PRODUCT::COU_RATE	= VAL(D3::COTAX) / 100000.0
	PP_SITE_PRODUCT::COU_INTP	= "Y"
	PP_SITE_PRODUCT::COU_ACCOUNT	= ""
	PP_SITE_PRODUCT::CTY_RATE	= VAL(D3::CITAX) / 100000.0
	PP_SITE_PRODUCT::CTY_INTP	= "Y"
	PP_SITE_PRODUCT::CTY_ACCOUNT	= ""
	PP_SITE_PRODUCT::STX_INTP	= "Y"
	PP_SITE_PRODUCT::STX_INTP	= "N" IF D3::SATYPE <> "I"
	PP_SITE_PRODUCT::STX_RATE	= VAL(D3::SATAX) / 100000.0
	PP_SITE_PRODUCT::STX_ACCOUNT	= ""

	WHEN ERROR IN
		PUT #PP_SITE_PRODUCT.CH%
	USE
		FILENAME$ = "PP_SITE_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	TEXT$ = "this record was added to the GAS PRODUCT file:"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Site: " + PP_SITE::HOST + " " + &
		PP_SITE_PRODUCT::SITE + " " + &
		PP_SITE_PRODUCT::STYPE + &
		"Prod: " + PP_SITE_PRODUCT::PRODUCT
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	RETURN

	%PAGE

8000	!*******************************************************************
	! 'CT' CONTROL - LOCAL BILLING RECORDS
	!*******************************************************************

	!
	ZZ% = LEN(CT::TRQUANT)
	THESIGN$ = RIGHT(CT::TRQUANT, ZZ%)
	TRQUANT = TRQUANT +  FUNC_ROUND(VAL(THESIGN$ + &
		LEFT(CT::TRQUANT, ZZ% - 1%)) / 100.0, 2%)
	!
	TRCOUNT% = TRCOUNT% + VAL(CT::TRCOUNT)
	SMCOUNT% = SMCOUNT% + VAL(CT::SMCOUNT)
	SDCOUNT% = SDCOUNT% + VAL(CT::SDCOUNT)
	!
	RETURN

8100	!*******************************************************************
	! 'CT' CONTROL - LOCAL BILLING RECORDS
	!*******************************************************************

	ZZ% = LEN(C4::TRQUANT)
	THESIGN$ = RIGHT(C4::TRQUANT, ZZ%)
	TRQUANT = TRQUANT +  FUNC_ROUND(VAL(THESIGN$ + &
		LEFT(C4::TRQUANT, ZZ% - 1%)) / 100.0, 2%)

	TRCOUNT% = TRCOUNT% + VAL(C4::TRCOUNT)
	SMCOUNT% = SMCOUNT% + VAL(C4::SMCOUNT)
	SDCOUNT% = SDCOUNT% + VAL(C4::SDCOUNT)

	RETURN

	%PAGE

	!
	! Calculate subtotals
	!
 AddSubtotal:
	FOR I% = 1% TO SUBTOTAL%
		GOTO AddSubtotal1 &
			IF (SUBTOTAL(I%)::BUYFRAN = SUBBUYFRAN$) AND &
			(SUBTOTAL(I%)::PRODUCT = PP_DAILY::PRODUCT)

		GOTO AddSubtotal2 &
			IF (SUBTOTAL(I%)::BUYFRAN + SUBTOTAL(I%)::PRODUCT) > &
			SUBBUYFRAN$ +  PP_DAILY::PRODUCT
	NEXT I%
	I% = SUBTOTAL% + 1%

 AddSubtotal2:
	!
	! Insert new subtotal at I%
	!
	SUBTOTAL(J% + 1%) = SUBTOTAL(J%) FOR J% = SUBTOTAL% TO I% STEP -1%
	SUBTOTAL% = SUBTOTAL% + 1%
	SUBTOTAL(I%)::BUYFRAN = SUBBUYFRAN$
	SUBTOTAL(I%)::PRODUCT = PP_DAILY::PRODUCT
	SUBTOTAL(I%)::COUNTER = 0%
	SUBTOTAL(I%)::QUANTITY = 0.0
	SUBTOTAL(I%)::PRICE = 0.0

 AddSubTotal1:
	!
	! Increment counts
	!
	SUBTOTAL(I%)::COUNTER = SUBTOTAL(I%)::COUNTER + 1%
	SUBTOTAL(I%)::QUANTITY = SUBTOTAL(I%)::QUANTITY + PP_DAILY::QUANTITY
	SUBTOTAL(I%)::PRICE = SUBTOTAL(I%)::PRICE + &
		FUNC_ROUND(PP_DAILY::SELLPRICE * PP_DAILY::QUANTITY, 2%)

	RETURN

	%PAGE

 ExitTotal:
18910	!*******************************************************************
	! END OF FILE
	!*******************************************************************

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	!
	! Dump out subtotals
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 10%)
	TEXT$ = "   " + &
		"Product       " + &
		"       Count " + &
		"      Quantity " + &
		"       Dollars"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	XTOTAL$ = "~~~~~~~~"
	COUNTER% = 0%
	QUANTITY = 0.0
	PRICE = 0.0

	FOR I% = 1% TO SUBTOTAL%

		IF SUBTOTAL(I%)::BUYFRAN <> XTOTAL$
		THEN
			IF COUNTER%
			THEN
				TEXT$ = "      Total       " + &
					FORMAT$(COUNTER%, "###,###,### ") + &
					FORMAT$(QUANTITY, "###,###,###.## ") + &
					FORMAT$(PRICE, "###,###,###.##")

				CALL OUTP_LINE("", UTL_REPORTX, &
					TITLE$(), TEXT$, 0%)
				CALL OUTP_LINE("", UTL_REPORTX, &
					TITLE$(), "", -1%)

				COUNTER% = 0%
				QUANTITY = 0.0
				PRICE = 0.0
			END IF

			SELECT SUBTOTAL(I%)::BUYFRAN
			CASE "~~~"
				TEXT$ = "Total of all Locations"
			CASE "F"
				TEXT$ = "Total of Foreign Sales (F)"
			CASE "P"
				TEXT$ = "Total of Foreign Purchases(P)"
			CASE ELSE
				TEXT$ = "Total of Local Sales (" + &
					SUBTOTAL(I%)::BUYFRAN + ")"
			END SELECT

			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			XTOTAL$ = SUBTOTAL(I%)::BUYFRAN
		END IF

		TEXT$ = "   " + &
			SUBTOTAL(I%)::PRODUCT + &
			FORMAT$(SUBTOTAL(I%)::COUNTER, " ###,###,### ") + &
			FORMAT$(SUBTOTAL(I%)::QUANTITY, "###,###,###.## ") + &
			FORMAT$(SUBTOTAL(I%)::PRICE, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		COUNTER% = COUNTER% + SUBTOTAL(I%)::COUNTER
		QUANTITY = QUANTITY + SUBTOTAL(I%)::QUANTITY
		PRICE = PRICE + SUBTOTAL(I%)::PRICE

	NEXT I%

	IF COUNTER%
	THEN
		TEXT$ = "      Total       " + &
			FORMAT$(COUNTER%, "###,###,### ") + &
			FORMAT$(QUANTITY, "###,###,###.## ") + &
			FORMAT$(PRICE, "###,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "Locbill TR record count  " + &
		NUM1$(TRCOUNT%) + "  =  " + NUM1$(TRREC%) + &
		" Gascon's TR record count"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Locbill SM record count  " + &
		NUM1$(SMCOUNT%) + "  =  " + NUM1$(SMREC%) + &
		" Gascon's SM record count"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TEXT$ = "Locbill SD record count  " + &
		NUM1$(SDCOUNT%) + "  =  " + NUM1$(SDREC%) + &
		" Gascon's SD record count"

	TEXT$ = "Locbill TR quantity  " + NUM1$(TRQUANT) + &
		"  =  " + NUM1$(TRAMT) + " Gascon's TR quantity    "

	IF CUST_MISSING%
	THEN
		TEXT$ = "WARNING:  Missing cust #'s - " + &
			"CHECK THE GSSICN REPORT - " + &
			"ADD IT AND RESTART."
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

18960	IF PROD_MISSING%
	THEN
		TEXT$ = "WARNING:  Missing Prod #'s - " + &
			"CHECK THE GSSICN REPORT - " + &
			"ADD IT AND RESTART."
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

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

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

32767	END
