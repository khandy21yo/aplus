1	%TITLE "Ticket Entry Journal"
	%SBTTL "PS_SPEC_RECREATEJOUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1995 BY
	!
	! Software Solutions
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
	! Software Solutions.
	!
	! Software Solutions assumes no responsibility for the use or
	! reliability of its software on equipment which is not supported
	! by Software Solutions.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Re reates a ticket journal from the register information.
	!	.lm -5
	!
	! Index:
	!	.x Ticket Entry Journal
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PS_SOURCE:PS_SPEC_RECREATEJOUR/LINE
	!	$ LINK/EXE=PS_EXE: PS_SPEC_RECREATEJOUR, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PS_SPEC_RECREATEJOUR.OBJ;*
	!
	! Author:
	!
	!	04/23/95 - Kevin Handy
	!
	! Modification history:
	!
	!	05/18/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/08/95 - Kevin Handy
	!		Changes for OE_REGLINE::NOTES, OE_ORDERLINE::NOTES,
	!		OE_REGLINE::SUBACCT, OE_ORDERLINE::SUBACCT
	!
	!	10/28/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/16/98 - Kevin Handy
	!		Handle new deposit number field
	!
	!	10/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PS_WINDOW.INC"

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	MAP (OE_REGHEADER) OE_REGHEADER_CDD OE_REGHEADER

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.HB"
	MAP (OE_REGLINE) OE_REGLINE_CDD OE_REGLINE

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR) OE_ORDERJOUR_CDD OE_ORDERJOUR

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.HB"
	MAP (OE_ORDERLINE) OE_ORDERLINE_CDD OE_ORDERLINE

	%PAGE

	!
	! Initialize all the standard stuff through an external call
	!
	CALL READ_INITIALIZE

200	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.OPN"

210	%INCLUDE "SOURCE:[OE.OPEN]OE_REGLINE.OPN"

	!
	! Look up device
	!
	CALL  READ_DEVICE("OE_ORDERJOUR", OE_ORDERJOUR.DEV$, STAT%)

	!
	! Ask for batch number
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		20%, &
		80%, &
		SMG_SCREEN_DATA% &
	)

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_SCREEN_DATA%, &
		SCOPE::SMG_PBID, &
		1%, &
		1% &
	)

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Register Number:", &
		10%, 30%)

	!
	! Assign default register number
	!
	REG_NO$ = "????"
	BATCH_NO$ = "??????"

310	!
	! Set up the help message
	!
	SCOPE::PRG_ITEM = "FLD01REGNUM"

	!++
	! Abstract:FLD01REGNUM
	!--

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 10%, 48%, REG_NO$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?

		OPTION$ = "EXIT"
		GOTO ExitProgram

	END SELECT

	REG_NO$ = EDIT$(REG_NO$, -1%)

	GOTO 310 IF INSTR(1%, REG_NO$, "?")

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Batch number:", 11%, 30%)

320	!
	! Set up the help message
	!
	SCOPE::PRG_ITEM = "FLD02BATCH"

	!++
	! Abstract:FLD02BATCH
	!--

	!
	! Assign default batch number
	!
	BATCH_NUM$ = "01"

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 11%, 48%, &
		BATCH_NUM$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?

		OPTION$ = "EXIT"
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 310

	END SELECT

	BATCH_NUM$ = EDIT$(BATCH_NUM$, -1%)

	IF LEN(TRM$(BATCH_NUM$)) <> 2%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the batch number in XX format", 0%)
		GOTO 320
	END IF

	BATCH_NO$ = REG_NO$ + "_" + BATCH_NUM$

	SMG_STATUS% = SMG$PUT_CHARS(SMG_SCREEN_DATA%, "Post Batch:", 12%, 30%)

330	!
	! Set up the help message
	!
	SCOPE::PRG_ITEM = "FLD03BATCH"

	!++
	! Abstract:FLD03BATCH
	!--

	!
	! Assign default batch number
	!
	POST_BATCH$ = "??????"

	SELECT ENTR_3ENTER(SCOPE, SMG_SCREEN_DATA%, 12%, 48%, &
		POST_BATCH$, -1%, 16%)

	CASE 3%, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ	! Exit key ?

		OPTION$ = "EXIT"
		GOTO ExitProgram

	CASE SMG$K_TRM_UP
		GOTO 320

	END SELECT

	POST_BATCH$ = EDIT$(POST_BATCH$, -1%)

	IF LEN(TRM$(POST_BATCH$)) <> 6%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Please enter the batch number in XXXXXX format", 0%)
		GOTO 320
	END IF

400	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(SMG_SCREEN_DATA%)

1000	!******************************************************************
	! Open files (create if necessary)
	!******************************************************************

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.CRE"

1010	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.CRE"

	GOTO 2100

2000	!*******************************************************************
	! Process header file
	!*******************************************************************

	WHEN ERROR IN
		FIND #OE_ORDERJOUR.CH%, KEY #0% EQ OE_ORDERLINE::ORDNUM
	USE
		CONTINUE 2005
	END WHEN

	GOTO 2090

2005	WHEN ERROR IN
		GET #OE_REGHEADER.CH%, &
			KEY #0% EQ OE_ORDERLINE::ORDNUM, &
			REGARDLESS
	USE
		CONTINUE 2090
	END WHEN

2020	OE_ORDERJOUR::ORDNUM	= OE_REGHEADER::ORDNUM
	OE_ORDERJOUR::ORDDATE	= OE_REGHEADER::ORDDATE
	OE_ORDERJOUR::ORDTYPE	= OE_REGHEADER::ORDTYPE
	OE_ORDERJOUR::ORDCAT	= OE_REGHEADER::ORDCAT
	OE_ORDERJOUR::CUSNUM	= OE_REGHEADER::CUSNUM
	OE_ORDERJOUR::DISC	= OE_REGHEADER::DISC
	OE_ORDERJOUR::MISC	= 0.0
	OE_ORDERJOUR::SHIPNAM	= OE_REGHEADER::SHIPNAM
	OE_ORDERJOUR::ADD1	= OE_REGHEADER::ADD1
	OE_ORDERJOUR::ADD2	= OE_REGHEADER::ADD2
	OE_ORDERJOUR::ADD3	= OE_REGHEADER::ADD3
	OE_ORDERJOUR::CITY	= OE_REGHEADER::CITY
	OE_ORDERJOUR::STATE	= OE_REGHEADER::STATE
	OE_ORDERJOUR::ZIP	= OE_REGHEADER::ZIP
	OE_ORDERJOUR::COUNTRY	= OE_REGHEADER::COUNTRY
	OE_ORDERJOUR::DEPOSIT	= OE_REGHEADER::DEPOSIT
	OE_ORDERJOUR::OLDCUSTPO	= OE_REGHEADER::OLDCUSTPO
	OE_ORDERJOUR::SHIPDATE	= OE_REGHEADER::SDATE
	OE_ORDERJOUR::SHIPVIA	= OE_REGHEADER::SHIPVIA
	OE_ORDERJOUR::TERMS	= OE_REGHEADER::TERMS
	IF OE_REGHEADER::TAXFLAG = "1"
	THEN
		OE_ORDERJOUR::SALESTAX	= 5.0
	ELSE
		OE_ORDERJOUR::SALESTAX	= 0.0
	END IF

	OE_ORDERJOUR::LOCATION	= OE_REGHEADER::LOCATION
	OE_ORDERJOUR::OPERATOR	= OE_REGHEADER::OPERATOR
	OE_ORDERJOUR::COMMAMT	= OE_REGHEADER::COMMAMT
	OE_ORDERJOUR::COMMPERC	= 0.0
	OE_ORDERJOUR::SALESMAN	= OE_REGHEADER::SALESMAN
	OE_ORDERJOUR::CREASON	= ""
	OE_ORDERJOUR::SALCOMM	= OE_REGHEADER::SALCOMM
	OE_ORDERJOUR::HANDLING	= 0.0
	OE_ORDERJOUR::AMTPAID	= OE_REGHEADER::AMTPAID
	OE_ORDERJOUR::CHECK	= ""
	OE_ORDERJOUR::NOTES(0%)	= OE_REGHEADER::NOTES(0%)
	OE_ORDERJOUR::NOTES(1%)	= OE_REGHEADER::NOTES(1%)
	OE_ORDERJOUR::NOTES(2%)	= OE_REGHEADER::NOTES(2%)
	OE_ORDERJOUR::MISCACCT	= ""
	OE_ORDERJOUR::TRANDATE	= ""
	OE_ORDERJOUR::TRANTIME	= ""
	OE_ORDERJOUR::INVNUM	= ""
	OE_ORDERJOUR::FREIGHT	= 0.0
	OE_ORDERJOUR::TAXCODE	= OE_REGHEADER::TAXCODE
	OE_ORDERJOUR::TAXFLAG	= OE_REGHEADER::TAXFLAG
	OE_ORDERJOUR::SHIPLIN	= OE_REGHEADER::SHIPLIN
	OE_ORDERJOUR::PAYMNT	= 0.0
	OE_ORDERJOUR::REG_FLAG	= ""
	OE_ORDERJOUR::CUSTPO	= OE_REGHEADER::CUSTPO

	PUT #OE_ORDERJOUR.CH%

2090	RETURN

2100	!*******************************************************************
	! Process header file
	!*******************************************************************

	WHEN ERROR IN
		FIND #OE_REGLINE.CH%, KEY #2% GE POST_BATCH$
	USE
		CONTINUE 2190
	END WHEN

	GOTIT% = 0%

2110	WHEN ERROR IN
		GET #OE_REGLINE.CH%, REGARDLESS
	USE
		CONTINUE 2190
	END WHEN

2120	IF (OE_REGLINE::BATCH = POST_BATCH$)
	THEN
		IF (GOTIT% <> 0%) AND &
			((OE_ORDERLINE::ORDNUM <> OE_REGLINE::ORDNUM) OR &
			(OE_ORDERLINE::LIN <> OE_REGLINE::LIN))
		THEN
			OE_ORDERLINE::BCKQTY	= &
				OE_ORDERLINE::ORDQTY - &
				OE_ORDERLINE::SHPQTY

			OE_ORDERLINE::LIN = "NEWL"
			PUT #OE_ORDERLINE.CH%

			GOSUB 2000

			OE_ORDERLINE::ORDQTY	= 0.0
			OE_ORDERLINE::SHPQTY	= 0.0
			OE_ORDERLINE::BCKQTY	= 0.0

			GOTIT% = 0%

		END IF

		IF (GOTIT% = 0%)
		THEN
			OE_ORDERLINE::ORDNUM	= OE_REGLINE::ORDNUM
			OE_ORDERLINE::PRODUCT	= OE_REGLINE::PRODUCT
			OE_ORDERLINE::PRICE	= OE_REGLINE::PRICE
			OE_ORDERLINE::DISCOUNT	= OE_REGLINE::DISCOUNT
			OE_ORDERLINE::COST	= OE_REGLINE::COST
			OE_ORDERLINE::REQDATE	= OE_REGLINE::TDATE
			OE_ORDERLINE::PROMO	= OE_REGLINE::PROMO
			OE_ORDERLINE::MISCH	= OE_REGLINE::MISCH
			OE_ORDERLINE::NOTES1	= OE_REGLINE::NOTES1
			OE_ORDERLINE::NOTES2	= OE_REGLINE::NOTES2
			OE_ORDERLINE::SUBACCT	= OE_REGLINE::SUBACCT
			OE_ORDERLINE::LIN	= OE_REGLINE::LIN
			OE_ORDERLINE::MISCH2	= OE_REGLINE::MISCH2

			GOTIT% = -1%
		END IF

		SELECT OE_REGLINE::TRANTYPE
		CASE "01"
			OE_ORDERLINE::ORDQTY	= OE_REGLINE::QTY
		CASE "02"
			OE_ORDERLINE::SHPQTY	= OE_REGLINE::QTY
		CASE "03"
			OE_ORDERLINE::BCKQTY	= OE_REGLINE::QTY
		END SELECT

		GOTO 2110
	END IF

2190	IF GOTIT%
	THEN
		OE_ORDERLINE::BCKQTY	= &
			OE_ORDERLINE::ORDQTY - &
			OE_ORDERLINE::SHPQTY

		PUT #OE_ORDERLINE.CH%

		GOSUB 2000

		OE_ORDERLINE::ORDQTY	= 0.0
		OE_ORDERLINE::SHPQTY	= 0.0
		OE_ORDERLINE::BCKQTY	= 0.0

		GOTIT% = 0%
	END IF


	!******************************************************************
	! End of the program
	!******************************************************************

 ExitProgram:
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

32767	END
