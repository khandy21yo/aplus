1	%TITLE "Register by Make"
	%SBTTL "MO_RPRT_REGCUS"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1995 BY
	!
	! Software Solutions.
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be
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
	! ID:MO0??
	!
	! Abstract:HELP
	!	.lm +5
	!	.b
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_RPRT_REGCUS/LINE
	!	$ LINK/EXE=MO_EXE: MO_RPRT_REGCUS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE MO_RPRT_REGCUS.OBJ;*
	!
	! AUTHOR:
	!
	!	04/18/95 - Kevin Handy
	!		Based on MO_RPRT_REGMAKE
	!
	! MODIFICATION HISTORY:
	!
	!	04/19/95 - Kevin Handy
	!		Do a find using key #3 if a from-make is given.
	!		Add message about creating sort file.
	!
	!	04/28/95 - Kevin Handy
	!		Change from.item to from_item and to.item to
	!		to_item.
	!		Add from/to customer options.
	!
	!	05/05/95 - Kevin Handy
	!		Modified sort to be by make instead of by date.
	!
	!	07/25/96 - Kevin Handy
	!		Changed references from MO_REGHEADER to
	!		OE_REGHEADER, since that's the file that was
	!		being read anyway.
	!
	!	09/06/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/31/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include CMC codes
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.HB"
	MAP (MO_REGLINE)	MO_REGLINE_CDD		MO_REGLINE

	%INCLUDE "SOURCE:[OE.OPEN]OE_REGHEADER.HB"
	DECLARE			OE_REGHEADER_CDD	OE_REGHEADER_READ

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	RECORD TEMP_STRUCT
		STRING	CUSTOM = 10%
		STRING	ADATE = 8%
		STRING	MAKE = 10%
		STRING	YEAR = 4%
		STRING	MODEL = 4%
		STRING	SERIAL = 10%
	END RECORD

	MAP (TEMP_FILE) TEMP_STRUCT TEMP_FILE

	!
	! Declare external functions
	!
	EXTERNAL LONG	FUNCTION OE_READ_REGHEADER
	EXTERNAL LONG	FUNCTION AR_EXAM_CUSTOM

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Make\*
	!	.b
	!	.lm +5
	!	The ^*From Make\* field begins the report
	!	with a particular make.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	make in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Make\*
	!	.b
	!	.lm +5
	!	The ^*To Make\* field ends printing
	!	with a particular make.
	!	.b
	!	A blank field will cause the report to end with the last
	!	make in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated makes by entering a "wildcard"
	!	using the Wildcarding Technique.
	!	.b
	!	For information on "Wildcarding" techniques refer to Appendix B.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	DISPLAY_QTY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	.x Display Quantity
	!	^*(04) Display Quantity
	!	.b
	!	.lm +5
	!	The ^*Display Quantity\* field selects
	!	designated quantities to be printed.
	!	.b
	!	Valid codes are:
	!	.table 3,25
	!	.te
	!	^*O\* - Ordered Quantity
	!	.te
	!	^*S\* - Shipped Quantity
	!	.te
	!	^*C\* - Cancelled Quantity
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_DATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) From Date\*
	!	.b
	!	.lm +5
	!	This field specifies the date of the first order to
	!	be included.
	!	Leaving this field blank will all orders up to the
	!	from date to be included.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_DATE$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*(07) To Date\*
	!	.b
	!	.lm +5
	!	This field specifies the date of the last order to include.
	!	Leaving this field blank will cause all item older than the
	!	from date to be included.
	!	.lm -5
	!
	! Index:
	!
	!--

	FROM_CUST$ = LEFT(UTL_REPORTX::OPTDEF(7%), 10%)

	!++
	! Abstract:FLD08
	!	^*(08) From Customer\*
	!	.b
	!	.lm +5
	!	This field specifies the first customer to include.
	!	Leaving this field blank will cause the report to start with the
	!	first cistomer in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

	TO_CUST$ = LEFT(UTL_REPORTX::OPTDEF(8%), 10%)

	!++
	! Abstract:FLD09
	!	^*(09) To Customer\*
	!	.b
	!	.lm +5
	!	This field specifies the last customer to include.
	!	Leaving this field blank will cause the report to include all
	!	customers up to the last customer in the file.
	!	.lm -5
	!
	! Index:
	!
	!--

310	!
	! Open Order Line file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_REGLINE.OPN"
	USE
		FILENAME$ = "MO_REGLINE"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "REGISTER REPORT BY CUSTOMER"

	SELECT DISPLAY_QTY$

	CASE "O"
		TITLE$(2%) = "QUANTITY ORDERED"
		QD$ = " QtyOrd"
		QD% = 1%
		QTYPE$ = "01"

	CASE "S"
		TITLE$(2%) = "QUANTITY SHIPPED"
		QD$ = "QtyShip"
		QD% = 2%
		QTYPE$ = "02"

	CASE "C"
		TITLE$(2%) = "QUANTITY CANCELLED"
		QD$ = " QtyCan"
		QD% = 3%
		QTYPE$ = "03"

	END SELECT

	!
	! Heading
	!
	TITLE$(3%) = "Manufacture to Order System"
	TITLE$(4%) = ""

	TITLE$(5%) = "  Make        Year Model   Serial#      Date"

	TITLE$(6%) = "."

	%PAGE

500	!*******************************************************************
	! Create sorted file
	!*******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Creating sort file...", 1%)

	CALL ASSG_CHANNEL(TEMP_FILE.CH%, STAT%)

	WHEN ERROR IN
		OPEN "MO_TEMP_SORT.TMP" FOR OUTPUT AS FILE TEMP_FILE.CH%, &
			ORGANIZATION INDEXED FIXED, &
			TEMPORARY, &
			BUFFER 32%, &
			MAP TEMP_FILE, &
			PRIMARY KEY (TEMP_FILE::CUSTOM, TEMP_FILE::MAKE, &
				TEMP_FILE::YEAR, &
				TEMP_FILE::SERIAL) DUPLICATES, &
			ACCESS MODIFY, &
			ALLOW NONE
	USE
		FILENAME$ = "TEMP_FILE"
		CONTINUE HelpError
	END WHEN

505	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #MO_REGLINE.CH%, KEY #3%
		ELSE
			FIND #MO_REGLINE.CH%, KEY #3% GE FROM_ITEM$
		END IF
	USE
		FILENAME$ = "MO_REGLINE"
		CONTINUE HelpError
	END WHEN

510	WHEN ERROR IN
		GET #MO_REGLINE.CH%
	USE
		CONTINUE 600 IF ERR = 11%
		FILENAME$ = "MO_REGLINE"
		CONTINUE HelpError
	END WHEN

	!
	! Checks to see if this is a desired record
	!
	GOTO 510 IF MO_REGLINE::TRANTYPE <> QTYPE$

	GOTO 600 IF (TO_ITEM$ <> "") AND (MO_REGLINE::MAKE > FROM_ITEM$)

	GOTO 510 IF (FROM_DATE$ <> "") AND (MO_REGLINE::TDATE < FROM_DATE$)
	GOTO 510 IF (TO_DATE$ <> "") AND (MO_REGLINE::TDATE > TO_DATE$)

	!
	! Get additional information
	!
	IF OE_READ_REGHEADER(MO_REGLINE::ORDNUM, OE_REGHEADER_READ) <> &
		CMC$_NORMAL
	THEN
		OE_REGHEADER_READ::CUSNUM = ""
		OE_REGHEADER_READ::CUSTPO = ""
	END IF

560	!
	! Look at from/to customer
	!
	GOTO 510 IF (FROM_CUST$ <> "") AND &
		(OE_REGHEADER_READ::CUSNUM < FROM_CUST$)

	GOTO 510 IF (TO_CUST$ <> "") AND &
		(OE_REGHEADER_READ::CUSNUM > TO_CUST$)

	!
	! Add another record to the temporary file
	!
	TEMP_FILE::CUSTOM	= OE_REGHEADER_READ::CUSNUM
	TEMP_FILE::ADATE	= MO_REGLINE::TDATE
	TEMP_FILE::MAKE		= MO_REGLINE::MAKE
	TEMP_FILE::YEAR		= MO_REGLINE::YEAR
	TEMP_FILE::MODEL	= MO_REGLINE::MODELCODE
	TEMP_FILE::SERIAL	= MO_REGLINE::IDNUM

	WHEN ERROR IN
		PUT #TEMP_FILE.CH%
	USE
		CONTINUE 510 IF ERR = 134%	! Duplicate
		FILENAME$ = "TEMP_FILE"
		CONTINUE HelpError
	END WHEN

	GOTO 510

	%PAGE

600	!

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		RESET #TEMP_FILE.CH%
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "TEMP_FILE"
		CONTINUE HelpError
	END WHEN

	THIS_CUSTOMER$ = "!%^#%!@^#&@#"

 GetNextRec:
17020	WHEN ERROR IN
		GET #TEMP_FILE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "TEMP_FILE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record if should be printed
	!
	GOSUB PrintLine IF THIS_CUSTOMER$ <> TEMP_FILE::CUSTOM

	THIS_CUSTOMER$ = TEMP_FILE::CUSTOM

	!
	! Output this line
	!
	TEXT$ = "   " + &
		TEMP_FILE::MAKE + " " + &
		TEMP_FILE::YEAR + " " + &
		TEMP_FILE::MODEL + "   " + &
		TEMP_FILE::SERIAL + " " + &
		PRNT_DATE(TEMP_FILE::ADATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 5%)
	GOTO ExitProgram IF UTL_REPORTX::STAT


	GOTO GetNextRec

 PrintLine:
	!
	! Print any totals
	!

	!
	! Gather info
	!
	IF AR_EXAM_CUSTOM(TEMP_FILE::CUSTOM, AR_35CUSTOM_EXAM) <> &
		CMC$_NORMAL
	THEN
		AR_35CUSTOM_EXAM::CUSNAM = &
			STRING$(LEN(AR_35CUSTOM_EXAM::CUSNAM), A"?"B)
	END IF

	!
	! Print title line
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = TEMP_FILE::CUSTOM + "  " + AR_35CUSTOM_EXAM::CUSNAM

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 5%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	RETURN

 ExitTotal:

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
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Resume to display untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
