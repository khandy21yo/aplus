1	%TITLE "Accounts Receivable Customer Label Writer"
	%SBTTL "AR_RPRT_NAMELABEL"
	%IDENT "V3.6a Calico"

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
	! ID:AR051
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Name Address Labels\* routine prints
	!	labels for customers.
	!	.b
	!	The print setting screen provides fields in which entries may be
	!	made to print only a partial listing of the file and also provides
	!	a setting in which the labels can be printed in customer number order,
	!	customer name order, or alphabetical order.
	!	.lm -5
	!
	! Index:
	!	.x Labels>Customer
	!	.x Customer>Labels
	!	.x Print>Customer Labels
	!
	! Option:
	!
	!
	! Author:
	!
	!	03/11/88 - Aaron Redd
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_NAMELABEL/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_RPRT_NAMELABEL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_NAMELABEL.OBJ;*
	!
	! Modification history:
	!
	!	03/16/92 - Kevin Handy
	!		Removed someones very strange and undocumented
	!		changed that made completly different reports
	!		when "utl_reportx::printtype=1".
	!
	!	03/16/92 - Kevin Handy
	!		Moved about odd chenge that tried to blank
	!		the utl_country::descriptor for USA, but
	!		instead caused only american names to print.
	!
	!	03/16/92 - Kevin Handy
	!		Modified to lose blanks between lines on labels.
	!
	!	03/16/92 - Kevin Handy
	!		Added error trapping for file opens.
	!
	!	08/26/92 - Kevin Handy
	!		Added Zipcode sort.
	!
	!	08/27/92 - Kevin Handy
	!		Fixed so that "TO ITEM" works with zip codes.
	!
	!	09/14/92 - Dan Perkins
	!		Added option to choose whether to print customer
	!		number or not.
	!
	!	10/09/92 - Kevin Handy
	!		Added option for wildcard customer number.
	!
	!	11/18/92 - Kevin Handy
	!		Added wildcard type, wildcard category.
	!
	!	11/23/92 - Dan Perkins
	!		Added ZIP CODE documentation to the sort option.
	!
	!	11/09/93 - Kevin Handy
	!		Added Number of labels per customer field.
	!		(as per Forde Johnson Request where they want 200
	!		labels for one customer)
	!
	!	11/01/94 - Kevin Handy
	!		Added ability to sort by salesman number, as
	!		requested by KINGB. Also moved handling of
	!		from/to to the creation of the temp file
	!		instead of in the printing routine.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/11/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/26/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	06/29/2001 - Kevin Handy
	!		Add an "Active Only" option.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.HB"
	MAP (UTL_COUNTRY)	UTL_COUNTRY_CDD		UTL_COUNTRY

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	!
	! Dimension Statements
	!
	DIM XLINE$(66%)

	!
	! External Functions
	!
	EXTERNAL LONG	FUNCTION COMP_STRING

	%PAGE

	ON ERROR GOTO 19000


 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the printing
	!	to begin with the selected Item _#.
	!	The value must be in agreement with field
	!	(03) Sort by.
	!	.b
	!	A blank setting will cause the report to begin with the first
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Name/Address Labels
	!	.x Name/Address Labels>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the printing
	!	to end with the selected Item _#. The
	!	value must be in agreement with field (03) Sort by.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	Item _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Name/Address Labels
	!	.x Name/Address Labels>To Item
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	.x Sort by>Name/Address Labels
	!	^*(03) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field determines the order
	!	in which the report will print.
	!	.b
	!	Valid settings are:
	!	.te
	!	.table 3,25
	!	^*N\* - Number
	!	.te
	!	^*T\* - Type
	!	.te
	!	^*C\* - Category
	!	.te
	!	^*A\* - Alphabetical
	!	.te
	!	^*Z\* - Zip Code
	!	.TE
	!	*S - Salesman
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Name/Address Labels>Sort by
	!
	!--

	SELECT SORTBY$

	CASE "N"
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::CUSNUM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::CUSNUM))

	CASE "Z"
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::ZIP))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::ZIP))

	CASE "T"
		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::TTYPE))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::TTYPE))

	CASE "C"
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::CATEGORY))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::CATEGORY))

	CASE "A"
		K_NUM% = 3%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::ALPSRT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::ALPSRT))

	CASE "S"
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(AR_35CUSTOM::SALESMAN))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(AR_35CUSTOM::SALESMAN))

	END SELECT

	INCLUDE.CUSNUM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	.ts 55
	!	^*(04) Include Customer Number\*
	!	.b
	!	.lm +5
	!	The ^*Include Customer Number\* field
	!	determines whether or not the Customer Number will be printed
	!	on the label.
	!	.b
	!	Valid settings are:
	!	.te
	!	.table 3,25
	!	^*Y\* - Yes
	!	.te
	!	^*N\* - No
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!
	!--

	WILD_CUST$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 128%)

	!++
	! Abstract:FLD05
	!	^*(05) Wildcard Customer\*
	!
	! Index:
	!	.x Name/Address Labels>Wildcard Customer
	!
	!--

	WILD_TYPE$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 128%)

	!++
	! Abstract:FLD06
	!	^*(06) Wildcard Type\*
	!
	! Index:
	!	.x Name/Address Labels>Wildcard Type
	!
	!--

	WILD_CATEGORY$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 128%)

	!++
	! Abstract:FLD07
	!	^*(07) Wildcard Category\*
	!
	! Index:
	!	.x Name/Address Labels>Wildcard Category
	!
	!--

	LABEL_COUNT% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(7%), -1%))
	LABEL_COUNT% = 1% IF LABEL_COUNT% < 1%

	!++
	! Abstract:FLD08
	!	^*(08) Labels/Customer\*
	!
	! Index:
	!	.x Name/Address Labels>Wildcard Category
	!
	!--

	ONLY_ACTIVE$ = LEFT(UTL_REPORTX::OPTDEF(8%), 1%)

	!++
	! Abstract:FLD09
	!	^*(09) Only Active\*
	!
	! Index:
	!	.x Name/Address Labels>Wildcard Category
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_COUNTRY.OPN"
	USE
		CONTINUE 320
	END WHEN

320	SELECT SORTBY$

	CASE "Z"
		CALL ENTR_3MESSAGE(SCOPE, "Creating work file . . .", 1% + 16%)

		CALL ASSG_CHANNEL(AR_TEMP.CH%, STAT%)
		CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

		OPEN UTL_WORK.DEV$ + "AR_TEMP.TMP" FOR OUTPUT AS FILE AR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			TEMPORARY, &
			BUFFER 32%, &
			MAP AR_35CUSTOM, &
			PRIMARY KEY &
			( &
				AR_35CUSTOM::ZIP, &
				AR_35CUSTOM::CUSNUM &
			) DUPLICATES, &
			ALLOW NONE, &
			ACCESS MODIFY

330		WHEN ERROR IN
			RESET #AR_35CUSTOM.CH%
		USE
			CONTINUE ReportTitle
		END WHEN

 GetCustomRec:
		WHEN ERROR IN
			GET #AR_35CUSTOM.CH%, REGARDLESS
		USE
			CONTINUE ReportTitle
		END WHEN

		IF ((AR_35CUSTOM::ZIP <= TO_ITEM$) OR (TO_ITEM$ = "")) AND &
			(AR_35CUSTOM::ZIP >= FROM_ITEM$) AND &
			((ONLY_ACTIVE$ <> "Y") OR (AR_35CUSTOM::SSTATUS = "A"))
		THEN
			PUT #AR_TEMP.CH%
		END IF

		GOTO GetCustomRec

	CASE "S"
340		CALL ENTR_3MESSAGE(SCOPE, "Creating work file . . .", 1% + 16%)

		CALL ASSG_CHANNEL(AR_TEMP.CH%, STAT%)
		CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

		OPEN UTL_WORK.DEV$ + "AR_TEMP.TMP" FOR OUTPUT AS FILE AR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			TEMPORARY, &
			BUFFER 32%, &
			MAP AR_35CUSTOM, &
			PRIMARY KEY &
			( &
				AR_35CUSTOM::SALESMAN, &
				AR_35CUSTOM::CUSNUM &
			) DUPLICATES, &
			ALLOW NONE, &
			ACCESS MODIFY

345		WHEN ERROR IN
			RESET #AR_35CUSTOM.CH%
		USE
			CONTINUE ReportTitle
		END WHEN

 GetSlmRec:
		WHEN ERROR IN
			GET #AR_35CUSTOM.CH%, REGARDLESS
		USE
			CONTINUE ReportTitle
		END WHEN

		IF ((AR_35CUSTOM::SALESMAN <= TO_ITEM$) OR (TO_ITEM$ = "")) AND &
			(AR_35CUSTOM::SALESMAN >= FROM_ITEM$) AND &
			((ONLY_ACTIVE$ <> "Y") OR (AR_35CUSTOM::SSTATUS = "A"))
		THEN
			PUT #AR_TEMP.CH%
		END IF

		GOTO GetSlmRec

	END SELECT

	%PAGE

16000	!*******************************************************************
	! Title
	!*******************************************************************

 ReportTitle:
	IF (SORTBY$ = "Z") OR (SORTBY$ = "S")
	THEN
		CLOSE AR_35CUSTOM.CH%
		AR_35CUSTOM.CH% = AR_TEMP.CH%
	END IF

	!
	! Calculate length for individual labels
	!
	XLINE% = UTL_REPORTX::PAGELEN
	XLINE% = 6% IF (XLINE% >= 66%) OR (XLINE% = 0%) OR &
		(UTL_REPORTX::PRINTTO = 1%)

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AR_35CUSTOM.CH%, KEY #K_NUM%
		ELSE
			FIND #AR_35CUSTOM.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "AR_35CUSTOM"
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
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORTBY$
	CASE "N"
		GOTO ExitTotal IF (AR_35CUSTOM::CUSNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "T"
		GOTO ExitTotal IF (AR_35CUSTOM::TTYPE > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "C"
		GOTO ExitTotal IF (AR_35CUSTOM::CATEGORY > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "A"
		GOTO ExitTotal IF (AR_35CUSTOM::ALPSRT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

 !	CASE 'Z'
 !		GOTO ExitTotal IF (AR_35CUSTOM::ZIP > TO_ITEM$) AND &
 !			TO_ITEM$ <> ""

	END SELECT

	!
	! Check Wildcard Customer Number
	!
	IF WILD_CUST$ <> ""
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(AR_35CUSTOM::CUSNUM, WILD_CUST$) = 0%
	END IF

	IF ((ONLY_ACTIVE$ = "Y") AND (AR_35CUSTOM::SSTATUS <> "A"))
	THEN
		GOTO GetNextRec
	END IF

	!
	! Check Wildcard Customer Number
	!
	IF WILD_TYPE$ <> ""
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(AR_35CUSTOM::TTYPE, WILD_TYPE$) = 0%
	END IF

	!
	! Check Wildcard Customer Number
	!
	IF WILD_CATEGORY$ <> ""
	THEN
		GOTO GetNextRec &
			IF COMP_STRING(AR_35CUSTOM::CATEGORY, &
			WILD_CATEGORY$) = 0%
	END IF

17100	!
	! Try to get country definition
	!
	IF AR_35CUSTOM::COUNTRY = ""
	THEN
		THIS_COUNTRY$ = "US"
	ELSE
		THIS_COUNTRY$ = AR_35CUSTOM::COUNTRY
	END IF

	UTL_COUNTRY::DESCR = ""

	IF THIS_COUNTRY$ <> "US"
	THEN
		WHEN ERROR IN
			GET #UTL_COUNTRY.CH%, &
				KEY #0% EQ AR_35CUSTOM::COUNTRY, &
				REGARDLESS
		USE
			UTL_COUNTRY::DESCR = ""
			CONTINUE 17110
		END WHEN
	END IF

17110	!
	! Print out one line
	!
	XLINE$(I%) = "" FOR I% = 1% TO XLINE%

	!
	! Pull name/address up to remove any blank lines
	!
	XLINE1% = 2%	!START AT LINE 2 OF LABEL

	IF INCLUDE.CUSNUM$ = "Y"
	THEN
		XLINE$(2%) = TRM$(AR_35CUSTOM::CUSNAM) + &
			"  (" + TRM$(AR_35CUSTOM::CUSNUM) + ")"
	ELSE
		XLINE$(2%) = TRM$(AR_35CUSTOM::CUSNAM)
	END IF

	IF AR_35CUSTOM::ADD1 <> ""
	THEN
		XLINE1% = XLINE1% + 1%
		XLINE$(XLINE1%) = AR_35CUSTOM::ADD1
	END IF

	IF AR_35CUSTOM::ADD2 <> ""
	THEN
		XLINE1% = XLINE1% + 1%
		XLINE$(XLINE1%) = AR_35CUSTOM::ADD2
	END IF

	IF AR_35CUSTOM::ADD3 <> ""
	THEN
		XLINE1% = XLINE1% + 1%
		XLINE$(XLINE1%) = AR_35CUSTOM::ADD3
	END IF

	XLINE1% = XLINE1% + 1%

	IF UTL_COUNTRY::DESCR = ""
	THEN
		XLINE$(XLINE1%) = AR_35CUSTOM::CITY + "  " + &
			AR_35CUSTOM::STATE + "  " + &
			AR_35CUSTOM::ZIP
	ELSE
		XLINE$(XLINE1%) = AR_35CUSTOM::CITY + "  " + UTL_COUNTRY::DESCR
	END IF

	!
	! Create all the labels for this customer.
	!
	FOR J% = 1% TO LABEL_COUNT%

		FOR I% = 1% TO XLINE%
			CALL OUTP_LINENOTITLE("", UTL_REPORTX, XLINE$(I%), 0%)
		NEXT I%

	NEXT J%

17340	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!

 ExitProgram:
	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

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

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
