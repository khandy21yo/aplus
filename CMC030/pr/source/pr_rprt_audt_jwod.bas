1	%TITLE "Payroll JWOD Audit Report"
	%SBTTL "PR_RPRT_AUDT_JWOD"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	! ID:PR057
	!
	! Abstract:HELP
	!	.p
	!	The ^*Employee JWOD Audit report\* shows how the company time is
	!	distributed and gives the gross totals for the time, rate, and units.
	!
	! Index:
	!	.x Report>Employee JWOD Audit
	!	.x Employee JWOD Audit>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_AUDT_JWOD/LINE/NOOPT
	!	$ LINK/EXE=PR_EXE: PR_RPRT_AUDT_JWOD, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_AUDT_JWOD.OBJ;*
	!
	! Author:
	!
	!	05/29/90 - Aaron Redd
	!
	! Modification history:
	!
	!	06/14/90 - Aaron Redd
	!		Added line layout information so that report could
	!		be sent to a spreadsheet.
	!
	!	06/03/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" types in PR_PAY.
	!
	!	01/03/91 - Kevin Handy
	!		Fixed error trapping for 17110 so it wasn't
	!		duplicated.
	!
	!	10/06/93 - Kevin Handy
	!		Modify dimension from 2000 to 4000.
	!
	!	10/06/93 - Kevin Handy
	!		Modified to use a temporary file instead of
	!		opening each individual folder each time a
	!		new employee is printed.
	!
	!	01/25/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	01/18/95 - Kevin Handy
	!		Added from/to job number (subaccount)
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Remove unsolicited input stuff.
	!
	!	09/10/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/17/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set Options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY
	MAP (PR_HIS_PAY)	PR_TRN_PAY_CDD	PR_HIS_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_JWOD_CROSS_REF.HB"
	MAP (PR_JWOD_CROSS_REF)	PR_JWOD_CROSS_REF_CDD	PR_JWOD_CROSS_REF

	!
	! Variable/Constant Declarations
	!
	DECLARE INTEGER CONSTANT OPEN_MAX = 24
	DECLARE INTEGER CONSTANT FILE_MAX = 4000
	DECLARE REAL TEMP_RATE
	DECLARE STRING JWOD_FLAG
	DECLARE INTEGER JWOD_NUM, EMP_NUM

	!
	! Dimension
	!
	DIM DATA_FILE$(FILE_MAX), &
		USE_HISTORY%(OPEN_MAX), PR_TMP_PAY.CH%(OPEN_MAX)

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Payroll Date\*
	!	.p
	!	The ^*From Payroll Date\* field specifies
	!	the payroll date with which the report is to begin printing.
	!	A blank setting causes the report to begin with the first
	!	date in the file.
	!
	! Index:
	!
	!--

	FROM_BATCH_NO$ = DATE_STOREDATE(FROM_BATCH_NO$)
	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Payroll Date\*
	!	.p
	!	The ^*To Payroll Date\* field specifies the date
	!	with which the report is to end printing. A blank setting
	!	causes the report to print to the end of the file.
	!
	! Index:
	!
	!--

	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$)
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) From Item\*
	!	.p
	!	The ^*From Item\* field specifies the
	!	item from which the report will start printing.
	!	The value must be in agreement with
	!	field (05).
	!	.p
	!	A blank field will cause the report to begin with the
	!	first item in the file.
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) To Item\*
	!	.p
	!	The ^*To Item\* field specifies the
	!	item at which the report will end printing.
	!	The value must be in agreement with
	!	in field (05).
	!	.p
	!	A blank field will cause the report to end with the last
	!	item in the file.
	!
	! Index:
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Sort by (NU, NA, LO)\*
	!	.p
	!	The ^*Sort by\* field specifies how the report will be sorted.
	!	.p
	!	Valid codes are:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	NU = Number
	!	.le
	!	NA = Name
	!	.le
	!	LO = Location
	!	.lm -10
	!	.p
	!	.els
	!	An entry is required in this field. The only valid codes are
	!	shown above.
	!
	! Index:
	!
	!--

	FROMJOB$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) From Subaccount\*
	!	.lm +5
	!	.b
	!	Specifies the beginning job number to include.
	!	.lm -5
	!	.p
	!
	! Index:
	!
	!--

	TOJOB$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) To Subaccount\*
	!	.lm +5
	!	.b
	!	Specifies the ending job number to include.
	!	.lm -5
	!	.p
	!
	! Index:
	!
	!--

	SELECT SORTBY$

	CASE "NU"
		K_NUM% = 0%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::EMPNUM))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::EMPNUM))

	CASE "NA"
		K_NUM% = 1%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::EMPNAME))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::EMPNAME))

	CASE "SN"
		K_NUM% = 3%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::SSN))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::SSN))

	CASE "LO"
		K_NUM% = 4%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::LOCATION))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::LOCATION))

	CASE ELSE
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::SORT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::SORT))

	END SELECT

	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	CALL PR_FIND_DETAILFILE(FROM_BATCH_NO$, &
		TO_BATCH_NO$, &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	DATA_FILE% = VAL%(DATA_FILE$(0%))

	GOTO ExitProgram IF DATA_FILE% = 0%

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

305	!
	! Open up a temporary file
	!
	CALL ASSG_CHANNEL(PR_TMP_PAY.CH%, STAT%)
	CALL READ_DEVICE("PR_TRN_PAY",PR_TRN_PAY.DEV$, STAT%)

	PR_TMP_PAY.NAME$ = PR_TRN_PAY.DEV$ + "PR_TEMP.TMP"

	OPEN PR_TMP_PAY.NAME$ FOR OUTPUT AS FILE PR_TMP_PAY.CH%, &
		ORGANIZATION INDEXED FIXED, &
		TEMPORARY, &
		BUFFER 32%, &
		MAP PR_HIS_PAY, &
		PRIMARY KEY &
		( &
			PR_HIS_PAY::EMPNUM, &
			PR_HIS_PAY::PR_END_DATE &
		) DUPLICATES, &
		ACCESS MODIFY, ALLOW NONE

	!
	! Open up the pay file.  If there are more than OPEN_MAX files
	! then skip to slower logic - open close files
	!
	FOR PR_LOOP% = 1% TO DATA_FILE%

		BATCH_NO$ = DATA_FILE$(PR_LOOP%)

		CALL ENTR_3MESSAGE(SCOPE, "Processing: " + BATCH_NO$, 1%)

310		!
		! Open Pay folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			CONTINUE 320 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		THIS.CH% = PR_TRN_PAY.CH%
		USE_HISTORY% = 0%

		GOTO 330

320		!
		! Open pay history folder if journal not there
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
		USE
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		THIS.CH% = PR_HIS_PAY.CH%
		USE_HISTORY% = -1%

330		!
		! Get first record
		!
		WHEN ERROR IN
			RESET #THIS.CH%
		USE
			CONTINUE 338
		END WHEN

335		!
		! Process all records in this file
		!
		WHEN ERROR IN
			GET #THIS.CH%
		USE
			CONTINUE 338
		END WHEN

		IF (USE_HISTORY% = 0%)
		THEN
			PR_HIS_PAY = PR_TRN_PAY
		END IF

		GOTO 335 IF PR_HIS_PAY::PTYPE = "A"

		IF (SORTBY$ = "NU")
		THEN
			GOTO 335 IF PR_HIS_PAY::EMPNUM < FROM_ITEM$
			GOTO 335 IF PR_HIS_PAY::EMPNUM > TO_ITEM$ AND TO_ITEM$ <> ""
		END IF

		GOTO 335 IF PR_HIS_PAY::SUBACC > FROMJOB$ AND FROMJOB$ <> ""
		GOTO 335 IF PR_HIS_PAY::SUBACC < TOJOB$ AND TOJOB$ <> ""

337		PUT #PR_TMP_PAY.CH%

		GOTO 335

338		CLOSE THIS.CH%

	NEXT PR_LOOP%

340	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_JWOD_CROSS_REF.OPN"
	USE
		CONTINUE ReportTitle
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Payroll Time/Unit Audit Report"
	TITLE$(2%) = "For the Payroll Folders Dated From: " + &
		PRNT_DATE(FROM_BATCH_NO$, 8%) + &
		" To: " + PRNT_DATE(TO_BATCH_NO$, 8%)
	TITLE$(3%) = ""

	!
	! Define headers
	!
	TITLE$(4%) = SPACE$(70%) + &
		"--------------Hour-------------  -------Units-------"
	TITLE$(5%) = "Emp#  Payroll Date  Account            SubAcct    " + &
		"Oper     EC  PT  RC     Rate  Reg Hrs  Ovt Hrs Fac        " + &
		"Rate       Qty     Gross"
	TITLE$(6%) = ""

	!
	! Define line layout
	!
	LYT_LINE$ = "DPRDate:016,$Acct:038,$SubAcct:049,$Oper:058," + &
		"$Code:061,$PType:064,$RType:068,VHourlyRate:078," + &
		"VRegHrs:087,VOvtHrs:096,VOvtFactor:100,VPieceRate:111," + &
		"VPieceQty:121,VGrossPay:131"

	!
	! Set JWOD, employee counters to zero
	!
	JWOD_NUM, EMP_NUM = 0%

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	IF FROM_ITEM$ = ""
	THEN
		RESET #PR_EMP_MASTER.CH%, KEY #K_NUM%
	ELSE
		FIND #PR_EMP_MASTER.CH%, KEY #K_NUM% GE FROM_ITEM$, REGARDLESS
	END IF

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORTBY$

	CASE "NU"
		GOTO ExitTotal IF (PR_EMP_MASTER::EMPNUM > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "NA"
		GOTO ExitTotal IF (PR_EMP_MASTER::EMPNAME > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "SN"
		GOTO ExitTotal IF (PR_EMP_MASTER::SSN > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE "DP"
		GOTO ExitTotal IF (PR_EMP_MASTER::DEPT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	CASE ELSE
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

	END SELECT

	!
	! Initialize some variables
	!
	JWOD_FLAG = "N"
	EMP_TOTAL(I%) = 0.0 FOR I% = 1% TO 10%
	EMP_LINE_COUNTER% = 0%

17100	!
	! Get pay detail information
	!
	WHEN ERROR IN
		FIND #PR_TMP_PAY.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17200 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

17110	WHEN ERROR IN
		GET #PR_TMP_PAY.CH%, REGARDLESS
	USE
		CONTINUE 17200 IF ERR = 11%
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	GOTO 17200 IF (PR_EMP_MASTER::EMPNUM <> PR_HIS_PAY::EMPNUM)

	IF EMP_LINE_COUNTER% = 0%
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), &
			PR_EMP_MASTER::EMPNUM + " " + &
			PR_EMP_MASTER::EMPNAME, 0%)
	END IF

	!
	! Print employee record
	!
	TEXT$ = "      " + &
		PRNT_DATE(PR_HIS_PAY::PR_END_DATE, 8%) + "    " + &
		PR_HIS_PAY::ACCT + " " + &
		PR_HIS_PAY::SUBACC + " " + &
		PR_HIS_PAY::OPER + " " + &
		PR_HIS_PAY::CODE + "  " + &
		PR_HIS_PAY::PTYPE + "   " + &
		PR_HIS_PAY::RTYPE + "  " + &
		FORMAT$(PR_HIS_PAY::HOUR_RATE, "####.### ") + &
		FORMAT$(PR_HIS_PAY::REG_HR, "#####.## ") + &
		FORMAT$(PR_HIS_PAY::OVT_HR, "#####.## ") + &
		FORMAT$(PR_HIS_PAY::FACTOR, "###%  ") + &
		FORMAT$(PR_HIS_PAY::PIECE_RATE, "####.#### ") + &
		FORMAT$(PR_HIS_PAY::PIECE, "#####.### ") + &
		FORMAT$(PR_HIS_PAY::GROSS, "######.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Add one to the line counter
	!
	EMP_LINE_COUNTER% = EMP_LINE_COUNTER% + 1%

	!
	! Was the job a JWOD job?  If so, reset the flag
	!
17150	IF (JWOD_FLAG = "N") AND (PR_HIS_PAY::SUBACC <> "..........")
	THEN
		WHEN ERROR IN
			GET #PR_JWOD_CROSS_REF.CH%, &
				KEY #0% EQ PR_HIS_PAY::SUBACC, REGARDLESS
		USE
			CONTINUE 17160 IF (ERR = 9%)
			CONTINUE 17160 IF (ERR = 155%)
			FILENAME$ = "PR_JWOD_CROSS_REF"
			CONTINUE HelpError
		END WHEN

		JWOD_FLAG = "Y" IF (PR_JWOD_CROSS_REF::FLAG = "J")
	END IF

17160	GOTO ExitProgram IF UTL_REPORTX::STAT

	EMP_TOTAL(1%) = EMP_TOTAL(1%) + PR_HIS_PAY::REG_HR
	EMP_TOTAL(2%) = EMP_TOTAL(2%) + PR_HIS_PAY::OVT_HR
	EMP_TOTAL(3%) = EMP_TOTAL(3%) + PR_HIS_PAY::PIECE
	EMP_TOTAL(4%) = EMP_TOTAL(4%) + PR_HIS_PAY::GROSS

	GOTO 17110

17200	!
	! Skip the totals and averages if less than two lines
	!
	GOTO 17300 IF (EMP_LINE_COUNTER% < 2%)

	!
	! Print employee total
	!
	TEXT$ = SPACE$(48%) + "Employee Total                 " + &
		FORMAT$(EMP_TOTAL(1%), "#####.## ") + &
		FORMAT$(EMP_TOTAL(2%), "#####.##                 ") + &
		FORMAT$(EMP_TOTAL(3%), "#####.### ") + &
		FORMAT$(EMP_TOTAL(4%), "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Print out Avg hourly rate line
	!
	IF ((EMP_TOTAL(1%) + EMP_TOTAL(2%)) = 0.0)
	THEN
		TEXT$ = SPACE$(48%) + &
			"Employee Average Rate    0.000 " + &
			SPACE$(53%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	ELSE
		TEMP_RATE = EMP_TOTAL(4%) / (EMP_TOTAL(1%) + EMP_TOTAL(2%))

		TEXT$ = SPACE$(48%) + "Employee Average Rate" + &
			FORMAT$(TEMP_RATE, "  ###.### ") + &
			SPACE$(53%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! Print JWOD flag and extra line if employee counter is greater than 0
	!
17300	IF EMP_LINE_COUNTER% > 0%
	THEN
		TEXT$ = SPACE$(48%) + "Worked JWOD job (Y/N)        " + &
			JWOD_FLAG + SPACE$(53%)

		EMP_NUM = EMP_NUM + 1%
		JWOD_NUM = JWOD_NUM + 1% IF (JWOD_FLAG = "Y")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	GRAND_TOTAL(I%) = GRAND_TOTAL(I%) + EMP_TOTAL(I%) &
		FOR I% = 1% TO 4%

17350	!
	! Go to next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	TEXT$ = SPACE$(48%) + "Grand Total                    " + &
		FORMAT$(GRAND_TOTAL(1%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(2%), "#####.##                 ") + &
		FORMAT$(GRAND_TOTAL(3%), "#####.### ") + &
		FORMAT$(GRAND_TOTAL(4%), "######.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT1$ = SPACE$(48%) + "Total of " + &
		FORMAT$(EMP_NUM, "####") + " employees; "

	IF (JWOD_NUM = 0%)
	THEN
		TEXT$ = TEXT1$ + "none worked on JWOD jobs"
	ELSE
		TEXT$ = TEXT1$ + &
			FORMAT$(JWOD_NUM, "####") + " worked on JWOD jobs"
		TEXT$ = TEXT1$ + "one worked on a JWOD job" IF (JWOD_NUM = 1%)
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

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

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
