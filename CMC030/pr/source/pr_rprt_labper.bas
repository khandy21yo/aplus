1	%TITLE "Labor Performance"
	%SBTTL "PR_RPRT_LABPER"
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
	! ID:PR026
	!
	! Abstract:HELP
	!	.p
	!	The ^*Labor Performance Report\* option
	!	prints the Labor Audit Report required by
	!	the U.S. Department of Labor. The report may cover any specified
	!	period of time and may be printed alphabetically by employee name or
	!	in employee number order.
	!	.p
	!	The report includes the following column headings:
	!	.b
	!	.lm +15
	!	.ls 0,"o"
	!	.le
	!	#Employee Number
	!	.le
	!	#Employee Name
	!	.le
	!	#Sub-Account Number
	!	.le
	!	#Operation Number
	!	.le
	!	#Type (Hourly or Piece Rate)
	!	.le
	!	#Standard Study Rate %
	!	.le
	!	#Standard Prevailing Wage
	!	.le
	!	#Standard Client Rate Per Hour
	!	.le
	!	#Standard Pieces Per Hour
	!	.le
	!	#Standard Client Pieces Per Hour
	!	.le
	!	#Standard Certificate Minimum Rate
	!	.le
	!	#Actual Rate Per Hour
	!	.le
	!	#Actual Piece Rate
	!	.le
	!	#Actual Hours Worked
	!	.le
	!	#Actual Pieces Produced
	!	.le
	!	#Certified Minimum Pay
	!	.le
	!	#Piece Rate Pay
	!	.le
	!	#Gross Actual Pay
	!	.le
	!	#Certified Rate Variance
	!	.le
	!	#Piece Rate Variance
	!	.le
	!	#Piece Unit Variance
	!	.els
	!
	! Index:
	!	.x Report>Labor Performance
	!	.x Labor Performance>Report
	!	.x Report>DOL Audit Report
	!
	! Option:
	!
	! Author:
	!
	!	12/11/87 - Frank F. Starman
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_LABPER
	!	$ LINK/EXE=PR_EXE:*.EXE PR_RPRT_LABPER, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_LABPER.OBJ;*
	!
	! Modification history:
	!
	!	06/23/89 - Kevin Handy
	!		Opened PR_TEMP as TEMPORARY instead of trying
	!		to remember to delete it.
	!
	!	06/15/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		be sent to either a spreadsheet or a DIF file.
	!
	!	03/18/91 - Kevin Handy
	!		Added eval_date field to pr_read_rate.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	04/15/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/30/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/22/2000 - Kevin Handy
	!		Add EFF_DATE parameter to PR_READ_RATE
	!
	!	11/20/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP	(PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.HB"
	MAP	(PR_HIS_PAY)	PR_HIS_PAY_CDD	PR_HIS_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP	(PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD PR_TAX_PROFILE_F

	!
	! External functions
	!
	EXTERNAL REAL	FUNCTION PR_READ_CERTRATE

	!
	! Dimension statements
	!
	DIM DATA_FILE$(200%)

	%PAGE

	ON ERROR GOTO 19000

	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 202%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.p
	!	The ^*Item\* field causes the report to print
	!	beginning with a particular item. The value must
	!	be in agreement with field (05).
	!	.p
	!	A blank in this field causes the report to begin with the
	!	first item in the file.
	!
	! Index:
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* field causes the report to
	!	end with a particular Item. The value must
	!	be in agreement with field (05).
	!	.p
	!	A blank in this field causes the report to end with the last
	!	item in the file.
	!
	! Index:
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Sort by (NU,NA,SO)\*
	!	.p
	!	The ^*Sort by\* field
	!	indicates how the report will be sorted.
	!	.p
	!	Valid codes are:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	NU = Number
	!	.le
	!	NA = Name
	!	.le
	!	SO = Sort Order
	!	.lm -10
	!	.els
	!	.p
	!	An entry is required in this field and only the above codes
	!	are valid.
	!
	! Index:
	!
	!--

	FROM_BATCH_NO$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(5%), 132%))

	!++
	! Abstract:FLD06
	!	^*(06) From Date\*
	!	.p
	!	The ^*From Date\* field causes the printing
	!	to begin with a particular date.
	!	.p
	!	A blank in this field causes the report to begin with the
	!	first date in the file.
	!
	! Index:
	!
	!--

	TO_BATCH_NO$ = DATE_STOREDATE(EDIT$(UTL_REPORTX::OPTDEF(6%), 132%))

	!++
	! Abstract:FLD07
	!	^*(07) To Date\*
	!	.p
	!	The ^*To Date\* field enters the date the report
	!	is to end printing.
	!	.p
	!	A blank in this field causes the report to end with the last
	!	date in the file.
	!
	! Index:
	!
	!--

	SELECT SORTBY$
	CASE "NU"
		K_NUM% = 0%
	CASE "NA"
		K_NUM% = 1%
	CASE ELSE
		K_NUM% = 2%
	END SELECT

	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

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

310	!
	! Open file
	!
	MIN_WAGE = 0.0
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"

		GET #PR_TAX_PROFILE.CH%, KEY #0% EQ "F  ", REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 5% OR ERR = 155%
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

	MIN_WAGE = PR_TAX_PROFILE_F::MIN_WAGE

	CLOSE PR_TAX_PROFILE.CH%

410	!
	! Create working file
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Creating work file.  Reading Pay folder.", 1%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT &
			AS FILE PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TRN_PAY, &
			TEMPORARY, &
			BUFFER 32%, &
			PRIMARY KEY &
			( &
				PR_TRN_PAY::EMPNUM, &
				PR_TRN_PAY::SUBACC, &
				PR_TRN_PAY::OPER, &
				PR_TRN_PAY::RTYPE &
			)	DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN


	!
	! Open all payroll folder files
	!
	FOR PR_LOOP% = 1% TO DATA_FILE%
		BATCH_NO$ = DATA_FILE$(PR_LOOP%)

420		!
		! Open Pay folder
		!
		USE_HISTORY% = 0%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			CONTINUE 425 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_TRN_PAY.CH%

		GOTO 430

425		!
		! Open pay history folder if journal not there
		!
		USE_HISTORY% = -1%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
		USE
			CONTINUE 490 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_HIS_PAY.CH%

430		WHEN ERROR IN
			RESET #PR_TMP_PAY.CH%, KEY #0%
		USE
			CONTINUE 490
		END WHEN

440		WHEN ERROR IN
			GET #PR_TMP_PAY.CH%, REGARDLESS
		USE
			CONTINUE 490 IF ERR = 11%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		!
		! If journal net there the set history map to journal
		!
		IF USE_HISTORY%
		THEN
			PR_TRN_PAY = PR_HIS_PAY
		END IF

		GOTO 440 IF PR_TRN_PAY::PTYPE = "A"

		PR_TRN_PAY::PR_END_DATE = BATCH_NO$

		PUT #PR_TEMP.CH%

		!
		! Get next payroll record
		!
		GOTO 440

490		CLOSE PR_TMP_PAY.CH%
		CALL ASSG_FREECHANNEL(PR_TRN_PAY.CH%)
		CALL ASSG_FREECHANNEL(PR_HIS_PAY.CH%)
	!
	! Get next payroll
	!
	NEXT PR_LOOP%

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Department of Labor Audit Report"
	TITLE$(2%) = "For The Period " + PRNT_DATE(FROM_BATCH_NO$, 8%) + &
		" Thru " + PRNT_DATE(TO_BATCH_NO$, 8%)
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = SPACE$(65%) + &
		"------------------Standard--------------------- | " + &
		"-------------Actual------------ | ----------Pay---------- |" + &
		" --------Variance----------"
	TITLE$(5%) = SPACE$(65%) + &
		"  Study      PV CltRate  Pieces  CltPcs    Cert | " + &
		"   Rate   Piece     Hrs  Pieces |    Cert  PcRate   GrAct |" + &
		" CertRate  PcRate   PcUnit "
	TITLE$(6%) = "Employee#  EmployeeName                   SubAcct#   Oper#    Tp " + &
		"Rate(%)    Wage   PerHr   PerHr   PerHr MinRate | " + &
		"  PerHr    Rate  Worked    Prod |  MinPay     Pay     Pay |" + &
		"      Var     Var      Var "
	TITLE$(7%) = "."

	!
	! Line layouts
	!
	LYT_LINE$ = "$EmpNum:010,$EmpName:041,$SubAcct:052,$Oper:061," + &
		"$Type:063,VStudyRate:072,VPVWage:080,VStRatePerHr:088," + &
		"VPcsPerHr:096,VCertMinRate:104,VActRatePerHr:112," + &
		"VPcRate:122,VActHrsWorked:130,VPcsProd:138,VCertMinPay:146," + &
		"VPcRatePay:156,VGrActPay:164,VCertRateVar:172," + &
		"VPcRateVar:183,VPcRateVar:191,$PT:192,VPcUnitVar:200,$UT:201"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_EMP_MASTER.CH%, KEY #K_NUM%
		ELSE
			FIND #PR_EMP_MASTER.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_EMP_MASTER"
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
	CASE ELSE
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

	GOSUB 18500

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!
	CLOSE PR_TEMP.CH%

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

	%Page

18500	!
	! Read Temp File
	!
	WHEN ERROR IN
		FIND #PR_TEMP.CH%, KEY #0% EQ PR_EMP_MASTER::EMPNUM, REGARDLESS
	USE
		CONTINUE ComeBack1 IF ERR = 155%
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

 NextJob:
18510	WHEN ERROR IN
		GET #PR_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ComeBack3 IF ERR = 11%
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	GOTO ComeBack3 IF PR_TRN_PAY::EMPNUM <> PR_EMP_MASTER::EMPNUM

	CERT = PR_READ_CERTRATE(PR_TRN_PAY::PR_END_DATE)

	CALL PR_READ_SUBJOPER(PR_TRN_PAY::OPER, PR_TRN_PAY::PR_END_DATE, &
		PIECE_RATE, HOUR_RATE)

	CALL PR_READ_RATE(PR_EMP_MASTER::EMPNUM, &
		PR_TRN_PAY::OPER, &
		PR_TRN_PAY::PR_END_DATE, &
		"", &
		"", &
		0.0, &
		0.0, &
		0%, &
		STDEFF, &
		EVALDATE$, &
		EFF_DATE$)

	GOSUB 18600 &
		IF TEST_SUBACC$ <> PR_TRN_PAY::SUBACC OR &
		TEST_OPER$ <> PR_TRN_PAY::OPER OR &
		TEST_TYPE$ <> PR_TRN_PAY::RTYPE OR &
		TEST_CERT <> CERT OR &
		TEST_STDEFF <> STDEFF OR &
		TEST_HOUR_RATE <> HOUR_RATE OR &
		TEST_PIECE_RATE <> PIECE_RATE OR &
		TEST_HOUR <> PR_TRN_PAY::HOUR_RATE OR &
		TEST_PIECE <> PR_TRN_PAY::PIECE_RATE

	TEST_EMPNUM$ = PR_EMP_MASTER::EMPNUM

	TEST_CERT = CERT
	TEST_STDEFF = STDEFF
	TEST_HOUR_RATE = HOUR_RATE
	TEST_PIECE_RATE = PIECE_RATE

	TEST_SUBACC$ = PR_TRN_PAY::SUBACC
	TEST_OPER$ = PR_TRN_PAY::OPER
	TEST_TYPE$ = PR_TRN_PAY::RTYPE
	TEST_HOUR = PR_TRN_PAY::HOUR_RATE
	TEST_PIECE = PR_TRN_PAY::PIECE_RATE

	LINE_HOUR = LINE_HOUR  + PR_TRN_PAY::REG_HR + PR_TRN_PAY::OVT_HR
	LINE_PIECE = LINE_PIECE + PR_TRN_PAY::PIECE

	GOTO NextJob

 ComeBack3:
	GOSUB 18600

	IF EMP_LINE%
	THEN
		TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
			"T O T A L" + &
			SPACE$(93%) + &
			"|" + &
			SPACE$(16%) + &
			FORMAT$(EMP_LINE_HOUR, "#####.##") + &
			FORMAT$(EMP_LINE_PIECE, "#####.##") + &
			" |" + &
			FORMAT$(EMP_CERT_PAY, "#####.##") + &
			FORMAT$(EMP_RATE_PAY, "#####.##") + &
			FORMAT$(EMP_ACT_PAY, "#####.##") + &
			" | "

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)
	END IF

	TEXT$ = SPACE$(113%) + &
		"|" + &
		SPACE$(32%) + &
		" |" + &
		SPACE$(24%) + &
		" | "

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 1%)

	EMP_LINE_HOUR = 0.0
	EMP_LINE_PIECE = 0.0
	EMP_CERT_PAY = 0.0
	EMP_RATE_PAY = 0.0
	EMP_ACT_PAY = 0.0

	EMP_LINE% = 0%

 ComeBack1:
	TEST_EMPNUM$ = ""
	RETURN


18600	!
	! Print a line
	!
	RETURN IF EDIT$(TEST_EMPNUM$, -1%) = ""

	IF PR_EMP_MASTER::EMPNUM = TEST_PRINT$
	THEN
		EMP_LINE% = -1%
		PR_EMP_MASTER::EMPNAME = SPACE$(LEN(PR_EMP_MASTER::EMPNAME))
	END IF

	RT$, UT$ = " "
	RATE_VAR, UNIT_VAR = 0.0

	IF LINE_HOUR <> 0.0
	THEN
		UNIT_VAR = FUNC_ROUND(TEST_STDEFF * TEST_PIECE_RATE * 0.01, 2%) - &
			LINE_PIECE / LINE_HOUR
		UT$ = "U" IF SGN(UNIT_VAR) = 1%
		UT$ = "F" IF SGN(UNIT_VAR) = -1%
		RATE_VAR = TEST_HOUR_RATE - TEST_PIECE * LINE_PIECE / LINE_HOUR
		RT$ = "U" IF SGN(RATE_VAR) = 1%
		RT$ = "F" IF SGN(RATE_VAR) = -1%
	END IF

	TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
		PR_EMP_MASTER::EMPNAME + " " + &
		TEST_SUBACC$ + " " + &
		TEST_OPER$ + " " + &
		TEST_TYPE$ + " " + &
		"  " + &
		FORMAT$(TEST_STDEFF, "###.##") + &
		FORMAT$(MIN_WAGE, "#####.##") + &
		FORMAT$(TEST_HOUR_RATE, "#####.##") + &
		FORMAT$(TEST_PIECE_RATE, "###.####") + &
		FORMAT$(FUNC_ROUND(TEST_STDEFF * TEST_PIECE_RATE * 0.01, &
			2%), "#####.##") + &
		FORMAT$(TEST_CERT, " ####.##") + &
		" |" + &
		FORMAT$(TEST_HOUR, "####.###") + &
		FORMAT$(TEST_PIECE, "####.###") + &
		FORMAT$(LINE_HOUR, "#####.##") + &
		FORMAT$(LINE_PIECE, "#####.##") + &
		" |" + &
		FORMAT$(FUNC_ROUND(TEST_CERT * LINE_HOUR, 2%), "#####.##") + &
		FORMAT$(FUNC_ROUND(TEST_PIECE * LINE_PIECE, 2%), "#####.##") + &
		FORMAT$(FUNC_ROUND(TEST_HOUR * LINE_HOUR, 2%), "#####.##") + &
		" | " + &
		FORMAT$(TEST_HOUR - TEST_CERT, "####.###") + &
		FORMAT$(RATE_VAR, "####.###") + RT$ + &
		FORMAT$(UNIT_VAR, "####.###") + UT$

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, -1%)

	TEST_PRINT$ = PR_EMP_MASTER::EMPNUM
	EMP_LINE_HOUR = EMP_LINE_HOUR + LINE_HOUR
	EMP_LINE_PIECE = EMP_LINE_PIECE + LINE_PIECE
	EMP_CERT_PAY = EMP_CERT_PAY + FUNC_ROUND(TEST_CERT * LINE_HOUR, 2%)
	EMP_RATE_PAY = EMP_RATE_PAY + FUNC_ROUND(TEST_PIECE * LINE_PIECE, 2%)
	EMP_ACT_PAY = EMP_ACT_PAY + FUNC_ROUND(TEST_HOUR * LINE_HOUR, 2%)

	LINE_HOUR = 0.0
	LINE_PIECE = 0.0

	RETURN

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
	!+-+-+
	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects designated items to be
	!	printed by entering a "Wildcard" selection in this field.
	!
	! Index:
	!	.x Wildcard
	!
	!--
