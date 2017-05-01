1	%TITLE "Payroll Federal Year To Date"
	%SBTTL "PR_RPRT_FED_YTD"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1988 BY
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
	! ID:PR030
	!
	! Abstract:HELP
	!	.p
	!	The ^*Federal Year to Date\* reports the Federal Payroll for the year up to the
	!	present date. The following fields are included in this report:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Employee Number
	!	.le
	!	Name
	!	.le
	!	Year to Date Federal Wage
	!	.le
	!	Year to Date Federal Tax
	!	.le
	!	Year to Date FICA Wage
	!	.le
	!	Year to Date FICA Tax
	!	.le
	!	Quarter to Date Federal Wage
	!	.le
	!	Quarter to Date Federal Tax
	!	.le
	!	Quarter to Date FICA Wage
	!	.le
	!	Quarter to Date FICA Tax
	!	.els
	!
	! Index:
	!	.x Report>Federal Year to Date
	!	.x Federal Year to Date>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_FED_YTD/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_FED_YTD, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_FED_YTD.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	06/15/90 - Aaron Redd
	!		Added line layout information so that the report
	!		could be sent either to a spreadsheet or to a DIF file.
	!
	!	01/11/91 - Craig Tanner
	!		Added YYYY$ to some filenames in the error trapping.
	!
	!	01/23/91 - Kevin Handy
	!		Modified to use TAXABLE and REPORTABLE fields in
	!		the PR_REG_TAXES file instead of calculating from
	!		the PR_REG_ERNDED file.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/13/97 - Kevin Handy
	!		Handle FH code (roughly)
	!		Use integer for #key
	!
	!	08/25/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/12/2000 - Kevin Handy
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
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES) PR_REG_TAXES_CDD PR_REG_TAXES

	%PAGE

	ON ERROR GOTO 19000

	!
	! Other Variables
	!
	TAX_TYPE_TABLE$ = "FW!FI!FH!"

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Item\*
	!	.p
	!	The ^*Item\* field causes the printing
	!	to begin from this particular
	!	item.
	!	.p
	!	A blank in this field will cause the report to begin with
	!	the first item in the file.
	!
	! Index:
	!	.x From Item>Federal Year to Date
	!	.x Federal Year to Date>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* field causes the report
	!	to print ending with this particular item.
	!	.p
	!	A blank field will cause the report to end with the last item
	!	in the file.
	!
	! Index:
	!	.x To Item>Federal Year to Date
	!	.x Federal Year to Date>To Item
	!
	!--


	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) Year\*
	!	.p
	!	The ^*Year\* field allows for entry of the year for which
	!	this report is to print.
	!	.p
	!	An entry is required in this field. The format for entry
	!	is YYYY.
	!
	! Index:
	!	.x Year>Federal Year to Date
	!	.x Federal Year to Date>Year
	!
	! Datatype:TEXT
	! Size:4
	! Required:Y
	!--


	QTR% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(3%), 132%))

	!++
	! Abstract:FLD04
	!	^*(04) Quarter\*
	!	.p
	!	The ^*Quarter\* field allows for entry of the accounting
	!	quarter for which this report is to print.
	!	.p
	!	An entry is required in this field. The field will accommodate
	!	a one digit entry.
	!
	! Index:
	!	.x Quarter>Federal Year to Date
	!	.x Federal Year to Date>Quarter
	!
	! Datatype:TEXT
	! Size:1
	! Required:Y
	! Valid Input: 1,2,3,4
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*Sort by (NU, NA, LO)\*
	!	.p
	!	The ^*Sort by\* field enters a code which
	!	causes the report to be sorted in the indicated manner.
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
	!	.x Sort by>Federal Year to Date
	!	.x Federal Year to Date>Sort by
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

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Tax withholding register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.OPN"
	USE
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

	%PAGE


 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Federal YTD Report - " + NUM1$(QTR%) + &
		MID("stndrdth", QTR% * 2% - 1%, 2%) + " Quarter"
	TITLE$(2%) = "For the year of " + YYYY$
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = SPACE$(43%) + &
		"----------------Year to Date---------------   " + &
		"--------------Quarter to Date--------------"
	TITLE$(5%) = "Emp #      Name                               " + &
		"Fed Wage   Fed Tax   Fica Wage  Fica Tax      Fed Wage   " + &
		"Fed Tax   Fica Wage  Fica Tax"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "$EmpNum:010,$EmpName:041,VYTDFedWage:054," + &
		"VYTDFedTax:064,VYTDFICAWage:076,VYTDFICATax:086," + &
		"VQTDFedWage:100,VQTDFedTax:110,VQTDFICAWage:122,VQTDFICATax:132"

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
	CASE "SN"
		GOTO ExitTotal IF (PR_EMP_MASTER::SSN > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE "LO"
		GOTO ExitTotal IF (PR_EMP_MASTER::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	CASE ELSE
		GOTO ExitTotal IF (PR_EMP_MASTER::SORT > TO_ITEM$) AND &
			TO_ITEM$ <> ""
	END SELECT

	EMP(LOOP%) = 0.0 FOR LOOP% = 1% TO 8%


17040	WHEN ERROR IN
		FIND #PR_REG_TAXES.CH%, &
			KEY #0% EQ PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17060 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

17050	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, REGARDLESS
	USE
		CONTINUE 17060 IF ERR = 11%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

	GOTO 17060 IF PR_EMP_MASTER::EMPNUM <> PR_REG_TAXES::EMPNUM

	TAX_TYPE% = (INSTR(1%, TAX_TYPE_TABLE$, PR_REG_TAXES::TTYPE) + 2%) /3%

	SELECT TAX_TYPE%
	CASE 1%
		EMP(1%) = FUNC_ROUND(EMP(1%) + &
			PR_REG_TAXES::REPORTABLE(LOOP%), 2%) &
			FOR LOOP% = 0% TO QTR% - 1%
		EMP(2%) = FUNC_ROUND(EMP(2%) + &
			PR_REG_TAXES::TAX(LOOP%), 2%) &
			FOR LOOP% = 0% TO QTR% - 1%

		EMP(5%) = FUNC_ROUND(EMP(5%) + &
			PR_REG_TAXES::REPORTABLE(QTR% - 1%), 2%)
		EMP(6%) = FUNC_ROUND(EMP(6%) + &
			PR_REG_TAXES::TAX(QTR% - 1%), 2%)

	CASE 2%
		EMP(3%) = FUNC_ROUND(EMP(3%) + &
			PR_REG_TAXES::REPORTABLE(LOOP%), 2%) &
			FOR LOOP% = 0% TO QTR% - 1%
		EMP(4%) = FUNC_ROUND(EMP(4%) + &
			PR_REG_TAXES::TAX(LOOP%), 2%) &
			FOR LOOP% = 0% TO QTR% - 1%

		EMP(7%) = FUNC_ROUND(EMP(7%) + &
			PR_REG_TAXES::REPORTABLE(QTR% - 1%), 2%)
		EMP(8%) = FUNC_ROUND(EMP(8%) + &
			PR_REG_TAXES::TAX(QTR% - 1%), 2%)

	CASE 3%
		EMP(4%) = FUNC_ROUND(EMP(4%) + &
			PR_REG_TAXES::TAX(LOOP%), 2%) &
			FOR LOOP% = 0% TO QTR% - 1%

		EMP(8%) = FUNC_ROUND(EMP(8%) + &
			PR_REG_TAXES::TAX(QTR% - 1%), 2%)
	END SELECT

	GOTO 17050

17060	!

	%Page

17100	!
	! Print fed total
	!
	TEST% = 0%
	TEST% = -1% IF EMP(LOOP%) <> 0.0 FOR LOOP% = 1% TO 8%

	IF TEST%
	THEN
		TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
			LEFT(PR_EMP_MASTER::EMPNAME, 30%) + "  " + &
			FORMAT$(EMP(1%), "########.## ") + &
			FORMAT$(EMP(2%), "######.## ") + &
			FORMAT$(EMP(3%), "########.## ") + &
			FORMAT$(EMP(4%), "######.##   ") + &
			FORMAT$(EMP(5%), "########.## ") + &
			FORMAT$(EMP(6%), "######.## ") + &
			FORMAT$(EMP(7%), "########.## ") + &
			FORMAT$(EMP(8%), "######.##")

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

		TOTAL(LOOP%) = TOTAL(LOOP%) + EMP(LOOP%) FOR LOOP% = 1% TO 8%
	END IF

17350	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	TEXT$ = SPACE$(36%) + "Total  " + &
		FORMAT$(TOTAL(1%), "########.## ") + &
		FORMAT$(TOTAL(2%), "######.## ") + &
		FORMAT$(TOTAL(3%), "########.## ") + &
		FORMAT$(TOTAL(4%), "######.##   ") + &
		FORMAT$(TOTAL(5%), "########.## ") + &
		FORMAT$(TOTAL(6%), "######.## ") + &
		FORMAT$(TOTAL(7%), "########.## ") + &
		FORMAT$(TOTAL(8%), "######.##")

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
