1	%TITLE "Payroll Paid Employees"
	%SBTTL "PR_RPRT_PAIDEMPLOYEES"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! ID:PR040
	!
	! Abstract:HELP
	!	.p
	!	The ^*Paid Employees\* gives a list of employees who have
	!	been paid (through payroll or Billing Agency) in a given
	!	quarter.
	!
	! Index:
	!	.x Report>Paid Employees
	!	.x Paid Employees>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_PAIDEMPLOYEES/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_PAIDEMPLOYEES, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_PAIDEMPLOYEES.OBJ;*
	!
	! Author:
	!
	!	09/10/91 - Kevin Handy
	!
	! Modification history:
	!
	!	12/18/91 - Kevin Handy
	!		Fixed bug in looking through PR_REG_ERNDED that it
	!		pulled addition records from PR_REG_TAXES instead
	!		of PR_REG_ERNDED.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to only look at "P" and "D" types in
	!		PR_REG_ERNDED (Ignore "A" types).
	!
	!	06/14/93 - Kevin Handy
	!		Added REGARDLESS to several get statements.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/16/97 - Kevin Handy
	!		Reformat source code
	!
	!	05/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	05/29/98 - Kevin Handy
	!		Modified to handle new 'f' final deduction code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/21/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED) PR_REG_ERNDED_CDD PR_REG_ERNDED

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP) GL_YYYY_PP_CDD GL_YYYY_PP

	RECORD WHOINBA_CDD
		STRING EMPNUM = 10%
	END RECORD

	MAP (WHOINBA) WHOINBA_CDD WHOINBA

	!
	! Dimensions
	!
	DECLARE INTEGER CONSTANT MAX.RANGE = 100%
	DIM GL_YYYY_PP_FILE$(MAX.RANGE)

	%PAGE

	ON ERROR GOTO 19000

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
	!	The ^*From Item\* field causes the
	!	report to print beginning with this particular
	!	item. The value must be in agreement with the value
	!	entered in field (03).
	!	.p
	!	A blank in this field will cause the report to begin
	!	with the first item in the file.
	!
	! Index:
	!	.x From Item>Paid Employees
	!	.x Paid Employees>From Item
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.p
	!	The ^*To Item\* field causes the report
	!	to end with this particular item. The value
	!	must be in agreement with field (03).
	!	.p
	!	A blank field will cause the report to end with the last item
	!	in the file.
	!
	! Index:
	!	.x To Item>Paid Employees
	!	.x Paid Employees>To Item
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Sort (NU,SO,NA)\*
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
	!	SO = Alphabetical (last name first)
	!	.le
	!	NA = Name
	!	.lm -10
	!	.p
	!	.els
	!	This field requires an entry and only the entries listed above
	!	are valid.
	!
	! Index:
	!	.x Sort>Paid Employees
	!	.x Paid Employees>Sort
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

	CASE "DP"
		K_NUM% = 4%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::DEPT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::DEPT))

	CASE ELSE
		K_NUM% = 2%
		FROM_ITEM$ = LEFT(FROM_ITEM$, LEN(PR_EMP_MASTER::SORT))
		TO_ITEM$ = LEFT(TO_ITEM$, LEN(PR_EMP_MASTER::SORT))

	END SELECT

	YYYY$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Report Year (YYYY)\*
	!	.p
	!	The ^*Report Year\* field enters the
	!	year for which this report is to print.
	!	.p
	!	The format for entry is YYYY.
	!
	! Index:
	!	.x Report Year>Paid Employees
	!	.x Paid Employees>Report Year
	!
	!--


	QTR% = VAL%(EDIT$(UTL_REPORTX::OPTDEF(4%), 132%))

	!++
	! Abstract:FLD05
	!	^*(05) Quarter\*
	!	.p
	!	The ^*Quarter\* field enters the payroll
	!	quarter for which this report is to be printed.
	!	.p
	!	The field will accommodate a one digit entry and requires an entry.
	!
	! Index:
	!	.x Quarter>Paid Employees
	!	.x Paid Employees>Quarter
	!
	!--

	FROMPERIOD$ = TRM$(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) From Period\*
	!	.p
	!	The ^*From Period\* is used to tell which General Ledger Period to
	!	start scanning for ^~Billing Agency\~ information.
	!	If you do not want to scan for ^~Billing Agency\~ information,
	!	or do not use ^~Billing Agency\~,
	!	then leave this field and the next one blank.
	!
	! Index:
	!	.x From Period>Paid Employees
	!	.x Paid Employees>From Period
	!
	!--

	TOPERIOD$ = TRM$(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*(07) To Period\*
	!	.p
	!	The ^*To Period\* is used to tell which General Ledger Period to
	!	end scanning for ^~Billing Agency\~ information.
	!	If you do not want to scan for ^~Billing Agency\~ information,
	!	or do not use ^~Billing Agency\~,
	!	then leave this field and the previous one blank.
	!
	! Index:
	!	.x To Period>Paid Employees
	!	.x Paid Employees>To Period
	!
	!--


	!*******************************************************************
	! Create cross reference file for those employees who were billed
	! through Billing Agency.
	! Unless FROMPERIOD or TOPERIOD is blank.
	!*******************************************************************

250	IF (FROMPERIOD$ = "") OR (TOPERIOD$ = "")
	THEN
		FROMPERIOD$ = ""
		TOPERIOD$ = ""
		GOTO 300
	END IF

	!======================================================================
	! WHOINBA file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(WHOINBA.CH%, STAT%)

	WHOINBA.NAME$ = WHOINBA.DEV$ + "WHOINBA.TMP"

	OPEN WHOINBA.NAME$ FOR OUTPUT AS FILE WHOINBA.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP WHOINBA, &
		BUFFER 32%, &
		TEMPORARY, &
		PRIMARY KEY &
			WHOINBA::EMPNUM NODUPLICATES, &
		ACCESS MODIFY, ALLOW NONE

	!
	! Lookup all GL files that exist
	!
	CALL FIND_FILE( GL_YYYY_PP.DEV$ + "GL_*.LED", GL_YYYY_PP_FILE$(), &
		16%, "", "")

	GL_YYYY_PP_FILE% = VAL%(GL_YYYY_PP_FILE$(0%))

	IF GL_YYYY_PP_FILE% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"General ledger files do not exist", 0%)
		GOTO ExitProgram
	ELSE
		GL_YYYY_PP_FILE$(LOOP%) = &
			MID(GL_YYYY_PP_FILE$(LOOP%), 4%, 4%) + &
			MID(GL_YYYY_PP_FILE$(LOOP%), 9%, 2%) &
				FOR LOOP% = 1% TO GL_YYYY_PP_FILE%
	END IF

260	FOR LOOP% = 1% TO GL_YYYY_PP_FILE%

		GOTO 290 IF (GL_YYYY_PP_FILE$(LOOP%) < FROMPERIOD$) OR &
			(GL_YYYY_PP_FILE$(LOOP%) > TOPERIOD$)

		YYYY_PP$ = LEFT(GL_YYYY_PP_FILE$(LOOP%), 4%) + "_" + &
			RIGHT(GL_YYYY_PP_FILE$(LOOP%), 5%)

		WHEN ERROR IN
			%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.OPN"

			GET #GL_YYYY_PP.CH%, KEY #1% GT "          ", REGARDLESS
		USE
			CONTINUE 290
		END WHEN

270		IF (GL_YYYY_PP::SOURCE = "BA") AND &
			(WHOINBA::EMPNUM <> GL_YYYY_PP::SUBACC)
		THEN
			WHOINBA::EMPNUM = GL_YYYY_PP::SUBACC
			WHEN ERROR IN
				PUT #WHOINBA.CH%
			USE
				CONTINUE 280
			END WHEN
		END IF

280		WHEN ERROR IN
			GET #GL_YYYY_PP.CH%, REGARDLESS
		USE
			CONTINUE 290
		END WHEN

		GOTO 270

290	NEXT LOOP%

300	!
	! Open employee master file
	!
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

340	!
	! Open Tax withholding register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "PAID EMPLOYEES REPORT"
	TITLE$(2%) = "For the " + NUM1$(QTR%) + " Quarter of " + YYYY$
	TITLE$(3%) = ""

	!
	! Define headings
	!
	TITLE$(4%) = "Employee   SocSecNum   Name"
	TITLE$(5%) = ""

	!
	! Line layouts
	!
	LYT_LINE$ = "$EmpNum:010,$SocSec:22,$EmpName:046"

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

	TOTAL_EMPLOYEES% = 0%

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

17040	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, &
			KEY #0% GE PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17050 IF ERR = 155% OR ERR = 11%
		FILENAME$ = "PR_REG_TAXES_" + YYYY$
		CONTINUE HelpError
	END WHEN

	WHILE (PR_REG_TAXES::EMPNUM = PR_EMP_MASTER::EMPNUM)

		GOTO 17100 IF (PR_REG_TAXES::TAXABLE(QTR% - 1%) <> 0.0) OR &
			(PR_REG_TAXES::TAX(QTR% - 1%) <> 0.0)
		GET #PR_REG_TAXES.CH%, REGARDLESS
	NEXT

17050	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, &
			KEY #0% GE PR_EMP_MASTER::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17060 IF ERR = 155% OR ERR = 11%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

	WHILE (PR_REG_ERNDED::EMPNUM = PR_EMP_MASTER::EMPNUM)

		IF PR_REG_ERNDED::ETYPE = "P" OR &
			PR_REG_ERNDED::ETYPE = "D" OR &
			PR_REG_ERNDED::ETYPE = "F"
		THEN
			GOTO 17100 &
				IF (PR_REG_ERNDED::QTR_DOLL(QTR% - 1%) <> 0.0)
		END IF

		WHEN ERROR IN
			GET #PR_REG_ERNDED.CH%, REGARDLESS
		USE
			CONTINUE 17060 IF ERR = 155% OR ERR = 11%
			FILENAME$ = "PR_REG_ERNDED_" + YYYY$
			CONTINUE HelpError
		END WHEN
	NEXT

17060	IF (WHOINBA.CH% <> 0%)
	THEN
		WHEN ERROR IN
			GET #WHOINBA.CH%, KEY #0% EQ PR_EMP_MASTER::EMPNUM
		USE
			CONTINUE 17070
		END WHEN

		GOTO 17100
	END IF

17070	GOTO 17020

17100	!
	! Print total for employee
	!
	TEXT$ = PR_EMP_MASTER::EMPNUM + " " + &
		PR_EMP_MASTER::EMPNAME

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TOTAL_EMPLOYEES% = TOTAL_EMPLOYEES% + 1%

17350	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	TEXT$ = "Total of " + NUM1$(TOTAL_EMPLOYEES%) + " Employees Printed"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
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
