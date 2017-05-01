1	%TITLE "Payroll Net Check Report"
	%SBTTL "PR_RPRT_TRN_NET_CHECK"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	! Abstract:HELP
	!	.p
	!	This program prints a payroll net check report
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TRN_NET_CHECK/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TRN_NET_CHECK, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TRN_NET_CHECK.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	06/19/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	06/04/91 - Kevin Handy
	!		Unwound error trapping.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/13/97 - Kevin Handy
	!		Handle FH Code
	!		Use integer for #key
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP	(PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP	(PR_TRN_PAY)	PR_TRN_PAY_CDD		PR_TRN_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP	(PR_TRN_DED)	PR_TRN_DED_CDD		PR_TRN_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP	(PR_TRN_CHECK)	PR_TRN_CHECK_CDD	PR_TRN_CHECK

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$)
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Pay folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
	USE
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

320	!
	! Open Deduction folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.OPN"
	USE
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Check folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.OPN"
	USE
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Net Check Register"
	TITLE$(2%) = "For the Payroll ending " + PRNT_DATE(BATCH_NO$, 8%)
	TITLE$(3%) = ""

	!
	! Column headings
	!
	TITLE$(4%) = "CkNum  EmpNum     EmpName                  " + &
		"Date     RegHrs   OvtHrs  GrossPay Non-Comp Misc/Ded" + &
		"    State  Federal     Fica  NetCheck"
	TITLE$(5%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$CheckNum:006,$EmpNum:017,$EmpName:041," + &
		"DPRDate:052,VRegHrs:061,VOvtHrs:070,VGrossPay:080," + &
		"VNonComp:089,VMiscDed:097,VState:106,VFederal:115," + &
		"VFica:124,VNetCheck:133"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #PR_TRN_CHECK.CH%, KEY #0%
		ELSE
			FIND #PR_TRN_CHECK.CH%, &
				KEY #0% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "PR_TRN_CHECK"
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
		GET #PR_TRN_CHECK.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11% OR ERR = 9%
		FILENAME$ = "PR_TRN_CHECK"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (PR_TRN_CHECK::CHECK > TO_ITEM$) AND &
		TO_ITEM$ <> ""

	EMP_TOTAL(I%) = 0.0 FOR I% = 1% TO 10%

17040	WHEN ERROR IN
		FIND #PR_TRN_PAY.CH%, &
			KEY #0% EQ PR_TRN_CHECK::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17060 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

17050	WHEN ERROR IN
		GET #PR_TRN_PAY.CH%, REGARDLESS
	USE
		CONTINUE 17060 IF ERR = 11%
		FILENAME$ = "PR_TRN_PAY"
		CONTINUE HelpError
	END WHEN

	GOTO 17060 IF PR_TRN_CHECK::EMPNUM <> PR_TRN_PAY::EMPNUM

	GOTO 17050 IF PR_TRN_PAY::PTYPE = "A"

	IF PR_TRN_PAY::PR_END_DATE = PR_TRN_CHECK::PR_END_DATE
	THEN
		EMP_TOTAL(1%) = FUNC_ROUND(EMP_TOTAL(1%) + &
			PR_TRN_PAY::REG_HR, 2%)
		EMP_TOTAL(2%) = FUNC_ROUND(EMP_TOTAL(2%) + &
			PR_TRN_PAY::OVT_HR, 2%)
		EMP_TOTAL(3%) = FUNC_ROUND(EMP_TOTAL(3%) + &
			PR_TRN_PAY::GROSS, 2%) &
			IF PR_TRN_PAY::PTYPE <> "O"

		EMP_TOTAL(4%) = FUNC_ROUND(EMP_TOTAL(4%) + &
			PR_TRN_PAY::GROSS, 2%) &
			IF PR_TRN_PAY::PTYPE = "O"
	END IF

	GOTO 17050

17060	WHEN ERROR IN
		FIND #PR_TRN_DED.CH%, &
			KEY #0% EQ PR_TRN_CHECK::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 17080 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

17070	WHEN ERROR IN
		GET #PR_TRN_DED.CH%, REGARDLESS
	USE
		CONTINUE 17080 IF ERR = 11%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	GOTO 17100 IF PR_TRN_DED::EMPNUM <> PR_TRN_CHECK::EMPNUM

	IF PR_TRN_DED::PR_END_DATE = PR_TRN_CHECK::PR_END_DATE
	THEN
		SELECT PR_TRN_DED::CODE

		CASE "SW"
			EMP_TOTAL(6%) = FUNC_ROUND(EMP_TOTAL(6%) + &
				PR_TRN_DED::AMOUNT, 2%)

		CASE "FW"
			EMP_TOTAL(7%) = FUNC_ROUND(EMP_TOTAL(7%) + &
				PR_TRN_DED::AMOUNT, 2%)

		CASE "FI", "FH"
			EMP_TOTAL(8%) = FUNC_ROUND(EMP_TOTAL(8%) + &
				PR_TRN_DED::AMOUNT, 2%)

		CASE ELSE
			EMP_TOTAL(5%) = FUNC_ROUND(EMP_TOTAL(5%) + &
				PR_TRN_DED::AMOUNT, 2%)

		END SELECT
	END IF

	GOTO 17070

17080	!
	! Master file look up
	!
	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, &
			KEY #0% EQ PR_TRN_CHECK::EMPNUM, &
			REGARDLESS
	USE
		PR_EMP_MASTER::EMPNAME = "??????????????????????????????????"

		CONTINUE 17100 IF ERR = 155%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

17100	!
	! Print total for employee
	!
	SUM = 0.0
	SUM = FUNC_ROUND(SUM + EMP_TOTAL(LOOP%), 2%) &
		FOR LOOP% = 1% TO 8%

	IF SUM <> 0.0
	THEN
		EMP_TOTAL(9%) = FUNC_ROUND(EMP_TOTAL(3%) - EMP_TOTAL(5%) - &
			EMP_TOTAL(6%) - EMP_TOTAL(7%) - &
			EMP_TOTAL(8%), 2%)

		GRAND_TOTAL(LOOP%) = GRAND_TOTAL(LOOP%) + EMP_TOTAL(LOOP%) &
			FOR LOOP% = 1% TO 9%

		TEXT$ = PR_TRN_CHECK::CHECK + " " + &
			PR_TRN_CHECK::EMPNUM + " " + &
			LEFT(PR_EMP_MASTER::EMPNAME, 23%) + " " + &
			PRNT_DATE(PR_TRN_CHECK::PR_END_DATE, 8%) + " " + &
			FORMAT$(EMP_TOTAL(1%), "#####.## ") + &
			FORMAT$(EMP_TOTAL(2%), "#####.## ") + &
			FORMAT$(EMP_TOTAL(3%), "######.## ") + &
			FORMAT$(EMP_TOTAL(4%), "#####.##") + &
			FORMAT$(EMP_TOTAL(5%), "#####.## ") + &
			FORMAT$(EMP_TOTAL(6%), "#####.## ") + &
			FORMAT$(EMP_TOTAL(7%), "#####.## ") + &
			FORMAT$(EMP_TOTAL(8%), "#####.##") + &
			FORMAT$(EMP_TOTAL(9%), "######.##")

		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

	END IF

17350	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "                  Grand Total                     " + &
		FORMAT$(GRAND_TOTAL(1%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(2%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(3%), "######.## ") + &
		FORMAT$(GRAND_TOTAL(4%), "#####.##") + &
		FORMAT$(GRAND_TOTAL(5%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(6%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(7%), "#####.## ") + &
		FORMAT$(GRAND_TOTAL(8%), "#####.##") + &
		FORMAT$(GRAND_TOTAL(9%), "######.##")

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
