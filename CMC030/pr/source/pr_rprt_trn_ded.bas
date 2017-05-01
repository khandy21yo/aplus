1	%TITLE "Payroll Deduction Report"
	%SBTTL "PR_RPRT_TRN_DED"
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
	! ID:PR004
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Deductions Register\* report option allows for entry to
	!	a report for a specific payroll folder which lists all non-tax payroll
	!	deductions which were made for that payroll.
	!	.b
	!	The report is listed in deduction code order. The report can be
	!	printed with or without a page break after each deduction code.
	!	.b
	!	It is recommended that the final version for each
	!	payroll folder be filed permanently.
	!	.b
	!	The fields include:
	!	.table 3
	!	.te
	!	Transaction Type
	!	.te
	!	Code
	!	.te
	!	Description of Deduction
	!	.te
	!	Employee Number
	!	.te
	!	Employee Name
	!	.te
	!	Payroll End Date
	!	.te
	!	Current Amount of Deduction (per employee)
	!	.te
	!	Year-to-Date Amount of Deduction (per employee)
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Deductions Register>Report
	!	.x Deductions Register Report
	!	.x Report>Deductions Register
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_TRN_DED/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_TRN_DED, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_TRN_DED.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	04/03/89 - Kevin Handy
	!		Added code for SI tax table.
	!
	!	06/23/89 - Kevin Handy
	!		Opened PR_TEMP file TEMPORARY instead of trying
	!		to remember to delete it.
	!
	!	06/18/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	11/19/90 - Kevin Handy
	!		Removed "KEY #PR_TEMP.CH%" from reset on 17020,
	!		because it was stupid.
	!
	!	11/20/90 - Kevin Handy
	!		Modifications to fix very strange problems in the
	!		report.  Looks like someone started to make it go
	!		from-to periods.
	!
	!	01/24/91 - Kevin Handy
	!		Modified so that "D" types that are actually added
	!		taxes "FW", "SW", ... do not show up on the-report
	!		as a deduction.
	!
	!	05/06/91 - Kevin Handy
	!		Cleaned up IF statement re. 01/24/91 edit.
	!
	!	05/15/91 - Kevin Handy
	!		Changed PR_TEMP to use a record structure.
	!
	!	09/03/92 - Kevin Handy
	!		Added "sortby" field to allow report to print
	!		either by deduction code or by location-deduction.
	!		Included employee name in the temp file since I
	!		had to read the master file it to get location
	!		anyway.
	!
	!	09/14/92 - Kevin Handy
	!		Fixed bug introduced by 09/03/92 where was not
	!		pulling up right employee name.
	!
	!	04/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/13/97 - Kevin Handy
	!		Handle FH Codes
	!
	!	05/29/98 - Kevin Handy
	!		Handle new 'F' deduction code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/21/99 - Kevin Handy
	!		Fix unsoliocited input
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD	PR_TRN_DED
	MAP (PR_HIS_DED)	PR_TRN_DED_CDD	PR_HIS_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED)	PR_REG_ERNDED_CDD	PR_REG_ERNDED

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	RECORD PR_TEMP_CDD
		STRING	SORTBY = 4%
		STRING	DTYPE = 1%
		STRING	CODE = 2%
		STRING	EMPNUM = 10%
		STRING	PR_END_DATE = 8%
		REAL	AMOUNT
		WORD	UPDATE_FLAG
		STRING	EMPNAME = 25%
	END RECORD

	MAP (PR_TEMP) PR_TEMP_CDD PR_TEMP

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! External functions
	!
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	%PAGE

	TAX_TYPE_TABLE$ = "!FI!FH!FW!SW!SX!CW!DW!EW!SU!SI!"

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Find a channel for the TEMP file
	!
	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

	%PAGE

 Init:
	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	BATCH_NO$ = DATE_STOREDATE(BATCH_NO$) ! Reformat to (YYYYMMDD)
	YYYY$ = LEFT(BATCH_NO$, 4%)

	!++
	! Abstract:FLD01
	!	.ts 55
	!	^*(01) Start Payroll Date	MMDDYYYY or MMDDYY\*
	!	.b
	!	.lm +5
	!	The ^*Start Payroll Date\* contains the date of
	!	the payroll folder with which the report will begin printing.
	!	.b
	!	A blank field will cause the report to start
	!	with the first payroll folder date in the file.
	!	.lm -5
	!
	! Index:
	!	.x Start Payroll Date>Deductions Register Report
	!	.x Payroll Date>Deductions Register Report
	!	.x Date>Deductions Register Report
	!	.x Deductions Register Report>Start Payroll Date
	!
	!--

	PAGE_IT$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	.ts 55
	!	^*(02) Page After Each Deduction	Y or N\*
	!	.b
	!	.lm +5
	!	The ^*Page After Each Deduction\* field causes a page
	!	break to occur after printing each deduction.
	!	.table 3,25
	!	.te
	!	Y	Yes page break
	!	.te
	!	N	No page break
	!	.lm -5
	!	.end table
	!
	! Index:
	!	.x Page After>Deductions Register Report
	!	.x Deductions Register Report>Page After
	!
	!--

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	.ts 55
	!	^*(03) Sort By	DC or LO\*
	!	.b
	!	.lm +5
	!	This field determines in what order the report will be
	!	printed.
	!	.table 3,25
	!	.te
	!	DC	By deduction code
	!	.te
	!	LO	By Location, then deduction code
	!	.lm -5
	!	.end table
	!
	! Index:
	!	.x Sort By>Deductions Register Report
	!	.x Deductions Register Report>Sort By
	!
	!--

	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Ded folder
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.OPN"
	USE
		CONTINUE 315 IF ERR = 5%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	GOTO 320

315	!
	! Open Ded history folder if journal folder not there
	!
	USE_HISTORY% = -1%

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.OPN"
	USE
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	PR_TRN_DED.CH% = PR_HIS_DED.CH%

320	!
	! Ernded register
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "PR_REG_ERNDED_" + YYYY$
		CONTINUE HelpError
	END WHEN

330	!
	! Ernded Definition
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		CONTINUE 360 IF ERR = 5%
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

360	!
	! Temporary file
	!
	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "PR_TEMP.TMP" FOR OUTPUT AS FILE PR_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP PR_TEMP, &
			TEMPORARY, &
			BUFFER 32%, &
			PRIMARY KEY &
			( &
				PR_TEMP::SORTBY, &
				PR_TEMP::DTYPE, &
				PR_TEMP::CODE, &
				PR_TEMP::EMPNUM, &
				PR_TEMP::PR_END_DATE &
			)	DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	%Page

400	!
	! Create work file
	!
	WHEN ERROR IN
		RESET #PR_TRN_DED.CH%, KEY #0%
	USE
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	CALL ENTR_3MESSAGE(SCOPE, "Creating work file by Deduction type.  Working. . .", 1%)

	!
	! Set up to trap interrupt
	!
	SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
		LOC(OUTP_XUNSOL) BY VALUE, &
		LOC(SCOPE::SMG_KBID) BY VALUE)

	RRR_FLAG% = 0%

410	WHEN ERROR IN
		GET #PR_TRN_DED.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 11%
		FILENAME$ = "PR_TRN_DED"
		CONTINUE HelpError
	END WHEN

	!
	! Set journal map equal to history map if use history is set
	!
	IF USE_HISTORY%
	THEN
		PR_TRN_DED = PR_HIS_DED
	END IF

	!
	! Handle any special junk in RRR_FLAG%
	!
	SELECT RRR_FLAG%

	!
	! Repaint screen
	!
	CASE SMG$K_TRM_F11, SMG$K_TRM_CTRLW
		SMG_STATUS% = SMG$REPAINT_SCREEN(SCOPE::SMG_PBID)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Help
	!
	CASE SMG$K_TRM_HELP
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)
		CALL HELP_34MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
			SCOPE::PRG_PROGRAM, "", SCOPE::PRG_ITEM)
		SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

	!
	! Interupt
	!
	CASE SMG$K_TRM_F6, SMG$K_TRM_F20
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		CALL MENU_3INTERRUPT(SCOPE)

		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

	!
	! Exit
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		UTL_REPORTX::STAT = -1
		GOTO ExitProgram

	END SELECT

	RRR_FLAG% = 0%

415	!
	! Find master file record
	!
	IF (PR_TRN_DED::EMPNUM <> PR_EMP_MASTER::EMPNUM)
	THEN
		WHEN ERROR IN
			GET #PR_EMP_MASTER.CH%, &
				KEY #0% EQ PR_TRN_DED::EMPNUM, &
				REGARDLESS
		USE
			PR_EMP_MASTER::EMPNUM = PR_TRN_DED::EMPNUM
			PR_EMP_MASTER::LOCATION = "????"
			PR_EMP_MASTER::EMPNAME = "????????????????????????????????????"

			CONTINUE 420 IF ERR = 155%
			FILENAME$ = "PR_EMP_MASTER"
			CONTINUE HelpError
		END WHEN

	END IF

420	!
	! Test to see if deduction and not tax
	!

	IF ((PR_TRN_DED::DTYPE = "D") OR (PR_TRN_DED::DTYPE = "C") OR &
		(PR_TRN_DED::DTYPE = "F")) AND &
		(INSTR(1%, TAX_TYPE_TABLE$, PR_TRN_DED::CODE) = 0%)
	THEN
		!
		! Handle sortby
		!
		SELECT SORTBY$
		CASE "LO"
			PR_TEMP::SORTBY = PR_EMP_MASTER::LOCATION
		CASE ELSE
			PR_TEMP::SORTBY = ""
		END SELECT

		PR_TEMP::DTYPE	= PR_TRN_DED::DTYPE
		PR_TEMP::CODE	= PR_TRN_DED::CODE
		PR_TEMP::EMPNUM	= PR_TRN_DED::EMPNUM
		PR_TEMP::EMPNAME= PR_EMP_MASTER::EMPNAME
		PR_TEMP::PR_END_DATE = PR_TRN_DED::PR_END_DATE
		PR_TEMP::AMOUNT	= PR_TRN_DED::AMOUNT
		PR_TEMP::UPDATE_FLAG = PR_TRN_DED::UPDATE_FLAG

		WHEN ERROR IN
			PUT #PR_TEMP.CH%
		USE
			FILENAME$ = "PR_TEMP"
			CONTINUE HelpError
		END WHEN

	END IF

	GOTO 410

	%PAGE

 ReportTitle:
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Deduction Report"
	TITLE$(2%) = "For the Payroll Folder Dated:  " + PRNT_DATE(BATCH_NO$, 8%)
	TITLE$(3%) = "."
	TITLE$(4%) = ""

	!
	! Column headings
	!
	TITLE$(5%) = "Typ Code CodeDescription               EmpNum     " + &
		"EmpName                  Date         Current                YTD"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$EType:001,$Code:007,$CodeDescr:036,$EmpNum:049," + &
		"$EmpName:072,DPrDate:081,VCurrent:095,VYTD:114"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	TEST_EMPNUM$ = ""
	TEST_CODE$ = ""

	WHEN ERROR IN
		RESET #PR_TEMP.CH%, KEY #0%
	USE
		FILENAME$ = "PR_TEMP"
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
		GET #PR_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	GOTO 17060 &
		IF TEST_CODE$ = PR_TEMP::SORTBY + PR_TEMP::DTYPE + PR_TEMP::CODE

	IF TEST_CODE$ <> ""
	THEN
		GOSUB EmployeeTotal IF TEST_EMPNUM$ <> ""

		GOTO ExitProgram IF UTL_REPORTX::STAT

		GOSUB CodeTotal

		GOTO ExitProgram IF UTL_REPORTX::STAT

		GOSUB LocationTotal IF (SORTBY$ = "LO") AND &
			(LEFT(TEST_CODE$, 4%) <> PR_TEMP::SORTBY)

		TEST_CODE$ = ""
	END IF

17040	PR_ERNDED_DEF::ETYPE	= PR_TEMP::DTYPE
	PR_ERNDED_DEF::CODE	= PR_TEMP::CODE
	PR_ERNDED_DEF::DESCR	= "?????????????????????????????????????"

	WHEN ERROR IN
		GET #PR_ERNDED_DEF.CH%, &
			KEY #0% EQ PR_TEMP::DTYPE + PR_TEMP::CODE, &
			REGARDLESS
	USE
		CONTINUE 17050 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

17050	TEXT$ = PR_ERNDED_DEF::ETYPE + "    " + &
		PR_ERNDED_DEF::CODE + "  " + &
		LEFT(PR_ERNDED_DEF::DESCR + SPACE$(27%), 27%) + "  "

17060	GOTO 17100 IF TEST_EMPNUM$ = PR_TEMP::EMPNUM + PR_TEMP::PR_END_DATE

	IF EMP_TOTAL(1%) <> 0.0
	THEN
		GOSUB EmployeeTotal
		GOTO ExitProgram IF UTL_REPORTX::STAT

	END IF

17070	TEXT$ = LEFT(TEXT$ + SPACE$(39%), 39%) + &
		PR_TEMP::EMPNUM + " " + &
		LEFT(PR_TEMP::EMPNAME, 22%) + " " + &
		PRNT_DATE(PR_TEMP::PR_END_DATE, 6%)

17100	!
	! Add employee deduction total
	!
	TEST_CODE$ = PR_TEMP::SORTBY + PR_TEMP::DTYPE + PR_TEMP::CODE
	TEST_EMPNUM$ = PR_TEMP::EMPNUM + PR_TEMP::PR_END_DATE
	TEST_UPDATE_FLAG% = PR_TEMP::UPDATE_FLAG

	EMP_TOTAL(1%) = EMP_TOTAL(1%) + PR_TEMP::AMOUNT

	!
	! Try for next record
	!
	GOTO 17020

 ExitTotal:
	!
	! Handle end of report
	!
	IF TEST_EMPNUM$ <> ""
	THEN
		GOSUB EmployeeTotal
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	GOSUB CodeTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOSUB LocationTotal IF SORTBY$ = "LO"
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = SPACE$(39%) + "Grand Total"
	TEXT$ = LEFT(SPACE$(81%), 81%) + &
		FORMAT$(GRAND_TOTAL(1%), "  #,###,###.##     ") + &
		FORMAT$(GRAND_TOTAL(2%), "  #,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Close channel
	!
	CLOSE PR_TEMP.CH%

17510	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 EmployeeTotal:
	!******************************************************************
	! Print Employee total
	!******************************************************************
18100	!
	! Look up ernded register record
	!
	WHEN ERROR IN
		GET #PR_REG_ERNDED.CH%, &
			KEY #0% EQ LEFT(TEST_EMPNUM$, 10%) + &
			RIGHT(TEST_CODE$, 5%), &
			REGARDLESS
	USE
		CONTINUE 18110
	END WHEN

	EMP_TOTAL(2%) = EMP_TOTAL(2%) + PR_REG_ERNDED::QTR_DOLL(I%) &
		FOR I% = 0% TO 3%

18110	IF (TEST_UPDATE_FLAG% AND 1%) <> 1%
	THEN
		EMP_TOTAL(2%) = EMP_TOTAL(2%) + EMP_TOTAL(1%)
	END IF

	TITLE$(3%) = "For Location " + LEFT(TEST_CODE$, 4%)

	TEXT$ = LEFT(TEXT$ + SPACE$(81%), 81%) + &
		FORMAT$(EMP_TOTAL(1%), "  #,###,###.##     ") + &
		FORMAT$(EMP_TOTAL(2%), "  #,###,###.##")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	FOR I% = 1% TO 2%
		CODE_TOTAL(I%) = CODE_TOTAL(I%) + EMP_TOTAL(I%)
		LOC_TOTAL(I%) = LOC_TOTAL(I%) + EMP_TOTAL(I%)
		EMP_TOTAL(I%) = 0.0
	NEXT I%

	TEXT$ = ""
	TEST_EMPNUM$ = ""

	RETURN

	%Page

 LocationTotal:
	!******************************************************************
	! Print Location total
	!******************************************************************

18200	TEXT$ = "            Total for Location " + LEFT(TEST_CODE$, 4%)

	TEXT$ = LEFT(TEXT$ + SPACE$(81%), 81%) + &
		FORMAT$(LOC_TOTAL(1%), "  #,###,###.##     ") + &
		FORMAT$(LOC_TOTAL(2%), "  #,###,###.##")

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TITLE$(3%) = "For Location " + PR_TEMP::SORTBY

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 3000%)

	TEXT$ = ""

	LOC_TOTAL(I%) = 0.0 FOR I% = 1% TO 2%

	RETURN

	%Page

 CodeTotal:
	!******************************************************************
	! Print Code total
	!******************************************************************
	TEXT$ = SPACE$(39%) + "Total"
	TEXT$ = LEFT(SPACE$(81%), 81%) + &
		FORMAT$(CODE_TOTAL(1%), "  #,###,###.##     ") + &
		FORMAT$(CODE_TOTAL(2%), "  #,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	GRAND_TOTAL(I%) = GRAND_TOTAL(I%) + CODE_TOTAL(I%) &
		FOR I% = 1% TO 2%

	CODE_TOTAL(I%) = 0.0 FOR I% = 1% TO 2%

	IF PAGE_IT$ = "Y"
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 1000%)
	END IF

	TEXT$ = ""

	RETURN

	%Page

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
