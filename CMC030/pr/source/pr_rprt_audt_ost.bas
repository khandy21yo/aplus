1	%TITLE "Payroll State Tax Audit Report"
	%SBTTL "PR_RPRT_AUDT_OST"
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
	! ID:PR054
	!
	! Abstract:HELP
	!	.p
	!	The State Tax Audit Report
	!	shows the amount of state income tax, as well as other state taxes,
	!	that have been withheld during a specified folder date range.
	!	The following fields are included in this report:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	State
	!	.le
	!	Employer Tax Number
	!	.le
	!	Other State Tax Percent
	!	.le
	!	Other State Tax Limit
	!	.le
	!	Employee Number
	!	.le
	!	Employee Name
	!	.le
	!	State Earnings
	!	.le
	!	Other State Earnings
	!	.le
	!	State Tax
	!	.le
	!	Other State Tax
	!	.els
	!	.lm -15
	!	.note
	!	If the State Earnings or the OST Earnings show up as zero,
	!	you may need to resync the taxable and reportable fields
	!	in the earnings/deduction folder.
	!	.end note
	!
	! Index:
	!	.x Report>State Tax Audit
	!	.x State Tax Audit>Report
	!	.x Report>State Tax Audit
	!	.x State Tax Audit>Report
	!	.x Report>State Tax Audit
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_AUDT_OST/LINE/NOOPT
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_AUDT_OST, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_AUDT_OST.OBJ;*
	!
	! Author:
	!
	!	05/07/90 - Kevin Handy
	!
	! Modification history:
	!
	!	01/15/91 - Craig Tanner
	!		Added YYYY$ to some filename$ in error trapping.
	!
	!	01/29/92 - Kevin Handy
	!		Added more digits to grand total
	!
	!	11/30/92 - Kevin Handy
	!		Added error trap for when deduction file doesn't
	!		exist at all.
	!
	!	08/20/93 - Kevin Handy
	!		Modified to not print zero employee lines.
	!		This for LL where they had a resync go bad.
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Remove unsolicited_input stuff.
	!
	!	11/20/95 - Kevin Handy
	!		Increased file max from 2000 to 3000.
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
	!	11/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

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

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.HB"
	MAP (PR_TAX_TABLE)	PR_TAX_TABLE_CDD	PR_TAX_TABLE

	RECORD PR_TEMP_RECORD
		STRING	EMPLOYEE = 10%
		STRING	STATE = 2%
		STRING	SORT = 20%

		REAL	STATE_EARN
		REAL	STATE_TAX
		REAL	OST_EARN
		REAL	OST_TAX
	END RECORD

	MAP (PR_TEMP)	PR_TEMP_RECORD	PR_TEMP

	DECLARE INTEGER CONSTANT FILE_MAX = 3000%

	!
	! Dimension
	!
	DIM DATA_FILE$(FILE_MAX)

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	FROM_BATCH_NO$ = DATE_STOREDATE(FROM_BATCH_NO$)

	!++
	! Abstract:FLD01
	!	^*(01) Start Payroll Date\*
	!	.p
	!	The ^*Start Payroll Date\* field specifies the date of
	!	the payroll folder with which the report will begin printing.
	!	A blank field will cause the report to start
	!	with the first payroll folder date in the file.
	!
	! Index:
	!	.X State Tax Audit>Start Date
	!	.x Start Date>State Tax Audit
	!
	!--

	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$)

	!++
	! Abstract:FLD02
	!	^*(02) End Payroll Date\*
	!	.p
	!	The ^*End Payroll Date\* field specifies the date of
	!	the payroll folder with which the report is to end printing.
	!	A blank field will cause the report to end
	!	with the last payroll folder date in the file.
	!
	! Index:
	!	.x End Date>State Tax Audit
	!	.x State Tax Audit>End Date
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) From State\*
	!	.p
	!	The ^*From State\* field specifies the report should
	!	begin with a particular state.
	!	.p
	!	A blank field will cause the report to begin with the first state in
	!	the file.
	!
	! Index:
	!	.x State Tax Audit>From State
	!	.x From State>State Tax Audit
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) To State\*
	!	.p
	!	The ^*To State\* fiels specifies
	!	the report should end with a particular state.
	!	.p
	!	A blank field will cause the report to end with the last
	!	state in the file.
	!
	! Index:
	!	.x To State>State Tax Audit
	!	.x State Tax Audit>To State
	!
	!--


	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Sort by (NU,NA,LO,SN,SO)\*
	!	.p
	!	The ^*Sort by\* field specifies the how
	!	the report is to be sorted within each state.
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
	!	.le
	!	SN = Social Security Number
	!	.le
	!	SO = Alphabetical (last name first)
	!	.els
	!	.lm -10
	!	.p
	!	An entry is required in this field an only the above entries
	!	are valid.
	!
	! Index:
	!	.x State Tax Audit>Sort By
	!	.x Sort By>State Tax Audit
	!
	!--

	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	CALL PR_FIND_DETAILFILE(FROM_BATCH_NO$, &
		TO_BATCH_NO$, &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	DATA_FILE% = VAL%(DATA_FILE$(0%))

290	!
	! Create a temporary file
	!
	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)
	OPEN "PR_TEMP" FOR OUTPUT AS FILE PR_TEMP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		TEMPORARY, &
		BUFFER 32%, &
		MAP PR_TEMP, &
		PRIMARY KEY &
			(PR_TEMP::EMPLOYEE, PR_TEMP::STATE) &
			NODUPLICATES NOCHANGES, &
		ALTERNATE KEY &
			(PR_TEMP::STATE, PR_TEMP::SORT) &
			DUPLICATES NOCHANGES, &
		ACCESS MODIFY, &
		ALLOW NONE

300	!
	! Open up master file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

340	!
	! Open Tax Profile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"
	USE
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

350	!
	! Open Tax Table
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_TABLE.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "PR_TAX_TABLE_" + YYYY$
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
 !	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	!
	! Set up titles and whatnot
	!
	TITLE$(1%) = "Payroll State Tax Audit Report"
	TITLE$(2%) = "For Folders From: " + &
		MID(FROM_BATCH_NO$, 5%, 2%) + "/" + &
		MID(FROM_BATCH_NO$, 7%, 2%) + "/" + &
		LEFT(FROM_BATCH_NO$, 4%) + " To: " + &
		MID(TO_BATCH_NO$, 5%, 2%) + "/" + &
		MID(TO_BATCH_NO$, 7%, 2%) + "/" + &
		LEFT(TO_BATCH_NO$, 4%)
	TITLE$(3%) = ""
	TITLE$(4%) = ""

	TITLE$(5%) = "Employee                                      " + &
		"State      Ost     State      Ost"
	TITLE$(6%) = "Number     Name                               " + &
		" Earn     Earn       Tax      Tax"
	TITLE$(7%) = ""

	LYT_LINE$ = "$EMPNUM:11,$EMPNAM:41,VSTEARN:51,VOSTEARN:60," + &
		"VSTTAX:70,VOSTTAX:80"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	FOR PR_LOOP% = 1% TO DATA_FILE%

		BATCH_NO$ = DATA_FILE$(PR_LOOP%)
		CALL ENTR_3MESSAGE(SCOPE, "Processing: " + BATCH_NO$, 1%)

17020		!
		! Open Deduction folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.OPN"
		USE
			CONTINUE 17025 IF ERR = 5%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

		USE_HISTORY% = 0%
		GOTO 17100

17025		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.OPN"
		USE
			CONTINUE 17370 IF ERR = 5%
			FILENAME$ = "PR_HIS_DED"
			CONTINUE HelpError
		END WHEN

		PR_TRN_DED.CH% = PR_HIS_DED.CH%
		USE_HISTORY% = -1%

17100		RESET #PR_TRN_DED.CH%

 GetNextRec:
17200		!
		! Main loop starts here
		!
		GOTO ExitProgram IF UTL_REPORTX::STAT

		!
		! Get next record
		!
		WHEN ERROR IN
			GET #PR_TRN_DED.CH%, REGARDLESS
		USE
			CONTINUE 17370
		END WHEN

		!
		! If journal not there then set history map to journal
		!
		IF USE_HISTORY%
		THEN
			PR_TRN_DED = PR_HIS_DED
		END IF

		!
		! Skip if not state tax
		!
		GOTO 17200 IF INSTR(1%, "SW!SU!SI!SX", PR_TRN_DED::CODE) = 0%

		!
		! Skip if not for this state
		!
		GOTO 17200 IF PR_TRN_DED::TAX_CODE < FROM_ITEM$
		GOTO 17200 IF (PR_TRN_DED::TAX_CODE > TO_ITEM$) AND &
			(TO_ITEM$ <> "")

17210		!
		! If we haven't created a record in the temp file for this
		! employee yet, then create one, otherwise update the one
		! we already have.
		!
		WHEN ERROR IN
			GET #PR_TEMP.CH%, &
				KEY #0% EQ PR_TRN_DED::EMPNUM + &
				PR_TRN_DED::TAX_CODE
		USE
			CONTINUE 17230 IF ERR = 155%
			FILENAME$ = "PR_TEMP"
			CONTINUE HelpError
		END WHEN

		SELECT PR_TRN_DED::CODE

		CASE "SW"
			PR_TEMP::STATE_EARN = FUNC_ROUND(PR_TEMP::STATE_EARN + &
				PR_TRN_DED::REPORTABLE, 2%)
			PR_TEMP::STATE_TAX = FUNC_ROUND(PR_TEMP::STATE_TAX + &
				PR_TRN_DED::AMOUNT, 2%)

		CASE ELSE
			PR_TEMP::OST_EARN = FUNC_ROUND(PR_TEMP::OST_EARN + &
				PR_TRN_DED::REPORTABLE, 2%)
			PR_TEMP::OST_TAX = FUNC_ROUND(PR_TEMP::OST_TAX + &
				PR_TRN_DED::AMOUNT, 2%)
		END SELECT

17215		UPDATE #PR_TEMP.CH%

		GOTO 17200

17230		!
		! Get employee master record
		!
		IF (PR_EMP_MASTER::EMPNUM <> PR_TRN_DED::EMPNUM) AND &
			(SORTBY$ <> "NU")
		THEN
			WHEN ERROR IN
				GET #PR_EMP_MASTER.CH%, &
					KEY #0% EQ PR_TRN_DED::EMPNUM, &
					REGARDLESS
			USE
				PR_EMP_MASTER::EMPNAME = ""
				PR_EMP_MASTER::SSN = ""
				PR_EMP_MASTER::LOCATION = ""
				PR_EMP_MASTER::SORT = ""

				CONTINUE 17240
			END WHEN
		END IF

17240		PR_TEMP::EMPLOYEE = PR_TRN_DED::EMPNUM
		PR_TEMP::STATE = PR_TRN_DED::TAX_CODE

		SELECT SORTBY$
		CASE "NU"
			PR_TEMP::SORT = PR_TRN_DED::EMPNUM
		CASE "NA"
			PR_TEMP::SORT = PR_EMP_MASTER::EMPNAME
		CASE "SN"
			PR_TEMP::SORT = PR_EMP_MASTER::SSN
		CASE "LO"
			PR_TEMP::SORT = PR_EMP_MASTER::LOCATION
		CASE ELSE
			PR_TEMP::SORT = PR_EMP_MASTER::SORT
		END SELECT

		SELECT PR_TRN_DED::CODE

		CASE "SW"
			PR_TEMP::STATE_EARN = PR_TRN_DED::REPORTABLE
			PR_TEMP::STATE_TAX = PR_TRN_DED::AMOUNT
			PR_TEMP::OST_EARN = 0.0
			PR_TEMP::OST_TAX = 0.0

		CASE ELSE
			PR_TEMP::STATE_EARN = 0.0
			PR_TEMP::STATE_TAX = 0.0
			PR_TEMP::OST_EARN = PR_TRN_DED::REPORTABLE
			PR_TEMP::OST_TAX = PR_TRN_DED::AMOUNT
		END SELECT

		PUT #PR_TEMP.CH%

		GOTO 17200

17370		!
		! Make sure it doesn't close down files on us
		!
		CLOSE PR_TRN_DED.CH%
		CALL ASSG_FREECHANNEL(PR_TRN_DED.CH%)
		PR_TRN_DED.CH% = 0%

	!
	! Get data from next payroll folder
	!
	NEXT PR_LOOP%


	!*******************************************************************
	! Now, print out the information created
	!*******************************************************************

17400	RESET #PR_TEMP.CH%, KEY #1%

	TOTAL_STATE_EARN = 0.0
	TOTAL_STATE_TAX = 0.0
	TOTAL_OST_EARN = 0.0
	TOTAL_OST_TAX = 0.0

	STATE_STATE_EARN = 0.0
	STATE_STATE_TAX = 0.0
	STATE_OST_EARN = 0.0
	STATE_OST_TAX = 0.0
	STATE_COUNT% = 0%

	THIS_STATE$ = "%^&*#@@"

17410	GOTO ExitTotal IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #PR_TEMP.CH%
	USE
		CONTINUE ExitTotal
	END WHEN

	!
	! Ignoer if blank
	!
	IF (PR_TEMP::STATE_EARN = 0.0) AND &
		(PR_TEMP::STATE_TAX = 0.0) AND &
		(PR_TEMP::OST_EARN = 0.0) AND &
		(PR_TEMP::OST_TAX = 0.0)
	THEN
		GOTO 17410
	END IF

	IF PR_TEMP::STATE <> THIS_STATE$
	THEN
		GOSUB StateTotal
	END IF

	GOSUB PrintEmployeeLine

	!
	! Go to next record
	!
	GOTO 17410

	%PAGE

 ExitTotal:
17500	!
	! Handle end of report
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	!
	! Print final total
	!
	TEXT$ = "           " + &
		"Grand Total                  " + &
		FORMAT$(TOTAL_STATE_EARN, "########.##") + &
		FORMAT$(TOTAL_OST_EARN, "######.##") + &
		FORMAT$(TOTAL_STATE_TAX, "#######.##") + &
		FORMAT$(TOTAL_OST_TAX, "######.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

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

 PrintEmployeeLine:
18200	!******************************************************************
	! Print one line of employee history
	!******************************************************************

	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, &
			KEY #0% EQ PR_TEMP::EMPLOYEE, &
			REGARDLESS &
			IF PR_EMP_MASTER::EMPNUM <> PR_TEMP::EMPLOYEE
	USE
		PR_EMP_MASTER::EMPNAME = ""

		CONTINUE 18210
	END WHEN

18210	TEXT$ = &
		PR_TEMP::EMPLOYEE + " " + &
		PR_EMP_MASTER::EMPNAME + " " + &
		FORMAT$(PR_TEMP::STATE_EARN, "######.##") + &
		FORMAT$(PR_TEMP::OST_EARN, "######.##") + &
		FORMAT$(PR_TEMP::STATE_TAX, "#######.##") + &
		FORMAT$(PR_TEMP::OST_TAX, "######.##")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	TOTAL_STATE_EARN = TOTAL_STATE_EARN + PR_TEMP::STATE_EARN
	TOTAL_STATE_TAX = TOTAL_STATE_TAX + PR_TEMP::STATE_TAX
	TOTAL_OST_EARN = TOTAL_OST_EARN + PR_TEMP::OST_EARN
	TOTAL_OST_TAX = TOTAL_OST_TAX + PR_TEMP::OST_TAX

	STATE_STATE_EARN = STATE_STATE_EARN + PR_TEMP::STATE_EARN
	STATE_STATE_TAX = STATE_STATE_TAX + PR_TEMP::STATE_TAX
	STATE_OST_EARN = STATE_OST_EARN + PR_TEMP::OST_EARN
	STATE_OST_TAX = STATE_OST_TAX + PR_TEMP::OST_TAX
	STATE_COUNT% = STATE_COUNT% + 1%

	RETURN

 StateTotal:
	!******************************************************************
	! Print Total for State
	!******************************************************************
	IF STATE_COUNT% <> 0%
	THEN
		TEXT$ = "           " + &
			"Total for " + THIS_STATE$ + "  " + &
			FORMAT$(STATE_COUNT%, "###") + " Employees  " + &
			FORMAT$(STATE_STATE_EARN, "########.##") + &
			FORMAT$(STATE_OST_EARN, "######.##") + &
			FORMAT$(STATE_STATE_TAX, "#######.##") + &
			FORMAT$(STATE_OST_TAX, "######.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, -2%)

	END IF

	THIS_STATE$ = PR_TEMP::STATE

	REPNO$ = "????????????????????"
	OT_ANL_PCT = 0%
	OT_ANL_MAX = 0.0

18300	WHEN ERROR IN
		GET #PR_TAX_PROFILE.CH%, &
			KEY #0% EQ "S" + THIS_STATE$, &
			REGARDLESS
	USE
		CONTINUE 18310 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TAX_PROFILE"
		CONTINUE HelpError
	END WHEN

	REPNO$ = PR_TAX_PROFILE_F::REPNO

18310	WHEN ERROR IN
		GET #PR_TAX_TABLE.CH%, &
			KEY #0% EQ "S" + THIS_STATE$, &
			REGARDLESS
	USE
		CONTINUE 18390 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_TAX_TABLE_" + YYYY$
		CONTINUE HelpError
	END WHEN

	OT_ANL_PCT = PR_TAX_TABLE::OT_ANL_PCT / 100.0
	OT_ANL_MAX = PR_TAX_TABLE::OT_ANL_MAX

18390	TITLE$(4%) = "State: " + THIS_STATE$ + &
		" Employer Tax # " + REPNO$ + &
		"  OST % : " + FORMAT$(OT_ANL_PCT * 100.0, "##.###%") + &
		"  OST Limit: " + FORMAT$(OT_ANL_MAX, "####")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 500%)

	STATE_STATE_EARN = 0.0
	STATE_STATE_TAX = 0.0
	STATE_OST_EARN = 0.0
	STATE_OST_TAX = 0.0
	STATE_COUNT% = 0%

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
