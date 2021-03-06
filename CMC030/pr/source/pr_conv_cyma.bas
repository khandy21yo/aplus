1	%TITLE "Convert Cyma PR files to CMC"
	%SBTTL "PR_CONV_CYMA"
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
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_CONV_CYMA/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_CONV_CYMA, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_CONV_CYMA.OBJ;*
	!
	! Author:
	!	07/25/91 - Frank F. Starman
	!
	! Modification history:
	!
	!	07/29/91 - Craig Tanner
	!		Added display of records on screen.
	!
	!	08/14/91 - Kevin Handy
	!		Removed A+.
	!
	!	10/10/91 - Kevin Handy
	!		Fixed lines out of sequence?
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/09/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/09/99 - Kevin Handy
	!		Add second parameter to UNPASTE_VIRTUAL_DISPLAY
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

	!
	! Extrenal Functions
	!
	EXTERNAL STRING FUNCTION CONV_ACCT

10	ON ERROR GOTO 19000

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)		PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP (PR_TAX_PROFILE_F)		PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES)	PR_REG_TAXES_CDD	PR_REG_TAXES

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.HB"
	MAP (PR_REG_ERNDED)	PR_REG_ERNDED_CDD	PR_REG_ERNDED

	!*******************************************************************
	! Initilize Convert
	!*******************************************************************

	CALL READ_INITIALIZE

	!
	! Create first data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY( 16%, 80%, DISPLAY_ID%,,, )
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	! Create the second data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY( 16%, 80%, DISPLAY_FILE%,,, )
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	!
	! PRINT BANNER
	!
	SMG_STATUS% = SMG$PUT_CHARS_WIDE(DISPLAY_ID%, "Convert Cyma PR files", &
		2%, 15%,SMG$M_BOLD)
	SMG_STATUS% = SMG$DRAW_LINE(DISPLAY_ID%, 4%, 1%, 4%, 80%, &
		SMG$M_BOLD)

 Password:

	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY( DISPLAY_ID%, SCOPE::SMG_PBID, 1%, 1% )
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	EXT$="???"
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "Company Password (XXX) :", 8%, 20%)
	EXT$ = ENTR_3STRING(SCOPE, DISPLAY_ID%, &
		"8;43", "Password", EXT$, 0%, "'E", "")

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT
	!
	! Control/C, Uparrow,
	!
	CASE 3%, SMG$K_TRM_UP, SMG$K_TRM_DOWN
		GOTO Password	! (Ignored)

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, SMG$K_TRM_DO, SMG$K_TRM_CR, &
		SMG$K_TRM_SELECT

	!
	! Bad Keys
	!
	CASE ELSE
		GOTO Password

	END SELECT

	! Ask user if they realy would like to convert all files

 ConfirmConv:
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "Convert files :", 9%, 20%)
	CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
		"9;36", "Confirm Converting", "N", 16%, "'", "N"), -1%)

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT
	!
	! Control/C, Uparrow,
	!
	CASE 3%, SMG$K_TRM_UP, SMG$K_TRM_DOWN
		GOTO ConfirmConv	! (Ignored)

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, SMG$K_TRM_DO, SMG$K_TRM_CR, &
		SMG$K_TRM_SELECT

	!
	! Bad Keys
	!
	CASE ELSE
		GOTO ConfirmConv

	END SELECT

	SMG_STATUS% = SMG$DELETE_CHARS(DISPLAY_ID%, 30%, 9%, 20%)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	GOTO ExitProgram IF CONF$ <> "Y"

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "File   :", 14%, 4%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "Record :", 15%, 4%)


1200	!
	! EMPLOYEE file does not exist, so create it
	!
	IF PASS%=0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Creating new PR_EMP_MASTER file", 1%)
		KILL "PR_EMP_MASTER.MAS"
		PASS% = -1%
	ELSE
		CALL ENTR_3MESSAGE(SCOPE, "Adding records into PR_EMP_MASTER file", 1%)
	END IF

2000	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.CRE"

2010	CALL ASSG_CHANNEL(PRISAM.CH%,STAT%)

	OPEN "PRISAM."+EXT$ FOR INPUT AS FILE PRISAM.CH%, &
		ORGANIZATION SEQUENTIAL

	LINPUT #PRISAM.CH%, LINE$

2020	LINPUT #PRISAM.CH%, LINE$

	GOTO 2020 IF LEFT(LINE$,1%)="1" OR VAL(MID$(LINE$,6%,4%))=0%

	PR_EMP_MASTER::EMPNUM	= MID$(LINE$,2%,4%)
	PR_EMP_MASTER::LOCATION	= ""
	PR_EMP_MASTER::EMPNAME	= ""
	PR_EMP_MASTER::ADD1	= ""
	PR_EMP_MASTER::ADD2	= ""
	PR_EMP_MASTER::CITY	= ""
	PR_EMP_MASTER::STATE	= ""
	PR_EMP_MASTER::ZIP	= ""
	PR_EMP_MASTER::COUNTRY	= ""
	PR_EMP_MASTER::PHONE	= ""
	PR_EMP_MASTER::SSN	= FORMAT$(VAL(MID$(LINE$,6%,4%)),"<0>###")
	PR_EMP_MASTER::SORT	= ""
	PR_EMP_MASTER::SUBACC	= ""
	PR_EMP_MASTER::ACCT	= ""
	PR_EMP_MASTER::TRADE	= ""
	PR_EMP_MASTER::OPER	= ""
	PR_EMP_MASTER::UNION	= ""
	PR_EMP_MASTER::LOCATION	= ""
	PR_EMP_MASTER::DEPT	= ""
	PR_EMP_MASTER::WORK_CENTER= ""
	PR_EMP_MASTER::EMP_SKILL= ""
	PR_EMP_MASTER::EMP_GRADE= ""
	PR_EMP_MASTER::DISABLED	= "N"
	PR_EMP_MASTER::PAYFREQ	= 0%
	PR_EMP_MASTER::SUI_SW	= ""
	PR_EMP_MASTER::TAX_PKG	= ""
	PR_EMP_MASTER::W2_1099	= ""
	PR_EMP_MASTER::BIRTH	= ""
	PR_EMP_MASTER::HIREDAY	= ""
	PR_EMP_MASTER::TERMDAY	= ""
	PR_EMP_MASTER::SEX	= ""
	PR_EMP_MASTER::RACE	= ""
	PR_EMP_MASTER::USCIT	= ""
	PR_EMP_MASTER::WRKPERMIT= ""
	PR_EMP_MASTER::HOMCNTRY	= ""
	PR_EMP_MASTER::ACTIVE_FLAG = ""
	PR_EMP_MASTER::RATE_TYPE= ""
	PR_EMP_MASTER::RATE_CDE	= ""
	PR_EMP_MASTER::WC	= ""

2030	PUT #PR_EMP_MASTER.CH%

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, PR_EMP_MASTER::EMPNUM+LINE$, 19%, 13%)

	GOTO 2020

2120	CLOSE #PRISAM.CH%

	CALL ASSG_FREECHANNEL(PRISAM.CH%)
	CALL ASSG_CHANNEL(PRMASTER.CH%,STAT%)

2130	OPEN "PRMASTER."+EXT$ FOR INPUT AS FILE PRMASTER.CH%, &
		ORGANIZATION SEQUENTIAL

	LINPUT #PRMASTER.CH%, LINE$
	COUNTER% = 1%

	!
	! Paste the second data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY( DISPLAY_FILE%, SCOPE::SMG_PBID, 5%, 1% )
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "PRMASTER  ", 18%, 13%)

 ConfirmMaster:
	CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_FILE%, &
		"", "Display Each Record", "N", 16%, "'", "N"), -1%)

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT
	!
	! Control/C, Uparrow,
	!
	CASE 3%, SMG$K_TRM_UP, SMG$K_TRM_DOWN
		GOTO ConfirmMaster	! (Ignored)

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, SMG$K_TRM_DO, SMG$K_TRM_CR, &
		SMG$K_TRM_SELECT

	!
	! Bad Keys
	!
	CASE ELSE
		GOTO ConfirmMaster

	END SELECT

	IF CONF$ <> "Y"
	THEN
		SMG_STATUS% = SMG$ERASE_DISPLAY(DISPLAY_FILE%)
		SMG_STATUS% = SMG$UNPASTE_VIRTUAL_DISPLAY(DISPLAY_FILE%, &
			SCOPE::SMG_PBID)
		GOTO 2140
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "# of Emp "+MID$(LINE$,1%,5%), 1%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "# of Del "+MID$(LINE$,6%,5%), 2%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "# of Ent "+MID$(LINE$,11%,5%), 3%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Company  "+MID$(LINE$,16%,30%), 4%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Address1 "+MID$(LINE$,46%,30%), 5%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "City     "+MID$(LINE$,76%,18%), 6%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "State    "+MID$(LINE$,94%,2%), 7%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Zip Code "+MID$(LINE$,96%,9%), 8%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Phone    "+MID$(LINE$,105%,10%), 10%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Fed ID # "+MID$(LINE$,115%,10%), 9%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "State ID "+MID$(LINE$,127%,12%), 10%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Last Pay "+MID$(LINE$,139%,6%), 11%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Cash Acc "+CONV_ACCT(MID$(LINE$,145%,8%)), 12%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "PR Expen "+CONV_ACCT(MID$(LINE$,153%,8%)), 1%, 40%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Fed Tax  "+CONV_ACCT(MID$(LINE$,161%,8%)), 2%, 40%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "StateTax "+CONV_ACCT(MID$(LINE$,169%,8%)), 3%, 40%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "FICA Tax "+CONV_ACCT(MID$(LINE$,177%,8%)), 4%, 40%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "FUTA Tax "+MID$(LINE$,185%,6%), 5%, 40%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Max FUTA "+MID$(LINE$,191%,8%), 6%, 40%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Annl Exm "+MID$(LINE$,199%,8%), 7%, 40%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "EmpFICA  "+MID$(LINE$,207%,6%), 8%, 40%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Max FICA "+MID$(LINE$,213%,8%), 9%, 40%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Audit Fl "+MID$(LINE$,221%,1%), 10%, 40%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "EmprFICA "+MID$(LINE$,222%,6%), 11%, 40%)

2140	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.CRE"

	PR_TAX_PROFILE_F::AUTH		= "F"
	PR_TAX_PROFILE_F::CODE		= MID$(LINE$,115%,10%)
	PR_TAX_PROFILE_F::REPNO		= ""
	PR_TAX_PROFILE_F::WH_ACCT	= ""
	PR_TAX_PROFILE_F::FICA_EX_ACCT	= CONV_ACCT(MID$(LINE$,177%,8%))
	PR_TAX_PROFILE_F::FICA_LIA_ACCT_EMPR = ""
	PR_TAX_PROFILE_F::FUI_EX_ACCT	= ""
	PR_TAX_PROFILE_F::FUI_LIA_ACCT	= ""
	PR_TAX_PROFILE_F::FUI_PCT	= VAL(MID$(LINE$,185%,6%))
	PR_TAX_PROFILE_F::FUI_MAX	= 0.0
	PR_TAX_PROFILE_F::CASH_ACCT	= CONV_ACCT(MID$(LINE$,145%,8%))
	PR_TAX_PROFILE_F::PR_ACCRUAL_ACCT = ""
	PR_TAX_PROFILE_F::MIN_WAGE	= 0.0

	!PUT #PR_TAX_PROFILE_F.CH%

	CLOSE #PR_TAX_PROFILE_F.CH%
	CALL ASSG_FREECHANNEL(PR_TAX_PROFILE_F.CH%)

2150	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "PRMASTER", 18%, 13%)
	LINPUT #PRMASTER.CH%, LINE$
	COUNTER% = COUNTER% + 1%

	IF CONF$ <> "Y"
	THEN
		IF NOT BEFORE_%
		THEN
			SMG_STATUS% = SMG$ERASE_DISPLAY(DISPLAY_FILE%)
			BEFORE_% = 1%
		END IF
		GOTO 2155
	END IF

 ConfirmNext:
	CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_FILE%, &
		"", "Display Each Record", "N", 16%, "'", "N"), -1%)

	IF NOT BEFORE_%
	THEN
		SMG_STATUS% = SMG$ERASE_DISPLAY(DISPLAY_FILE%)
	END IF

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT
	!
	! Control/C, Uparrow,
	!
	CASE 3%, SMG$K_TRM_UP, SMG$K_TRM_DOWN
		GOTO ConfirmNext	! (Ignored)

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, SMG$K_TRM_DO, SMG$K_TRM_CR, &
		SMG$K_TRM_SELECT

	!
	! Bad Keys
	!
	CASE ELSE
		GOTO ConfirmNext

	END SELECT

	IF CONF$ <> "Y"
	THEN
		GOTO 2155
	END IF

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Emp Name "+MID$(LINE$,1%,30%), 1%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Address1 "+MID$(LINE$,31%,30%), 2%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "City     "+MID$(LINE$,61%,18%), 3%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "State    "+MID$(LINE$,79%,2%), 4%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Zip Code "+MID$(LINE$,81%,9%), 5%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Phone    "+MID$(LINE$,90%,10%), 6%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "SSN      "+MID$(LINE$,100%,9%), 7%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Birth D  "+MID$(LINE$,109%,6%), 8%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Sex      "+MID$(LINE$,115%,1%), 9%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "DateHire "+MID$(LINE$,116%,6%), 10%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "DateTerm "+MID$(LINE$,122%,6%), 11%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Married  "+MID$(LINE$,128%,1%), 12%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Status   "+MID$(LINE$,134%,1%), 13%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Exempts  "+MID$(LINE$,135%,2%), 14%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Dept     "+MID$(LINE$,136%,4%), 15%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Location "+MID$(LINE$,138%,4%), 3%, 30%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Race     "+MID$(LINE$,142%,2%), 4%, 30%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Pay Per  "+MID$(LINE$,146%,1%), 5%, 30%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Pay Type "+MID$(LINE$,148%,1%), 6%, 30%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Pay Rate "+MID$(LINE$,149%,9%), 7%, 30%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "FixedFed "+MID$(LINE$,150%,8%), 8%, 30%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "FixedSta "+MID$(LINE$,159%,8%), 9%, 30%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "FedTable "+MID$(LINE$,167%,2%), 10%, 30%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "StaTable "+MID$(LINE$,175%,2%), 11%, 30%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Sta%ofFed"+MID$(LINE$,177%,2%), 12%, 30%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Deduct#1 "+MID$(LINE$,181%,1%), 13%, 30%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Amount   "+MID$(LINE$,182%,8%), 14%, 30%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Deduct#2 "+MID$(LINE$,190%,1%), 15%, 30%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Amount   "+MID$(LINE$,191%,8%), 3%, 55%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Deduct#3 "+MID$(LINE$,199%,1%), 4%, 55%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Amount   "+MID$(LINE$,200%,8%), 5%, 55%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Deduct#4 "+MID$(LINE$,208%,1%), 6%, 55%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Amount   "+MID$(LINE$,209%,8%), 7%, 55%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Deduct#5 "+MID$(LINE$,217%,1%), 8%, 55%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Amount   "+MID$(LINE$,218%,8%), 9%, 55%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Deduct#6 "+MID$(LINE$,226%,1%), 10%, 55%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Amount   "+MID$(LINE$,227%,8%), 11%, 55%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Deduct#7 "+MID$(LINE$,235%,1%), 12%, 55%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Amount   "+MID$(LINE$,236%,8%), 13%, 55%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Deduct#8 "+MID$(LINE$,244%,1%), 14%, 55%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Amount   "+MID$(LINE$,245%,8%), 15%, 55%)


2155	GET #PR_EMP_MASTER.CH%, KEY#3% EQ FORMAT$(COUNTER%, "<0>###")

	PR_EMP_MASTER::LOCATION	= MID$(LINE$,6%,4%)
	PR_EMP_MASTER::EMPNAME	= MID$(LINE$,1%,30%)
	PR_EMP_MASTER::ADD1	= MID$(LINE$,31%,30%)
	PR_EMP_MASTER::ADD2	= ""
	PR_EMP_MASTER::CITY	= MID$(LINE$,61%,18%)
	PR_EMP_MASTER::STATE	= MID$(LINE$,79%,2%)
	PR_EMP_MASTER::ZIP	= MID$(LINE$,81%,9%)
	PR_EMP_MASTER::COUNTRY	= "US"
	PR_EMP_MASTER::PHONE	= MID$(LINE$,90%,10%)
	!PR_EMP_MASTER::SSN	= MID$(LINE$,100%,9%)
	PR_EMP_MASTER::SORT	= MID$(LINE$,1%,30%)
	PR_EMP_MASTER::SUBACC	= ""
	PR_EMP_MASTER::ACCT	= ""
	PR_EMP_MASTER::TRADE	= ""
	PR_EMP_MASTER::OPER	= ""
	PR_EMP_MASTER::UNION	= ""
	PR_EMP_MASTER::LOCATION	= MID$(LINE$,142%,4%)
	PR_EMP_MASTER::DEPT	= MID$(LINE$,138%,4%)
	PR_EMP_MASTER::WORK_CENTER= ""
	PR_EMP_MASTER::EMP_SKILL= ""
	PR_EMP_MASTER::EMP_GRADE= ""
	PR_EMP_MASTER::DISABLED	= "N"
	PR_EMP_MASTER::PAYFREQ	= VAL(MID$(LINE$,148%,1%))
	PR_EMP_MASTER::SUI_SW	= ""
	PR_EMP_MASTER::TAX_PKG	= ""
	PR_EMP_MASTER::W2_1099	= ""
	PR_EMP_MASTER::BIRTH	= "19"+MID$(LINE$,113%,2%)+MID$(LINE$,109%,4%)
	PR_EMP_MASTER::HIREDAY	= "19"+MID$(LINE$,120%,2%)+MID$(LINE$,116%,4%)
	PR_EMP_MASTER::TERMDAY	= "19"+MID$(LINE$,126%,2%)+MID$(LINE$,122%,4%) &
				IF MID$(LINE$,122%,6%)<>""
	PR_EMP_MASTER::SEX	= MID$(LINE$,115%,1%)
	PR_EMP_MASTER::RACE	= MID$(LINE$,146%,2%)
	PR_EMP_MASTER::USCIT	= ""
	PR_EMP_MASTER::WRKPERMIT= ""
	PR_EMP_MASTER::HOMCNTRY	= ""
	PR_EMP_MASTER::ACTIVE_FLAG = MID$(LINE$,135%,1%)
	PR_EMP_MASTER::RATE_TYPE= ""
	PR_EMP_MASTER::RATE_CDE	= ""
	PR_EMP_MASTER::WC	= ""

	UPDATE #PR_EMP_MASTER.CH%

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, PR_EMP_MASTER::EMPNUM, 19%, 13%)

	GOTO 2150

2160	SMG_STATUS% = SMG$ERASE_DISPLAY(DISPLAY_FILE%)
	SMG_STATUS% = SMG$UNPASTE_VIRTUAL_DISPLAY(DISPLAY_FILE%, &
		SCOPE::SMG_PBID)

2165	CLOSE #PRMASTER.CH%
	!CLOSE #PR_EMP_MASTER.CH%

2250	CALL ASSG_FREECHANNEL(PRMASTER.CH%)

	YYYY$ = "1991"
	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_ERNDED.CRE"

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.CRE"

	CALL ASSG_CHANNEL(PRYTD.CH%,STAT%)

	OPEN "PRYTD."+EXT$ FOR INPUT AS FILE PRYTD.CH%, &
		ORGANIZATION SEQUENTIAL

	COUNTER% = 0%
	CONF$ = "Y"

	!
	! Paste the second data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY( DISPLAY_FILE%, SCOPE::SMG_PBID, 5%, 1% )
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	SMG_STATUS% = SMG$ERASE_DISPLAY(DISPLAY_FILE%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "PRYTD       ", 18%, 13%)

 YTD:
	LINPUT #PRYTD.CH%, LINE$
	COUNTER% = COUNTER% + 1%

2255	GET #PR_EMP_MASTER.CH%, KEY#3% EQ FORMAT$(COUNTER%, "<0>###")

	ADD_DOLL = 0.0
	FOR I% = 0% TO 7%
		ADD_DOLL = ADD_DOLL + VAL(MID$(LINE$,82%+9%*I%,9%)) &
			IF VAL(MID$(LINE$,82%+9%*I%,9%))>0.0
	NEXT I%

	ADD_DOLL = ADD_DOLL + VAL(MID$(LINE$,28%,9%))+ &
			VAL(MID$(LINE$,37%,9%)) + VAL(MID$(LINE$,46%,9%))
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, PR_EMP_MASTER::EMPNUM, 9%, 40%)

2260	GET #PR_REG_TAXES.CH%, KEY#0% EQ PR_EMP_MASTER::EMPNUM + "FI"
	PR_REG_TAXES::TAX(2%)	= -VAL(MID$(LINE$,73%,9%))
	PR_REG_TAXES::TAXABLE(2%) = ADD_DOLL
	PR_REG_TAXES::REPORTABLE(2%) = ADD_DOLL

	UPDATE # PR_REG_TAXES.CH%
	GOTO 2265

 FICA:
	PR_REG_TAXES::EMPNUM	= PR_EMP_MASTER::EMPNUM
	PR_REG_TAXES::TTYPE	= "FI"
	PR_REG_TAXES::CODE	= ""

	FOR I% = 0% TO 3%
		PR_REG_TAXES::TAX(I%)		= 0.0
		PR_REG_TAXES::TAXABLE(I%)	= 0.0
		PR_REG_TAXES::REPORTABLE(I%)	= 0.0
		PR_REG_TAXES::WKWRK(I%)		= 0%
	NEXT I%

	PR_REG_TAXES::TAX(2%)	= -VAL(MID$(LINE$,73%,9%))
	PR_REG_TAXES::TAXABLE(2%) = ADD_DOLL
	PR_REG_TAXES::REPORTABLE(2%) = ADD_DOLL
	PR_REG_TAXES::WKWRK(I%)	= 0% FOR I% = 0% TO 3%

	PR_REG_TAXES::UPDATE_COUNTER = 1%
	PUT # PR_REG_TAXES.CH%

2265	GET #PR_REG_TAXES.CH%, KEY#0% EQ PR_EMP_MASTER::EMPNUM + "FW"
	PR_REG_TAXES::TAX(2%)	= -VAL(MID$(LINE$,55%,9%))
	PR_REG_TAXES::TAXABLE(2%) = ADD_DOLL
	PR_REG_TAXES::REPORTABLE(2%) = ADD_DOLL

	UPDATE # PR_REG_TAXES.CH%
	GOTO 2270

 FW:
	PR_REG_TAXES::EMPNUM	= PR_EMP_MASTER::EMPNUM
	PR_REG_TAXES::TTYPE	= "FW"
	PR_REG_TAXES::CODE	= ""
		FOR I% = 0% TO 3%
			PR_REG_TAXES::TAX(I%)		= 0.0
			PR_REG_TAXES::TAXABLE(I%)	= 0.0
			PR_REG_TAXES::REPORTABLE(I%)	= 0.0
			PR_REG_TAXES::WKWRK(I%)		= 0%
		NEXT I%

	PR_REG_TAXES::TAX(2%)	= -VAL(MID$(LINE$,55%,9%))
	PR_REG_TAXES::TAXABLE(2%) = ADD_DOLL
	PR_REG_TAXES::REPORTABLE(2%) = ADD_DOLL
	PR_REG_TAXES::WKWRK(I%)	= 0% FOR I% = 0% TO 3%

	PR_REG_TAXES::UPDATE_COUNTER = 1%
	PUT # PR_REG_TAXES.CH%

2270	GET #PR_REG_TAXES.CH%, KEY#0% EQ PR_EMP_MASTER::EMPNUM + "SWID"
	PR_REG_TAXES::TAX(2%)	= -VAL(MID$(LINE$,64%,9%))
	PR_REG_TAXES::TAXABLE(2%) = ADD_DOLL
	PR_REG_TAXES::REPORTABLE(2%) = ADD_DOLL

	UPDATE # PR_REG_TAXES.CH%
	GOTO 2275

 SW:
	PR_REG_TAXES::EMPNUM	= PR_EMP_MASTER::EMPNUM
	PR_REG_TAXES::TTYPE	= "SW"
	PR_REG_TAXES::CODE	= "ID"
		FOR I% = 0% TO 3%
			PR_REG_TAXES::TAX(I%)		= 0.0
			PR_REG_TAXES::TAXABLE(I%)	= 0.0
			PR_REG_TAXES::REPORTABLE(I%)	= 0.0
			PR_REG_TAXES::WKWRK(I%)		= 0%
		NEXT I%

	PR_REG_TAXES::TAX(2%)	= -VAL(MID$(LINE$,64%,9%))
	PR_REG_TAXES::TAXABLE(2%) = ADD_DOLL
	PR_REG_TAXES::REPORTABLE(2%) = ADD_DOLL
	PR_REG_TAXES::WKWRK(I%)	= 0% FOR I% = 0% TO 3%

	PR_REG_TAXES::UPDATE_COUNTER = 1%
	PUT # PR_REG_TAXES.CH%

2275	GET #PR_REG_ERNDED.CH%, KEY#0% EQ PR_EMP_MASTER::EMPNUM + "PRT"
	PR_REG_ERNDED::QTR_DOLL(2%)	= VAL(MID$(LINE$,28%,9%)) + &
		VAL(MID$(LINE$,37%,9%)) + VAL(MID$(LINE$,46%,9%))
	PR_REG_ERNDED::REG_HRS(2%)	= VAL(MID$(LINE$,1%,9%))
	PR_REG_ERNDED::PRE_HRS(2%)	= VAL(MID$(LINE$,10%,9%)) + &
		VAL(MID$(LINE$,19%,9%))

	UPDATE # PR_REG_ERNDED.CH%
	GOTO ConfirmYTD

 PTX:
	PR_REG_ERNDED::EMPNUM	= PR_EMP_MASTER::EMPNUM
	PR_REG_ERNDED::ETYPE	= "P"
	PR_REG_ERNDED::CODE	= "RT"
		FOR I% = 0% TO 3%
			PR_REG_ERNDED::QTR_DOLL(I%)		= 0.0
			PR_REG_ERNDED::REG_HRS(I%)		= 0.0
			PR_REG_ERNDED::PRE_HRS(I%)		= 0.0
			PR_REG_ERNDED::UNITS(I%)		= 0.0
		NEXT I%

	PR_REG_ERNDED::QTR_DOLL(2%)	= VAL(MID$(LINE$,28%,9%)) + &
		VAL(MID$(LINE$,37%,9%)) + VAL(MID$(LINE$,46%,9%))
	PR_REG_ERNDED::REG_HRS(2%)	= VAL(MID$(LINE$,1%,9%))
	PR_REG_ERNDED::PRE_HRS(2%)	= VAL(MID$(LINE$,10%,9%)) + &
		VAL(MID$(LINE$,19%,9%))

	PR_REG_ERNDED::UPDATE_COUNTER = 1%
	PUT # PR_REG_ERNDED.CH%

 ConfirmYTD:

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Tot Reg H "+MID$(LINE$,1%,9%), 1%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Tot Over  "+MID$(LINE$,10%,9%), 2%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Tot Doubl "+MID$(LINE$,19%,9%), 3%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Reg Pay   "+MID$(LINE$,28%,9%), 4%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Over Pay  "+MID$(LINE$,37%,9%), 5%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Doubl Pay "+MID$(LINE$,46%,9%), 6%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Fed Tax   "+MID$(LINE$,55%,9%), 7%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "State Tax "+MID$(LINE$,64%,9%), 8%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "FICA Tax  "+MID$(LINE$,73%,9%), 9%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Ded 1     "+MID$(LINE$,82%,9%), 10%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Ded 2     "+MID$(LINE$,91%,9%), 11%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Ded 3     "+MID$(LINE$,100%,9%), 12%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Ded 4     "+MID$(LINE$,109%,9%), 13%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Ded 5     "+MID$(LINE$,118%,9%), 14%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Ded 6     "+MID$(LINE$,127%,9%), 15%, 3%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Ded 7     "+MID$(LINE$,136%,9%), 3%, 30%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Ded 8     "+MID$(LINE$,145%,9%), 4%, 30%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "# State E "+MID$(LINE$,154%,6%), 5%, 30%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_FILE%, "Amt St Ex "+MID$(LINE$,160%,9%), 6%, 30%)

	CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_FILE%, &
		"", "Display next Record", "Y", 16%, "'", "Y"), -1%) &
		IF CONF$<>"N"

	!IF NOT BEFORE_%
	!THEN
	!END IF

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT
	!
	! Control/C, Uparrow,
	!
	CASE 3%, SMG$K_TRM_UP, SMG$K_TRM_DOWN
		GOTO ConfirmYTD	! (Ignored)

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, SMG$K_TRM_DO, SMG$K_TRM_CR, &
		SMG$K_TRM_SELECT

	!
	! Bad Keys
	!
	CASE ELSE
		GOTO ConfirmYTD

	END SELECT

	IF CONF$ <> "Y"
	THEN
		GOTO YTD
	END IF

	GOTO YTD

2280	SMG_STATUS% = SMG$ERASE_DISPLAY(DISPLAY_FILE%)
	SMG_STATUS% = SMG$UNPASTE_VIRTUAL_DISPLAY(DISPLAY_FILE%, &
		SCOPE::SMG_PBID)
	CLOSE #PRYTD.CH%
	CLOSE #PR_EMP_MASTER.CH%

	CALL ASSG_FREECHANNEL(PRYTD.CH%)
	CALL ASSG_FREECHANNEL(PR_EMP_MASTER.CH%)

	GOTO Password

 ExitProgram:
15000	!*******************************************************************
	! Exit program
	!*******************************************************************

	IF CONF$ = "Y"
	THEN
		CALL ENTR_3MESSAGE(SCOPE,"Conversion Process Complete", 0%)
	ELSE
		CALL ENTR_3MESSAGE(SCOPE,"Aborting Conversion Process", 0%)
	END IF

	CALL SUBR_3EXITPROGRAM(SCOPE, "RUN CMC$ROOT:[PR]PR_MAST_EMPLOYEE", "")

	%PAGE

 HelpError:
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	SELECT ERL

	CASE 1200%
		RESUME 2000 IF ERR = 5%
		FILENAME$ = "PR_EMP_MASTER"

	CASE 2000%
		FILENAME$ = "PR_EMP_MASTER"

	CASE 2010%
		FILENAME$ = "PRISAM"

	CASE 2020%
		RESUME 2120 IF ERR = 11%
		FILENAME$ = "PRISAM"

	CASE 2120%
		FILENAME$ = "PRMASTER"

	CASE 2130%
		FILENAME$ = "PRMASTER"

	CASE 2140%
		FILENAME$ = "PR_TAX_PROFILE_F"

	CASE 2150%
		RESUME 2160 IF ERR = 11%
		FILENAME$ = "PRMASTER"

	CASE 2155%
		RESUME 2150 IF ERR = 155%
		FILENAME$ = "PR_EMP_MASTER"

	CASE 2250%
		RESUME 2280 IF ERR = 11%
		FILENAME$ = "PRYTD"

	CASE 2255%
		RESUME YTD IF ERR = 155%
		FILENAME$ = "PR_EMP_MASTER"

	CASE 2260%
		RESUME FICA IF ERR = 155%
		FILENAME$ = "PR_TAX_REG"

	CASE 2265%
		RESUME FW IF ERR = 155%
		FILENAME$ = "PR_TAX_REG"

	CASE 2270%
		RESUME SW IF ERR = 155%
		FILENAME$ = "PR_TAX_REG"

	CASE 2275%
		RESUME PTX IF ERR = 155%
		FILENAME$ = "PR_REG_ERNDED"

	END SELECT

	RESUME HelpError

19999	END

20000	FUNCTION STRING CONV_ACCT (STRING ASCII_ACCT)
	CONV_ACCT = LEFT$(ASCII_ACCT,4%) + "." + RIGHT$(ASCII_ACCT,5%)
29999	END FUNCTION
