1	%TITLE "Payroll Direct Deposit Routine"
	%SBTTL "PR_SPEC_DIRECTDEPOSIT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1998 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! ID:PR004
	!
	! Abstract:HELP
	!	.p
	!	Generate a Direct Deposit Transaction File.
	!
	! Index:
	!	.x Direct Deposit>Generate
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_SPEC_DIRECTDEPOSIT/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_SPEC_DIRECTDEPOSIT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_SPEC_DIRECTDEPOSIT.OBJ;*
	!
	! Author:
	!
	!	06/01/98 - Kevin Handy
	!		Based upon PR_RPRT_TRN_DED_YR
	!
	! Modification history:
	!
	!	08/05/98 - Kevin Handy
	!		Fix several bugs in record creation
	!		Fix has total calculation.
	!
	!	08/06/98 - Kevin Handy
	!		Another fix to the hash total
	!
	!	08/07/98 - Kevin Handy
	!		Use %INCLUDE instead of '%INCLUDE %FROM %CDD'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/08/98 - Kevin Handy
	!		Match error trap for 305 with correct line number
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	02/22/99 - Kevin Handy
	!		Lost (Page after deduction) so that I could put
	!		in (Name of bank)
	!
	!	03/07/99 - Kevin Handy
	!		Use 'WHEN ERROR IN'
	!
	!	03/09/99 - Kevin Handy
	!		Add PR_DEPOSIT_HEAD.TXT file.
	!
	!	03/09/99 - Kevin Handy
	!		Fix bug not pulling DFI account properly.
	!
	!	03/23/99 - Kevin Handy
	!		Create a record for 'F' records, even if it
	!		is zero.
	!
	!	03/25/99 - Kevin Handy
	!		Transfer type '22' as '23' if the amount is zero.
	!		Some strange bank rule
	!
	!	04/21/99 - Kevin Handy
	!		Fix unsolicited input
	!
	!	05/05/99 - Kevin Handy
	!		Modified the bank code so that the user must
	!		enter a '1'...'9' on the front of the code in
	!		the report settings screen.
	!
	!	07/02/99 - Kevin Handy
	!		Grab the actual federal ID number for record 5.
	!
	!	07/12/99 - Kevin Handy
	!		Modifications so that ONAME and CNAME can be
	!		different (one is now main location, the other is
	!		default location, used to be both were main location)
	!
	!	10/16/2000 - Kevin Handy
	!		Add in code to calculate BOA code when file
	!		defining starting position exists. Table is
	!		currently hardcoded, but may someday need to
	!		be built into the text file along with the
	!		FIXED NUMBER.
	!
	!	10/26/2000 - Kevin Handy
	!		Add PRENOTE code to BOA calculations.
	!
	!	07/23/2004 - Kevin Handy
	!		RSET bank codes.
	!
	!	10/20/2004 - Kevin Handy
	!		Use new DIRECT field in profile.
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF)	PR_ERNDED_DEF_CDD	PR_ERNDED_DEF

	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.HB"
	MAP (PR_EMP_STD_ERNDED)	PR_EMP_STD_ERNDED_CDD	PR_EMP_STD_ERNDED

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP (UTL_PROFILE) UTL_PROFILE_CDD UTL_PROFILE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION) UTL_LOCATION_CDD UTL_LOCATION

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_F_CDD	PR_TAX_PROFILE_F

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_S.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_S_CDD	PR_TAX_PROFILE_S

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_C.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_C_CDD	PR_TAX_PROFILE_C

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_E.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_E_CDD	PR_TAX_PROFILE_E

	%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_D.HB"
	MAP (PR_TAX_PROFILE_F)	PR_TAX_PROFILE_D_CDD	PR_TAX_PROFILE_D

	RECORD PR_TEMP_CDD
		STRING	SORTS = 4%
		STRING	DTYPE = 1%
		STRING	CODE = 2%
		STRING	EMPNUM = 10%
		STRING	EMPNAME = 30%
		STRING	PR_END_DATE = 8%
		REAL	AMOUNT
		WORD	UPDATE_FLAG
	END RECORD

	MAP (PR_TEMP) PR_TEMP_CDD PR_TEMP

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! ACH File Header Format
	!
	RECORD ACH_FILEHEADER
		STRING RTYPE		= 1%	! Record Type Code (1)
		STRING PRIORITY		= 2%	! Priority code (01)
		STRING DESTINATION	= 10%	! Immediate Destination
		STRING ORIGIN		= 10%	! Immediate Origin
		STRING TRANDATE		= 6%	! Transmission Date
		STRING TRANTIME		= 4%	! Transmission Time
		STRING MODIFIER		= 1%	! File ID Modifier
		STRING RSIZE		= 3%	! Record Size (094)
		STRING BFACTOR		= 2%	! Blocking Factor
		STRING FCODE		= 1%	! Format Code
		STRING DNAME		= 23%	! Destination
		STRING ONAME		= 23%	! Origin
		STRING REFERENCE	= 8%	! Reference Code
	END RECORD

	!
	! ACH Company/Batch header
	!
	RECORD ACH_COMPANY
		STRING RTYPE		= 1%	! Record Type Code (5)
		STRING SERVICE		= 3%	! Service Class Code
		STRING CNAME		= 16%	! Company Name
		STRING CDATA		= 20%	! Company Discretionary Data
		STRING CIDENT		= 10%	! Company Identification
		STRING ENTCLASS		= 3%	! Standard Entry Class
		STRING CDDESC		= 10%	! Company Entry DESCRIPTION
		STRING CDDATE		= 6%	! Company Descriptive Date
		STRING EEDATE		= 6%	! Effective Entry Date
		STRING BLANK1		= 3%	! Blank
		STRING OSTATUS		= 1%	! Origin Status Code
		STRING ORIGIN		= 8%	! Originating DFI Identification
		STRING BATCH		= 7%	! Batch Number
	END RECORD

	!
	! ACH Entry Detail Record
	!
	RECORD ACH_ENTRYDETAIL
		STRING RTYPE		= 1%	! Record Type Code (6)
		STRING TRANCODE		= 2%	! Transaction Code
		STRING RECDFI		= 8%	! Receiving DFI Code
		STRING TRANSIT		= 1%	! Transit Routing Check Digit
		STRING DFI		= 17%	! DFI Account Number
		STRING AMOUNT		= 10%	! Amount (RightJ, 0 Fill)
		STRING IIN		= 15%	! Individual Ident No.
		STRING INAME		= 22%	! Individual Name
		STRING DDATA		= 2%	! Discretionary Data
		STRING ADDENDA		= 1%	! Addenda Record (0)
		STRING TRACENO		= 15%	! Trace Numbers
	END RECORD

	!
	! ACH Company/Batch Control Record
	!
	RECORD ACH_COMPANYBATCH
		STRING RTYPE		= 1%	! Record Type Code (8)
		STRING SERVICE		= 3%	! Service Class Code
		STRING ADDENDACNT	= 6%	! Entry/Addenda Count
		STRING ENTRYHASH	= 10%	! Entry Hash
		STRING TOTALDEBIT	= 12%	! Total Debit Dollars
		STRING TOTALCREDIT	= 12%	! Total Credit Dollars
		STRING CID		= 10%	! Company ID
		STRING BLANK1		= 19%	! Blank
		STRING FEDERAL		= 6%	! Reserved for Federal Reserve
		STRING ODFI		= 8%	! ODFI Identification
		STRING BATCH		= 7%	! Batch Number
	END RECORD

	!
	! ACH File Control Record
	!
	RECORD ACH_FILETOTAL
		STRING RTYPE		= 1%	! Record Type Code (9)
		STRING BATCHCOUNT	= 6%	! Number of Batches
		STRING BLOCKCOUNT	= 6%	! Block Count
		STRING ENTRYCOUNT	= 8%	! Entry/Addenda Count
		STRING ENTRYHASH	= 10%	! Entry Hash
		STRING TOTALDEBIT	= 12%	! Total Debit Dollar
		STRING TOTALCREDIT	= 12%	! Total Credit Dollar
		STRING BLANK1		= 39%	! Blank
	END RECORD

	!
	! Bundle together all the direct deposit records
	!
	MAP (DIRECTDEPOSIT) DIRECTDEPOSIT$ = 94%
	MAP (DIRECTDEPOSIT) ACH_FILEHEADER ACH_FILEHEADER
	MAP (DIRECTDEPOSIT) ACH_COMPANY ACH_COMPANY
	MAP (DIRECTDEPOSIT) ACH_ENTRYDETAIL ACH_ENTRYDETAIL
	MAP (DIRECTDEPOSIT) ACH_COMPANYBATCH ACH_COMPANYBATCH
	MAP (DIRECTDEPOSIT) ACH_FILETOTAL ACH_FILETOTAL

	!
	! External functions
	!
	EXTERNAL LONG	OUTP_XUNSOL ! (It's really an AST routine)

	!
	! Declare variables and constants
	!
	DECLARE	STRING	LYT_LINE

	DIM DATA_FILE$(1000%)

	%PAGE

	!
	! Find a channel for the TEMP file
	!
	CALL ASSG_CHANNEL(PR_TEMP.CH%, STAT%)

 Init:
	TAX_TYPE_TABLE$ = "!FI!FH!FW!SW!SX!CW!DW!EW!SU!SI!"

	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	FROM_BATCH_NO$ = DATE_STOREDATE(FROM_BATCH_NO$)

	!++
	! Abstract:FLD01
	!	^*(01)Start Payroll Date\*
	!	.p
	!	The ^*Start Payroll Date\* field is to be entered with the
	!	starting date for which the report is to print.
	!	.p
	!	This field requires an entry. The format for entry is
	!	MMDDYYYY or MMDDYY.
	!
	! Index:
	!	.x Start Payroll Date>Direct Deposit
	!	.x Direct Deposit>Start Payroll Date
	!
	! Datatype:DATE
	! Size:8
	! Required:Y
	!--

	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$)

	!++
	! Abstract:FLD02
	!	^*(02)Ending Payroll Date\*
	!	.p
	!	The ^*Ending Payroll Date\* field is to be entered with the
	!	ending date for which the report is to print.
	!	.p
	!	This field requires an entry. The format for entry is
	!	MMDDYYYY or MMDDYY.
	!
	! Index:
	!	.x Ending Payroll Date>Direct Deposit
	!	.x Direct Deposit>Ending Payroll Date
	!
	! Datatype:DATE
	! Size:8
	! Required:Y
	!--

	BANK_NAME$ = TRM$(UTL_REPORTX::OPTDEF(2%))

	!++
	! Abstract:FLD03
	!	^*(03) Page After Each Deduction\*
	!	.p
	!	The ^*Page After Each Deduction\* field causes a page
	!	break to occur after printing each deduction.
	!	.p
	!	To cause the page break to occur, a ^*Y\* must be entered, Otherwise enter
	!	a ^*N\*.
	!
	! Index:
	!	.x Page After>Direct Deposit
	!	.x Direct Deposit>Page After
	!
	! Datatype:TEXT
	! Size:1
	! Valid Input: Y,N,y,n
	!--

	WILDCARD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.p
	!	Used to select (using wildcards) the deduction types
	!	to appear on the report.
	!
	! Index:
	!	.x Wildcard>Direct Deposit
	!	.x Direct Deposit>Wildcard
	!
	! Datatype:TEXT
	! Size:15
	!--

	DIRECTNAME$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(05) Output File Name\*
	!	.p
	!	Name of the file to store the direct deposit
	!	transmittal into.
	!
	! Index:
	!	.x Wildcard>Direct Deposit
	!	.x Direct Deposit>Wildcard
	!
	! Datatype:TEXT
	! Size:15
	!--

	FROMLOC$ = EDIT$(UTL_REPORTX::OPTDEF(5%), 132%)

	!++
	! Abstract:FLD06
	!	^*(06) From Item\*
	!
	! Index:
	!
	! Datatype:TEXT
	! Size:15
	!--

	TOLOC$ = EDIT$(UTL_REPORTX::OPTDEF(6%), 132%)

	!++
	! Abstract:FLD07
	!	^*(07) To Item\*
	!
	! Index:
	!
	! Datatype:TEXT
	! Size:15
	!--

	SORTBY$ = "DC"
	BATCH$ = EDIT$(UTL_REPORTX::OPTDEF(7%), 132%)

	!++
	! Abstract:FLD08
	!	^*(08) Batch Number\*
	!	.LM +5
	!	.B
	!	A seven digit batch number to uniquely identify
	!	this set of transactions.
	!
	! Index:
	!
	!--

	BANKCODE$ = EDIT$(UTL_REPORTX::OPTDEF(8%), 132%)

	!++
	! Abstract:FLD09
	!	^*(09) Bank Code\*
	!	.B
	!	The code for your payroll account's bank (should be in
	!	the format 1ttttaaaac)
	!
	! Index:
	!
	!--

	FULLACCTCODE$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)
	ACCTCODE$ = RIGHT(FULLACCTCODE$, 2%)

	!++
	! Abstract:FLD10
	!	^*(10) Account Code\*
	!	.B
	!	The code for your payroll account at the
	!	specified bank. ('1' followed by Tax ID Number?)
	!	.b
	!	The account code includes a '1' for most accounts, but
	!	if you have more than one bank account it may change
	!	accordingly.
	!
	! Index:
	!
	!--

	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

300	!
	! Employee Master File
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.OPN"
	USE
		CONTINUE 17070 IF ERR = 155%
		FILENAME$ = "PR_EMP_MASTER"
		CONTINUE HelpError
	END WHEN

302	!
	! Get progile information
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.OPN"

		GET #UTL_PROFILE.CH%, REGARDLESS

		CLOSE #UTL_PROFILE.CH%
	USE
		FILENAME$ = "UTL_PROFILE"
		CONTINUE HelpError
	END WHEN

305	!
	! Open LOCATION file, Set up company name as the default
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	ONAME$ = ""
	CNAME$ = ""

	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, &
			KEY #0% EQ UTL_PROFILE::MAINLOCATION, &
			REGARDLESS

		ONAME$ = UTL_LOCATION::LOCNAME
	USE
		ONAME$ = ""
	END WHEN

	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, &
			KEY #0% EQ UTL_PROFILE::DEFLOCATION, &
			REGARDLESS

		CNAME$ = UTL_LOCATION::LOCNAME
	USE
		CNAME$ = ONAME$
	END WHEN

330	!
	! Ernded Definition
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.OPN"
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

340	!
	! Employee STD Ernded definition
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_STD_ERNDED.OPN"
	USE
		CONTINUE 350 IF ERR = 5%
		FILENAME$ = "PR_EMP_STD_ERNDED"
		CONTINUE HelpError
	END WHEN

350	!
	! Open up direct deposit output file
	!
	CALL ASSG_CHANNEL(DIRECT.CH%, STAT%)

	WHEN ERROR IN
		OPEN DIRECTNAME$ FOR OUTPUT AS FILE DIRECT.CH%, &
			RECORDSIZE 100%
	USE
		FILENAME$ = "DIRECTDEPOSIT.TXT"
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
				PR_TEMP::SORTS, &
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

365	!
	! Open Profile
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PR.OPEN]PR_TAX_PROFILE_F.OPN"
		GET #PR_TAX_PROFILE.CH%, KEY #0% EQ "F", REGARDLESS
		FEDERAL_ID$ = EDIT$(PR_TAX_PROFILE_F::DIRECT, -1%)
		WHILE (INSTR(1%, FEDERAL_ID$, "-"))
			I% = INSTR(1%, FEDERAL_ID$, "-")
			FEDERAL_ID$ = LEFT(FEDERAL_ID$, I% - 1%) + &
				RIGHT(FEDERAL_ID$, I% + 1%)
		NEXT
	USE
		FILENAME$ = "PR_TAX_PRODFILE"
		CONTINUE HelpError
	END WHEN

370	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	CALL PR_FIND_DETAILFILE(FROM_BATCH_NO$, &
		TO_BATCH_NO$, &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	DATA_FILE% = VAL%(DATA_FILE$(0%))

	%PAGE

	!*******************************************************************
	! Loop through all folders
	!*******************************************************************

	FOR PR_LOOP% = 1% TO DATA_FILE%

		BATCH_NO$ = DATA_FILE$(PR_LOOP%)
		YYYY$ = LEFT(BATCH_NO$, 4%)
		CALL ENTR_3MESSAGE(SCOPE, "Processing: " + BATCH_NO$, 1%)

380		!
		! Open Pay folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.OPN"
		USE
			CONTINUE 390 IF ERR = 5%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

		PR_TMP_DED.CH% = PR_TRN_DED.CH%
		USE_HISTORY% = 0%

		GOTO 400

390		!
		! Open pay history folder if journal not there
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.OPN"
		USE
			CONTINUE 430 IF ERR = 5%
			FILENAME$ = "PR_HIS_DED"
			CONTINUE HelpError
		END WHEN

		PR_TMP_DED.CH% = PR_HIS_DED.CH%
		USE_HISTORY% = -1%

400		!
		! Create work file
		!
		WHEN ERROR IN
			RESET #PR_TMP_DED.CH%, KEY #0%
		USE
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

 !		CALL ENTR_3MESSAGE(SCOPE, "Creating work file by Deduction type.  Working. . .", 1%)

		!
		! Set up to trap interrupt
		!
		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

		RRR_FLAG% = 0%

410		WHEN ERROR IN
			GET #PR_TMP_DED.CH%, REGARDLESS
		USE
			CONTINUE 430 IF ERR = 11%
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
			UTL_REPORTX::STAT = -1%
			GOTO ExitProgram

		END SELECT

		RRR_FLAG% = 0%

420		!
		! Test to see if deduction and not tax
		!
		IF (PR_TRN_DED::DTYPE = "F")
		THEN
			IF (WILDCARD$ = "") OR &
				(COMP_STRING(PR_TRN_DED::CODE, WILDCARD$) <> 0%)
			THEN
				GOSUB GetMaster

				!
				! Fill in Sortby field
				!
				SELECT SORTBY$
				CASE "LO"
					PR_TEMP::SORTS	= PR_EMP_MASTER::LOCATION
				CASE ELSE
					PR_TEMP::SORTS	= PR_TRN_DED::CODE
				END SELECT

				IF (PR_TEMP::SORTS >= FROMLOC$) AND &
					((TOLOC$ = "") OR (TRM$(PR_TEMP::SORTS) <= TOLOC$))
				THEN
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
			END IF
		END IF

		GOTO 410

430		CLOSE PR_TMP_DED.CH%

	NEXT PR_LOOP%

	%PAGE

	!*******************************************************************
	! Now that we have generated the temp file, print it out.
	!*******************************************************************

 ReportTitle:
	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Deduction Report"
	TITLE$(2%) = "For Payroll Folders Dated from: " &
		+ PRNT_DATE(FROM_BATCH_NO$, 8%) + &
		"  to: " + PRNT_DATE(TO_BATCH_NO$, 8%)
	TITLE$(3%) = "Sorted by Deduction"
	TITLE$(4%) = ""

	!
	! Column headings
	!
	TITLE$(5%) = "Typ Code CodeDescription               EmpNum     " + &
		"EmpName                  Date         Current      Account"
	TITLE$(6%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "$EType:001,$Code:007,$CodeDescr:036,$EmpNum:049," + &
		"$EmpName:072,DPrDate:081,VCurrent:095,$Account:120"

	%PAGE

16000	!*******************************************************************
	! Try to load in any header information
	!*******************************************************************

	CALL ASSG_CHANNEL(PR_HEAD.CH%, STAT%)
	CALL READ_DEVICE('PR_DIRECT', PR_DIRECT.DEV$, STAT%)

	WHEN ERROR IN
		OPEN PR_DIRECT.DEV$ + "PR_DIRECT_HEAD.TXT" &
			FOR INPUT AS FILE PR_HEAD.CH%, &
			ACCESS READ, &
			ALLOW READ
	USE
		CONTINUE 16090
	END WHEN

16050	!
	! Copy over the text files
	!
	WHEN ERROR IN
		LINPUT #PR_HEAD.CH%, TEXT$
		PRINT #DIRECT.CH%, TEXT$
	USE
		CONTINUE 16060
	END WHEN

	GOTO 16050

16060	!
	! Close out everything
	!
	CLOSE PR_HEAD.CH%

16090	!
	! Clean up
	!
	CALL ASSG_FREECHANNEL(PR_HEAD.CH%)

	%PAGE

16100	!*******************************************************************
	! Try to load in any header information
	! BOA Test code number
	!*******************************************************************

	FIXED_NUMBER% = 0%

	CALL ASSG_CHANNEL(PR_HEAD.CH%, STAT%)
	CALL READ_DEVICE('PR_DIRECT', PR_DIRECT.DEV$, STAT%)

	WHEN ERROR IN
		OPEN PR_DIRECT.DEV$ + "PR_BOA_FIXED.TXT" &
			FOR INPUT AS FILE PR_HEAD.CH%, &
			ACCESS READ, &
			ALLOW READ
	USE
		CONTINUE 16190
	END WHEN

16150	!
	! Copy over the text files
	!
	WHEN ERROR IN
		LINPUT #PR_HEAD.CH%, FIXED_NUMBER$
		FIXED_NUMBER% = VAL%(FIXED_NUMBER$)
	USE
		FIXED_NUMBER% = 0%
		CONTINUE 16160
	END WHEN

16160	!
	! Close out everything
	!
	CLOSE PR_HEAD.CH%

16190	!
	! Clean up
	!
	CALL ASSG_FREECHANNEL(PR_HEAD.CH%)

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	TEST_EMPNUM$ = ""
	ADDCOUNT% = 0%
	HASHCOUNT = 0%

	WHEN ERROR IN
		RESET #PR_TEMP.CH%, KEY #0%
	USE
		FILENAME$ = "PR_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Output the file header
	!
	DIRECTDEPOSIT$ = ""
	ACH_FILEHEADER::RTYPE		= "1"
	ACH_FILEHEADER::PRIORITY	= "01"
	RSET ACH_FILEHEADER::DESTINATION	= " " + BANKCODE$
	RSET ACH_FILEHEADER::ORIGIN		= "1" + ACCTCODE$
	ACH_FILEHEADER::TRANDATE	= RIGHT(DATE_TODAY, 3%)
	ACH_FILEHEADER::TRANTIME	= TIME_NOW
	ACH_FILEHEADER::MODIFIER	= "A"
	ACH_FILEHEADER::RSIZE		= "094"
	ACH_FILEHEADER::BFACTOR		= "10"
	ACH_FILEHEADER::FCODE		= "1"
	ACH_FILEHEADER::DNAME		= BANK_NAME$
	ACH_FILEHEADER::ONAME		= ONAME$
	ACH_FILEHEADER::REFERENCE	= ""

	PRINT #DIRECT.CH%, DIRECTDEPOSIT$

	!
	! Output Company/Batch Header
	!
	DIRECTDEPOSIT$ = ""
	ACH_COMPANY::RTYPE		= "5"
	ACH_COMPANY::SERVICE		= "200"
	ACH_COMPANY::CNAME		= CNAME$
	ACH_COMPANY::CDATA		= ""
	ACH_COMPANY::CIDENT		= LEFT(FULLACCTCODE$, 1%) + &
		FEDERAL_ID$
	ACH_COMPANY::ENTCLASS		= "PPD"
	ACH_COMPANY::CDDESC		= "DIRCT DPST"
	ACH_COMPANY::CDDATE		= RIGHT(DATE_TODAY, 3%)
	ACH_COMPANY::EEDATE		= RIGHT(DATE_TODAY, 3%)
	ACH_COMPANY::OSTATUS		= "1"
	ACH_COMPANY::ORIGIN		= MID(BANKCODE$, 1%, 8%)
	ACH_COMPANY::BATCH		= BATCH$

	PRINT #DIRECT.CH%, DIRECTDEPOSIT$

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

	IF (SORTBY$ = "LO") AND (THISLOC$ <> PR_TEMP::SORTS)
	THEN
		GOSUB EmployeeTotal IF TEST_EMPNUM$ <> ""
		GOSUB CodeTotal
		GOSUB LocationTotal
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 3000%)
	END IF

	GOTO 17060 IF TEST_CODE$ = PR_TEMP::DTYPE + PR_TEMP::CODE

	IF TEST_CODE$ <> ""
	THEN
		GOSUB EmployeeTotal IF TEST_EMPNUM$ <> ""

		GOTO ExitProgram IF UTL_REPORTX::STAT

		GOSUB CodeTotal

		GOTO ExitProgram IF UTL_REPORTX::STAT

	END IF

17040	PR_ERNDED_DEF::ETYPE	= PR_TEMP::DTYPE
	PR_ERNDED_DEF::CODE	= PR_TEMP::CODE
	PR_ERNDED_DEF::DESCR	= "?????????????????????????????????????"

	WHEN ERROR IN
		GET #PR_ERNDED_DEF.CH%, &
			KEY #0% EQ PR_TEMP::DTYPE + PR_TEMP::CODE, &
			REGARDLESS
		TEST_EMPNUM$ = ""
	USE
		CONTINUE 17050 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PR_ERNDED_DEF"
		CONTINUE HelpError
	END WHEN

17050	TEXT$ = PR_ERNDED_DEF::ETYPE + "    " + &
		PR_ERNDED_DEF::CODE + "  " + &
		LEFT(PR_ERNDED_DEF::DESCR + SPACE$(27%), 27%) + "  "

17060	GOTO 17100 IF TEST_EMPNUM$ = PR_TEMP::EMPNUM + PR_TEMP::PR_END_DATE

17070	!
	! Switch Employees
	!
	IF EMP_COUNT% <> 0%
	THEN
		GOSUB EmployeeTotal
		GOTO ExitProgram IF UTL_REPORTX::STAT
	END IF

	!
	! Look up user defined account
	!
	TEST_ACCOUNT$ = ""

	WHEN ERROR IN
		GET #PR_EMP_STD_ERNDED.CH%, &
			KEY #0% EQ PR_TEMP::EMPNUM + PR_TEMP::DTYPE + &
			PR_TEMP::CODE, &
			REGARDLESS
		TEST_ACCOUNT$ = PR_EMP_STD_ERNDED::USERDEF
	USE
		CONTINUE 17075
	END WHEN

17075	TEXT$ = LEFT(TEXT$ + SPACE$(39%), 39%) + &
		PR_TEMP::EMPNUM + " " + &
		LEFT(PR_TEMP::EMPNAME, 22%) + " " + &
		PRNT_DATE(PR_TEMP::PR_END_DATE, 6%)

	TEST_NAME$ = PR_TEMP::EMPNAME

17100	!
	! Add employee deduction total
	!
	TEST_CODE$ = PR_TEMP::DTYPE + PR_TEMP::CODE
	TEST_EMPNUM$ = PR_TEMP::EMPNUM + PR_TEMP::PR_END_DATE

	EMP_TOTAL = EMP_TOTAL + PR_TEMP::AMOUNT
	LOC_TOTAL = LOC_TOTAL + PR_TEMP::AMOUNT
	EMP_COUNT% = EMP_COUNT% + 1%
	LOC_COUNT% = LOC_COUNT% + 1%

	!
	! Try for next record
	!
	GOTO 17020

	%PAGE

 ExitTotal:
	!
	! Handle end of report
	!
	IF EMP_COUNT% <> 0%
	THEN
		GOSUB EmployeeTotal
		GOTO ExitProgram IF UTL_REPORTX::STAT

	END IF

	GOSUB CodeTotal
	GOTO ExitProgram IF UTL_REPORTX::STAT

	IF SORTBY$ = "LO"
	THEN
		GOSUB LocationTotal
	END IF

	TEXT$ = SPACE$(39%) + "Grand Total"
	TEXT$ = LEFT(TEXT$ + SPACE$(81%), 81%) + &
		FORMAT$(GRAND_TOTAL, "  #,###,###.##     ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Output Company/Batch Control Record
	!
	DIRECTDEPOSIT$ = ""
	ACH_COMPANYBATCH::RTYPE		= "8"
	ACH_COMPANYBATCH::SERVICE	= "200"
	ACH_COMPANYBATCH::ADDENDACNT	= FORMAT$(ADDCOUNT%, "<0>#####")
	ACH_COMPANYBATCH::ENTRYHASH	= FORMAT$(HASHCOUNT, "<0>#########")
	ACH_COMPANYBATCH::TOTALCREDIT	= FORMAT$(GRAND_TOTAL * 100.0, "<0>###########")
	ACH_COMPANYBATCH::TOTALDEBIT	= "000000000000"
	RSET ACH_COMPANYBATCH::CID	= "1" + ACCTCODE$
	ACH_COMPANYBATCH::ODFI		= MID(BANKCODE$, 1%, 8%)
	ACH_COMPANYBATCH::BATCH		= BATCH$

	PRINT #DIRECT.CH%, DIRECTDEPOSIT$

	!
	! Output File Control Record
	!
	DIRECTDEPOSIT$ = ""
	ACH_FILETOTAL::RTYPE		= "9"
	ACH_FILETOTAL::BATCHCOUNT	= "000001"
	ACH_FILETOTAL::BLOCKCOUNT	= &
		FORMAT$((ADDCOUNT% + 4% + 9%) / 10%, "<0>#####")
	ACH_FILETOTAL::ENTRYCOUNT	= FORMAT$(ADDCOUNT%, "<0>#######")
	ACH_FILETOTAL::ENTRYHASH	= FORMAT$(HASHCOUNT, "<0>#########")
	ACH_FILETOTAL::TOTALCREDIT	= FORMAT$(GRAND_TOTAL * 100.0, "<0>###########")
	ACH_FILETOTAL::TOTALDEBIT	= "000000000000"

	PRINT #DIRECT.CH%, DIRECTDEPOSIT$

	IF FIXED_NUMBER%
	THEN
		GOSUB BoaCode
	END IF

 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Close channel
	!
	CLOSE PR_TEMP.CH%
	CLOSE DIRECT.CH%

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

18110	TEXT$ = LEFT(TEXT$ + SPACE$(81%), 81%) + &
		FORMAT$(EMP_TOTAL, "  #,###,###.##     ") + TEST_ACCOUNT$

	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	ADDCOUNT% = ADDCOUNT% + 1%
 !	HASHCOUNT = HASHCOUNT + VAL(ACH_ENTRYDETAIL::RECDFI)
 !	HASHCOUNT = HASHCOUNT + VAL(MID$(TEST_ACCOUNT$, 1%, 8%))

	!
	TEST_ACCOUNT$ = EDIT$(TEST_ACCOUNT$, 16% + 128%)
	I1% = INSTR(1%, TEST_ACCOUNT$, " ")
	I2% = INSTR(I1% + 1%, TEST_ACCOUNT$, " ")
	IF I2% = 0%
	THEN
		TEXT$ = " ***** Malformed Account Definition *****"
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, -1%)
		I2% = LEN(TEST_ACCOUNT$) + 1%
	END IF

	TRANCODE$ = LEFT(TEST_ACCOUNT$, I1% - 1%)
	RECDFI$ = SEG$(TEST_ACCOUNT$, I1% + 1%, I2% - 1%)
	TRANSIT$ = RIGHT(RECDFI$, LEN(RECDFI$))
	DFI$ = RIGHT(TEST_ACCOUNT$, I2% + 1%)

	!
	! Output Entry/Detail Recoord
	!
	DIRECTDEPOSIT$ = ""

	IF (FUNC_ROUND(EMP_TOTAL, 2%) = 0.0) AND (TRANCODE$ = "22")
	THEN
		!
		! For zero records, we have to change type 22 to 23
		! or the bank gets upset.
		!
		TRANCODE$ = "23"
	END IF

	ACH_ENTRYDETAIL::RTYPE		= "6"
	ACH_ENTRYDETAIL::TRANCODE	= TRANCODE$
	ACH_ENTRYDETAIL::RECDFI		= RECDFI$
	ACH_ENTRYDETAIL::TRANSIT	= TRANSIT$
	ACH_ENTRYDETAIL::DFI		= DFI$
	ACH_ENTRYDETAIL::AMOUNT		= FORMAT$(EMP_TOTAL * 100.0, "<0>#########")
	ACH_ENTRYDETAIL::IIN		= LEFT(TEST_EMPNUM$, 10%)
	ACH_ENTRYDETAIL::INAME		= TEST_NAME$
	ACH_ENTRYDETAIL::DDATA		= "DIRCT DPST"
	ACH_ENTRYDETAIL::ADDENDA	= "0"
	IF FIXED_NUMBER%
	THEN
		ACH_ENTRYDETAIL::TRACENO	= LEFT(BANKCODE$, 8%) + &
			FORMAT$(ADDCOUNT%, "<0>######")
	ELSE
		ACH_ENTRYDETAIL::TRACENO	= BATCH$ + &
			FORMAT$(ADDCOUNT%, "<0>########")
	END IF

	PRINT #DIRECT.CH%, DIRECTDEPOSIT$

	HASHCOUNT = HASHCOUNT + VAL(ACH_ENTRYDETAIL::RECDFI)

	!
	! Summarize totals
	!
	CODE_TOTAL = CODE_TOTAL + EMP_TOTAL

	EMP_TOTAL = 0.0
	EMP_COUNT% = 0%

	TEXT$ = ""
	TEST_ACCOUNT$ = ""

	RETURN

	%Page

 CodeTotal:
	!******************************************************************
	! Print Code total
	!******************************************************************

	IF (TEST_CODE$ <> "") OR (CODE_TOTAL <> 0.0)
	THEN
		TEXT1$ = SPACE$(39%) + &
			"Total for Deduction " + RIGHT(TEST_CODE$, 2%)
		TEXT1$ = LEFT(TEXT1$ + SPACE$(81%), 81%) + &
			FORMAT$(CODE_TOTAL, "  #,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT1$, 0%)

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	GRAND_TOTAL = GRAND_TOTAL + CODE_TOTAL

	CODE_TOTAL = 0.0

	TEXT$ = ""

	RETURN

	%PAGE

	!*******************************************************************
	! Page for a new location
	!*******************************************************************

 LocationTotal:
	IF THISLOC$ <> ""
	THEN

		TEXT1$ = "      Total for location " + THISLOC$
		TEXT1$ = LEFT(TEXT1$ + SPACE$(81%), 81%) + &
			FORMAT$(LOC_TOTAL, "  #,###,###.##     ")

		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT1$, 0%)

		TITLE$(3%) = "For Location " + PR_TEMP::SORTS
	END IF

	!
	! SET UP FOR NEXT PASS
	!
	THISLOC$ = PR_TEMP::SORTS
	TEST_CODE$ = ""
	TITLE$(3%) = "For Location " + PR_TEMP::SORTS
	LOC_TOTAL = 0.0
	LOC_COUNT% = 0%

	RETURN

	%PAGE

 GetMaster:
	!*******************************************************************
	! Find employee master file record
	!*******************************************************************

18300	GOTO 18390 IF PR_EMP_MASTER::EMPNUM = PR_TRN_DED::EMPNUM

	PR_EMP_MASTER::EMPNAME = "????????????????????????????????????"

	WHEN ERROR IN
		GET #PR_EMP_MASTER.CH%, &
			KEY #0% EQ PR_TRN_DED::EMPNUM, &
			REGARDLESS
	USE
		CONTINUE 18390
	END WHEN

18390	RETURN

	%PAGE


 BoaCode:
18400	!*******************************************************************
	! Output BOA Code
	! (Bank Of America Transmittal code
	!*******************************************************************
	TOTAL% = FIXED_NUMBER%
	TEXT$ = "BOA Code Calculation"
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 10%)
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 0%)

	TEXT$ = "Fixed Number           " + FORMAT$(FIXED_NUMBER%, "######")
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Year
	!
	AMT% = VAL%(MID(FROM_BATCH_NO$, 5%, 2%))
	SELECT AMT%
	CASE 1%
		THIS_AMOUNT% = 158%
	CASE 2%
		THIS_AMOUNT% = 44%
	CASE 3%
		THIS_AMOUNT% = 213%
	CASE 4%
		THIS_AMOUNT% = 182%
	CASE 5%
		THIS_AMOUNT% = 37%
	CASE 6%
		THIS_AMOUNT% = 161%
	CASE 7%
		THIS_AMOUNT% = 12%
	CASE 8%
		THIS_AMOUNT% = 103%
	CASE 9%
		THIS_AMOUNT% = 96%
	CASE 10%
		THIS_AMOUNT% = 85%
	CASE 11%
		THIS_AMOUNT% = 227%
	CASE 12%
		THIS_AMOUNT% = 149%
	CASE ELSE
		THIS_AMOUNT% = 0%
	END SELECT

	TOTAL% = TOTAL% + THIS_AMOUNT%
	TEXT$ = "Month    " + FORMAT$(AMT%, "######") + &
		"        " + FORMAT$(THIS_AMOUNT%, "######")
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Month
	!
	AMT% = VAL%(MID(FROM_BATCH_NO$, 7%, 2%))
	SELECT AMT%
	CASE 1%
		THIS_AMOUNT% = 112%
	CASE 2%
		THIS_AMOUNT% = 108%
	CASE 3%
		THIS_AMOUNT% = 49%
	CASE 4%
		THIS_AMOUNT% = 173%
	CASE 5%
		THIS_AMOUNT% = 274%
	CASE 6%
		THIS_AMOUNT% = 41%
	CASE 7%
		THIS_AMOUNT% = 137%
	CASE 8%
		THIS_AMOUNT% = 70%
	CASE 9%
		THIS_AMOUNT% = 68%
	CASE 10%
		THIS_AMOUNT% = 56%
	CASE 11%
		THIS_AMOUNT% = 203%
	CASE 12%
		THIS_AMOUNT% = 118%
	CASE 13%
		THIS_AMOUNT% = 22%
	CASE 14%
		THIS_AMOUNT% = 226%
	CASE 15%
		THIS_AMOUNT% = 199%
	CASE 16%
		THIS_AMOUNT% = 55%
	CASE 17%
		THIS_AMOUNT% = 211%
	CASE 18%
		THIS_AMOUNT% = 158%
	CASE 19%
		THIS_AMOUNT% = 27%
	CASE 20%
		THIS_AMOUNT% = 140%
	CASE 21%
		THIS_AMOUNT% = 152%
	CASE 22%
		THIS_AMOUNT% = 135%
	CASE 23%
		THIS_AMOUNT% = 81%
	CASE 24%
		THIS_AMOUNT% = 260%
	CASE 25%
		THIS_AMOUNT% = 113%
	CASE 26%
		THIS_AMOUNT% = 61%
	CASE 27%
		THIS_AMOUNT% = 110%
	CASE 28%
		THIS_AMOUNT% = 98%
	CASE 29%
		THIS_AMOUNT% = 32%
	CASE 30%
		THIS_AMOUNT% = 251%
	CASE 31%
		THIS_AMOUNT% = 274%
	CASE ELSE
		THIS_AMOUNT% = 0%
	END SELECT

	TOTAL% = TOTAL% + THIS_AMOUNT%
	TEXT$ = "Date     " + FORMAT$(AMT%, "######") + &
		"        " + FORMAT$(THIS_AMOUNT%, "######")
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	ABSAMOUNT% = INT(ABS(GRAND_TOTAL))

	!
	! Prenote
	!
	IF ABSAMOUNT% = 0%
	THEN
		THIS_AMOUNT% = 52%
		TOTAL% = TOTAL% + THIS_AMOUNT%
		TEXT$ = "Prenote                " + &
			FORMAT$(THIS_AMOUNT%, "######")
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! 100,000,000,000
	!
 !	AMT% = MOD((ABSAMOUNT% / 100000000000%), 10%)
 !
 !	SELECT AMT%
 !	CASE 1%
 !		THIS_AMOUNT% = 22%
 !	CASE 2%
 !		THIS_AMOUNT% = 14%
 !	CASE 3%
 !		THIS_AMOUNT% = 21%
 !	CASE 4%
 !		THIS_AMOUNT% = 8%
 !	CASE 5%
 !		THIS_AMOUNT% = 20%
 !	CASE 6%
 !		THIS_AMOUNT% = 7%
 !	CASE 7%
 !		THIS_AMOUNT% = 2%
 !	CASE 8%
 !		THIS_AMOUNT% = 11%
 !	CASE 9%
 !		THIS_AMOUNT% = 4%
 !	END SELECT

	!
	! 10,000,000,000
	!
	AMT% = MOD(ABSAMOUNT% / 1000000000%, 10%)

	SELECT AMT%
	CASE 1%
		THIS_AMOUNT% = 51%
	CASE 2%
		THIS_AMOUNT% = 91%
	CASE 3%
		THIS_AMOUNT% = 8%
	CASE 4%
		THIS_AMOUNT% = 1%
	CASE 5%
		THIS_AMOUNT% = 5%
	CASE 6%
		THIS_AMOUNT% = 8%
	CASE 7%
		THIS_AMOUNT% = 6%
	CASE 8%
		THIS_AMOUNT% = 184%
	CASE 9%
		THIS_AMOUNT% = 2%
	CASE ELSE
		THIS_AMOUNT% = 0%
	END SELECT

	IF THIS_AMOUNT% <> 0%
	THEN
		TOTAL% = TOTAL% + THIS_AMOUNT%
		TEXT$ = "Amount " + FORMAT$(AMT%, "########") + &
			"        " + FORMAT$(THIS_AMOUNT%, "######")
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! 1,000,000,000
	!
	AMT% = MOD(ABSAMOUNT% / 100000000%, 10%)

	SELECT AMT%
	CASE 1%
		THIS_AMOUNT% = 64%
	CASE 2%
		THIS_AMOUNT% = 301%
	CASE 3%
		THIS_AMOUNT% = 218%
	CASE 4%
		THIS_AMOUNT% = 151%
	CASE 5%
		THIS_AMOUNT% = 135%
	CASE 6%
		THIS_AMOUNT% = 97%
	CASE 7%
		THIS_AMOUNT% = 277%
	CASE 8%
		THIS_AMOUNT% = 94%
	CASE 9%
		THIS_AMOUNT% = 182%
	CASE ELSE
		THIS_AMOUNT% = 0%
	END SELECT

	IF THIS_AMOUNT% <> 0%
	THEN
		TOTAL% = TOTAL% + THIS_AMOUNT%
		TEXT$ = "Amount " + FORMAT$(AMT%, "########") + &
			"        " + FORMAT$(THIS_AMOUNT%, "######")
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! 100,000,000
	!
	AMT% = MOD(ABSAMOUNT% / 100000000%, 10%)

	SELECT AMT%
	CASE 1%
		THIS_AMOUNT% = 54%
	CASE 2%
		THIS_AMOUNT% = 291%
	CASE 3%
		THIS_AMOUNT% = 208%
	CASE 4%
		THIS_AMOUNT% = 141%
	CASE 5%
		THIS_AMOUNT% = 125%
	CASE 6%
		THIS_AMOUNT% = 87%
	CASE 7%
		THIS_AMOUNT% = 167%
	CASE 8%
		THIS_AMOUNT% = 84%
	CASE 9%
		THIS_AMOUNT% = 172%
	CASE ELSE
		THIS_AMOUNT% = 0%
	END SELECT

	IF THIS_AMOUNT% <> 0%
	THEN
		TOTAL% = TOTAL% + THIS_AMOUNT%
		TEXT$ = "Amount " + FORMAT$(AMT%, "########") + &
			"        " + FORMAT$(THIS_AMOUNT%, "######")
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! 10,000,000
	!
	AMT% = MOD(ABSAMOUNT% / 10000000%, 10%)

	SELECT AMT%
	CASE 1%
		THIS_AMOUNT% = 44%
	CASE 2%
		THIS_AMOUNT% = 281%
	CASE 3%
		THIS_AMOUNT% = 198%
	CASE 4%
		THIS_AMOUNT% = 131%
	CASE 5%
		THIS_AMOUNT% = 125%
	CASE 6%
		THIS_AMOUNT% = 77%
	CASE 7%
		THIS_AMOUNT% = 257%
	CASE 8%
		THIS_AMOUNT% = 74%
	CASE 9%
		THIS_AMOUNT% = 162%
	CASE ELSE
		THIS_AMOUNT% = 0%
	END SELECT

	IF THIS_AMOUNT% <> 0%
	THEN
		TOTAL% = TOTAL% + THIS_AMOUNT%
		TEXT$ = "Amount " + FORMAT$(AMT%, "########") + &
			"        " + FORMAT$(THIS_AMOUNT%, "######")
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! 1,000,000
	!
	AMT% = MOD(ABSAMOUNT% / 1000000%, 10%)

	SELECT AMT%
	CASE 1%
		THIS_AMOUNT% = 280%
	CASE 2%
		THIS_AMOUNT% = 167%
	CASE 3%
		THIS_AMOUNT% = 110%
	CASE 4%
		THIS_AMOUNT% = 136%
	CASE 5%
		THIS_AMOUNT% = 75%
	CASE 6%
		THIS_AMOUNT% = 61%
	CASE 7%
		THIS_AMOUNT% = 126%
	CASE 8%
		THIS_AMOUNT% = 151%
	CASE 9%
		THIS_AMOUNT% = 98%
	CASE ELSE
		THIS_AMOUNT% = 0%
	END SELECT

	IF THIS_AMOUNT% <> 0%
	THEN
		TOTAL% = TOTAL% + THIS_AMOUNT%
		TEXT$ = "Amount " + FORMAT$(AMT%, "########") + &
			"        " + FORMAT$(THIS_AMOUNT%, "######")
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! 100,000
	!
	AMT% = MOD(ABSAMOUNT% / 100000%, 10%)

	SELECT AMT%
	CASE 1%
		THIS_AMOUNT% = 80%
	CASE 2%
		THIS_AMOUNT% = 118%
	CASE 3%
		THIS_AMOUNT% = 133%
	CASE 4%
		THIS_AMOUNT% = 189%
	CASE 5%
		THIS_AMOUNT% = 111%
	CASE 6%
		THIS_AMOUNT% = 156%
	CASE 7%
		THIS_AMOUNT% = 177%
	CASE 8%
		THIS_AMOUNT% = 114%
	CASE 9%
		THIS_AMOUNT% = 137%
	CASE ELSE
		THIS_AMOUNT% = 0%
	END SELECT

	IF THIS_AMOUNT% <> 0%
	THEN
		TOTAL% = TOTAL% + THIS_AMOUNT%
		TEXT$ = "Amount " + FORMAT$(AMT%, "########") + &
			"        " + FORMAT$(THIS_AMOUNT%, "######")
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! 10,000
	!
	AMT% = MOD(ABSAMOUNT% / 10000%, 10%)

	SELECT AMT%
	CASE 1%
		THIS_AMOUNT% = 30%
	CASE 2%
		THIS_AMOUNT% = 46%
	CASE 3%
		THIS_AMOUNT% = 119%
	CASE 4%
		THIS_AMOUNT% = 141%
	CASE 5%
		THIS_AMOUNT% = 208%
	CASE 6%
		THIS_AMOUNT% = 82%
	CASE 7%
		THIS_AMOUNT% = 191%
	CASE 8%
		THIS_AMOUNT% = 205%
	CASE 9%
		THIS_AMOUNT% = 159%
	CASE ELSE
		THIS_AMOUNT% = 0%
	END SELECT

	IF THIS_AMOUNT% <> 0%
	THEN
		TOTAL% = TOTAL% + THIS_AMOUNT%
		TEXT$ = "Amount " + FORMAT$(AMT%, "########") + &
			"        " + FORMAT$(THIS_AMOUNT%, "######")
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! 1,000
	!
	AMT% = MOD(ABSAMOUNT% / 1000%, 10%)

	SELECT AMT%
	CASE 1%
		THIS_AMOUNT% = 140%
	CASE 2%
		THIS_AMOUNT% = 72%
	CASE 3%
		THIS_AMOUNT% = 91%
	CASE 4%
		THIS_AMOUNT% = 161%
	CASE 5%
		THIS_AMOUNT% = 112%
	CASE 6%
		THIS_AMOUNT% = 63%
	CASE 7%
		THIS_AMOUNT% = 214%
	CASE 8%
		THIS_AMOUNT% = 122%
	CASE 9%
		THIS_AMOUNT% = 243%
	CASE ELSE
		THIS_AMOUNT% = 0%
	END SELECT

	IF THIS_AMOUNT% <> 0%
	THEN
		TOTAL% = TOTAL% + THIS_AMOUNT%
		TEXT$ = "Amount " + FORMAT$(AMT%, "########") + &
			"        " + FORMAT$(THIS_AMOUNT%, "######")
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! 100
	!
	AMT% = MOD(ABSAMOUNT% / 100%, 10%)

	SELECT AMT%
	CASE 1%
		THIS_AMOUNT% = 152%
	CASE 2%
		THIS_AMOUNT% = 213%
	CASE 3%
		THIS_AMOUNT% = 99%
	CASE 4%
		THIS_AMOUNT% = 207%
	CASE 5%
		THIS_AMOUNT% = 135%
	CASE 6%
		THIS_AMOUNT% = 148%
	CASE 7%
		THIS_AMOUNT% = 154%
	CASE 8%
		THIS_AMOUNT% = 252%
	CASE 9%
		THIS_AMOUNT% = 73%
	CASE ELSE
		THIS_AMOUNT% = 0%
	END SELECT

	IF THIS_AMOUNT% <> 0%
	THEN
		TOTAL% = TOTAL% + THIS_AMOUNT%
		TEXT$ = "Amount " + FORMAT$(AMT%, "########") + &
			"        " + FORMAT$(THIS_AMOUNT%, "######")
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! 10
	!
	AMT% = MOD(ABSAMOUNT% / 10%, 10%)

	SELECT AMT%
	CASE 1%
		THIS_AMOUNT% = 187%
	CASE 2%
		THIS_AMOUNT% = 228%
	CASE 3%
		THIS_AMOUNT% = 183%
	CASE 4%
		THIS_AMOUNT% = 232%
	CASE 5%
		THIS_AMOUNT% = 164%
	CASE 6%
		THIS_AMOUNT% = 155%
	CASE 7%
		THIS_AMOUNT% = 132%
	CASE 8%
		THIS_AMOUNT% = 71%
	CASE 9%
		THIS_AMOUNT% = 22%
	CASE ELSE
		THIS_AMOUNT% = 0%
	END SELECT

	IF THIS_AMOUNT% <> 0%
	THEN
		TOTAL% = TOTAL% + THIS_AMOUNT%
		TEXT$ = "Amount " + FORMAT$(AMT%, "########") + &
			"        " + FORMAT$(THIS_AMOUNT%, "######")
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	!
	! 10
	!
	AMT% = MOD(ABSAMOUNT%, 10%)

	SELECT AMT%
	CASE 1%
		THIS_AMOUNT% = 140%
	CASE 2%
		THIS_AMOUNT% = 68%
	CASE 3%
		THIS_AMOUNT% = 172%
	CASE 4%
		THIS_AMOUNT% = 214%
	CASE 5%
		THIS_AMOUNT% = 160%
	CASE 6%
		THIS_AMOUNT% = 133%
	CASE 7%
		THIS_AMOUNT% = 182%
	CASE 8%
		THIS_AMOUNT% = 79%
	CASE 9%
		THIS_AMOUNT% = 131%
	CASE ELSE
		THIS_AMOUNT% = 0%
	END SELECT

	IF THIS_AMOUNT% <> 0%
	THEN
		TOTAL% = TOTAL% + THIS_AMOUNT%
		TEXT$ = "Amount " + FORMAT$(AMT%, "########") + &
			"        " + FORMAT$(THIS_AMOUNT%, "######")
		CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF


	TEXT$ = "Fixed Number  " + &
		"         " + FORMAT$(TOTAL%, "######")
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)

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

32767	END
