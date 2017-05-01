1	%TITLE "Payroll Posting Program"
	%SBTTL "PR_TRAN_POST"
	%IDENT "V3.6a Calico"

	SUB PR_TRAN_POST(PASS%, &
		POST_STATUS%, &
		UPDATE_FLAG%, &
		FILE_NAME$, &
		BATCHNO$, &
		UTL_BATCH.CH%, &
		PR_ACCRUAL.CH%, &
		PR_FINAL.CH%, &
		PR_EMP_MASTER.CH%)

	!
	! COPYRIGHT (C) 1986 BY
	! Computer Management Center
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
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
	! ABSTRACT:HELP
	!	.p
	!	This function is used to make 3 passes through a
	!	file being posted:
	!		Pass 1 will kill crashed entries.
	!		Pass 2 will check for invalid employee names
	!		Pass 4 will actually add the records
	!	NOTE:  It is assumed that the file being posted
	!	is not modified between the two passes, although
	!	totals are calculated during the first path only.
	!		POST_STATUS% flag
	!			1% = Undefined account or key
	!			2% = Journal out of balance
	!			4% = Abort
	!		NOTE::  The POST_STATUS% flag can be added to but
	!			should not in any other way be altered.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_TRAN_POST
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_TRAN_POST
	!	$ DELETE PR_TRAN_POST.OBJ;*
	!
	! AUTHOR:
	!
	!	01/01/88 - Robert Peterson
	!
	! MODIFICATION HISTORY:
	!
	!	08/23/90 - Kevin Handy
	!		Modified to skip search of PR_EMP_MASTER if the
	!		current employee is already loaded.
	!
	!	04/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	02/03/94 - Kevin Handy
	!		Modifications to fix problem with vacation
	!		accrual records being "accrue" posted.
	!
	!	02/16/94 - Kevin Handy
	!		Format change so everything fit into 80 columns
	!		better (while looking for bad <exit> code).
	!
	!	01/18/95 - Kevin Handy
	!		Trap record not found in kill loop.
	!
	!	01/18/95 - Kevin Handy
	!		Add error message for line 1320, so we can see
	!		just what is causing the problem.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	07/19/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!
	!	08/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/21/99 - Kevin Handy
	!		Fix unsolicited input
	!
	!	12/08/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_EMP_MASTER.HB"
	MAP (PR_EMP_MASTER)	PR_EMP_MASTER_CDD	PR_EMP_MASTER

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD	PR_TRN_DED

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP (PR_TRN_CHECK)	PR_TRN_CHECK_CDD	PR_TRN_CHECK

	RECORD	UTL_BATCH
		STRING	FILE_NAME = 6%
		RFA	RFA_RFA
	END RECORD UTL_BATCH

	MAP (UTL_BATCH)	UTL_BATCH	UTL_BATCH

	MAP (DP_OUTP_XUNSOL) RRR_FLAG%

	!
	! Externals Functions
	!
	EXTERNAL LONG    OUTP_XUNSOL ! (It's really an AST routine)

	%PAGE

	ON ERROR GOTO 19000

1000	SELECT PASS%

	!
	! In the first pass, we will remove all entries
	! that have been posted to the registers with
	! the batch number that was interrupted
	!
	CASE 1%
1100		!***************************************************
		! This subroutine deletes all of the records in the
		! Transaction files that has the same  batch number
		! as the current batch.
		!***************************************************
		!
		! Set up to trap interrupt
		!
		SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID, &
			LOC(OUTP_XUNSOL) BY VALUE, &
			LOC(SCOPE::SMG_KBID) BY VALUE)

		RRR_FLAG% = 0%

		GOTO 1170 IF UPDATE_FLAG% <> 2% OR PR_FINAL.CH% = 0%

1105	!
 KillLoop:	GOSUB Interupt IF RRR_FLAG%

		WHEN ERROR IN
			GET #UTL_BATCH.CH%, KEY #0% EQ FILE_NAME$
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1120 IF ERR = 155%
			EXIT HANDLER
		END WHEN

		DELETE #UTL_BATCH.CH%

1110		WHEN ERROR IN
			GET #PR_FINAL.CH%, RFA UTL_BATCH::RFA_RFA
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1105
		END WHEN

		DELETE #PR_FINAL.CH%

		GOTO KillLoop

1120		!
		! Disable unsolicited input
		!
		SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

		!
		! Look for more records in this batch
		!
		WHEN ERROR IN
			RESET #PR_FINAL.CH%
		USE
			CONTINUE 1170
		END WHEN

		!
		! Create the data display
		!
		SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY(18%, &
			130%, SMG_VIEW%, SMG$M_BORDER)

		SMG_STATUS% = SMG$CHANGE_PBD_CHARACTERISTICS(SCOPE::SMG_PBID, &
			132%)

		SMG_STATUS% = SMG$LABEL_BORDER(SMG_VIEW%, &
			" Payroll Journal Post - ERROR ")

		!
		! Paste on the data display
		!
		SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY(SMG_VIEW%, &
			SCOPE::SMG_PBID, 2%, 2%)

		SELECT FILE_NAME$

		CASE "PAY"
			SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW%, &
				"Employee # End Date   " + &
				"Account #          SubAcct    " + &
				"Oper     Loc  Dept   WC CD       " + &
				"Rate   Reg Hr  " + &
				"Ovt Hr       Units     Gross", &
				9%, 1%, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW%, &
				"Remove record from the " + &
				"transaction file (PAY) ? ", &
				18%, 1%, , SMG$M_BOLD)

		CASE "DED"
			SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW%, &
				"Employee # End Date    Type  CD      " + &
				"Amount  Tax Code", &
				9%, 1%, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW%, &
				"Remove record from the transaction " + &
				"file (DEDUCTION) ? ", &
				18%, 1%, , SMG$M_BOLD)

		CASE "CHECK"
			SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW%, &
				"Employee # End Date    Ck #   Check Date", &
				9%, 1%, SMG$M_BOLD)

			SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW%, &
				"Remove record from the " + &
				"transaction file (CHECK) ? ", &
				18%, 1%, , SMG$M_BOLD)

		END SELECT

1140		GOSUB Interupt IF RRR_FLAG%

		WHEN ERROR IN
			GET #PR_FINAL.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 1170
		END WHEN

		SELECT FILE_NAME$

		CASE "PAY"
			IF PR_TRN_PAY::BATCH <> BATCHNO$
			THEN
				GOTO 1140
			END IF

		CASE "DED"
			IF PR_TRN_DED::BATCH <> BATCHNO$
			THEN
				GOTO 1140
			END IF

		CASE "CHECK"
			IF PR_TRN_CHECK::BATCH <> BATCHNO$
			THEN
				GOTO 1140
			END IF
		END SELECT

		SELECT FILE_NAME$

		CASE "PAY"
			SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW%, &
				PR_TRN_PAY::EMPNUM + " " + &
				PRNT_DATE(PR_TRN_PAY::PR_END_DATE, 8%) + " " + &
				PR_TRN_PAY::ACCT + " " + &
				PR_TRN_PAY::SUBACC + " " + &
				PR_TRN_PAY::OPER + " " + &
				PR_TRN_PAY::LOCATION + " " + &
				PR_TRN_PAY::DEPT + " " + &
				PR_TRN_PAY::WORK_CENTER + " " + &
				PR_TRN_PAY::CODE + " " + &
				FORMAT$(PR_TRN_PAY::HOUR_RATE, "######.### ") + &
				FORMAT$(PR_TRN_PAY::REG_HR, "#####.## ") + &
				FORMAT$(PR_TRN_PAY::OVT_HR, "####.## ") + &
				FORMAT$(PR_TRN_PAY::PIECE, "######.#### ") + &
				FORMAT$(PR_TRN_PAY::GROSS, "######.##"), &
				10%, 1%, , SMG$M_BOLD)

		CASE "DED"
			SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW%, &
				PR_TRN_DED::EMPNUM + " " + &
				PRNT_DATE(PR_TRN_DED::PR_END_DATE, 8%) + "    " + &
				PR_TRN_DED::DTYPE + "   " + &
				PR_TRN_DED::CODE + " " + &
				FORMAT$(PR_TRN_DED::AMOUNT, "######.##     ") + &
				PR_TRN_DED::TAX_CODE, &
				10%, 1%, , SMG$M_BOLD)

		CASE "CHECK"
			SMG_STATUS% = SMG$PUT_CHARS(SMG_VIEW%, &
				PR_TRN_CHECK::EMPNUM + " " + &
				PRNT_DATE(PR_TRN_CHECK::PR_END_DATE, 8%) + " " + &
				PR_TRN_CHECK::CHECK + " " + &
				PRNT_DATE(PR_TRN_CHECK::CHECK_DATE, 8%), &
				10%, 1%, , SMG$M_BOLD)

		END SELECT

1150		TEMP_IDENT$ = SCOPE::PRG_IDENT
		TEMP_ITEM$ = SCOPE::PRG_ITEM

		SCOPE::PRG_IDENT = "POST"
		SCOPE::PRG_ITEM = "DELETE_ENTRY"

		INP$ = EDIT$(ENTR_3YESNO(SCOPE, SCOPE::SMG_OPTION, "", &
			"Confirm Deletion of Entry - then press <Do> ", &
			"N", 0%, "", ""), -1%)

		SCOPE::PRG_IDENT = TEMP_IDENT$
		SCOPE::PRG_ITEM = TEMP_ITEM$

		SELECT SCOPE::SCOPE_EXIT

		!
		! Exit
		!
		CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			POST_STATUS% = (POST_STATUS% OR 4%)
			SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY( &
				SMG_VIEW%, SCOPE::SMG_PBID)
			GOTO ExitSubroutine

		!
		! Normal key typed
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		!
		! Bad key typed
		!
		CASE ELSE
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 1150

		END SELECT

1160		IF INP$ = "Y"
		THEN
			DELETE #PR_FINAL.CH%
		END IF

		GOTO 1140

1170		!
		! remove the posting flag from the update flag
		!
		TEMP.CH% = PR_FINAL.CH%
		TEMP.CH% = PR_ACCRUAL.CH% IF UPDATE_FLAG% = 2%

		WHEN ERROR IN
			RESET #TEMP.CH%
		USE
			CONTINUE 1190
		END WHEN

1180		GOSUB Interupt IF RRR_FLAG%

		WHEN ERROR IN
			GET #TEMP.CH%
		USE
			CONTINUE 1190
		END WHEN

		SELECT FILE_NAME$

		CASE "PAY"
			IF PR_TRN_PAY::BATCH <> BATCHNO$ OR &
				(PR_TRN_PAY::UPDATE_FLAG AND UPDATE_FLAG%) = 0%
			THEN
				GOTO 1180
			END IF

			PR_TRN_PAY::BATCH = ""
			PR_TRN_PAY::UPDATE_FLAG = &
				(PR_TRN_PAY::UPDATE_FLAG - UPDATE_FLAG%)

		CASE "DED"

			IF PR_TRN_DED::BATCH <> BATCHNO$ OR &
				(PR_TRN_DED::UPDATE_FLAG AND UPDATE_FLAG%) = 0%
			THEN
				GOTO 1180
			END IF

			PR_TRN_DED::BATCH = ""
			PR_TRN_DED::UPDATE_FLAG = &
				(PR_TRN_DED::UPDATE_FLAG - UPDATE_FLAG%)

		CASE "CHECK"

			IF PR_TRN_CHECK::BATCH <> BATCHNO$ OR &
				(PR_TRN_CHECK::UPDATE_FLAG AND &
				UPDATE_FLAG%) = 0%
			THEN
				GOTO 1180
			END IF

			PR_TRN_CHECK::BATCH = ""
			PR_TRN_CHECK::UPDATE_FLAG = &
				(PR_TRN_CHECK::UPDATE_FLAG - UPDATE_FLAG%)
		END SELECT

		UPDATE #TEMP.CH%

		GOTO 1180

1190		!
		! Remove Window
		!
		IF SMG_VIEW%
		THEN
			SMG_STATUS% = SMG$POP_VIRTUAL_DISPLAY(SMG_VIEW%, &
				SCOPE::SMG_PBID)
		END IF
		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)

	!
	! In the second pass, we will check for invalid
	! employee numbers in the header file
	!
	CASE 2%
1200		!
		! Check for correct employee numbers
		!
		TEMP$ = ""

		SELECT FILE_NAME$

		CASE "PAY"
			TEMP$ = PR_TRN_PAY::EMPNUM

		CASE "DED"
			TEMP$ = PR_TRN_DED::EMPNUM

		CASE "CHECK"
			TEMP$ = PR_TRN_CHECK::EMPNUM

		END SELECT

		IF PR_EMP_MASTER::EMPNUM <> TEMP$
		THEN
			WHEN ERROR IN
				GET #PR_EMP_MASTER.CH%, &
					KEY #0% EQ TEMP$, &
					REGARDLESS
			USE
				CONTINUE 1230
			END WHEN
		END IF

		GOTO ExitSubroutine

1230		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

		TEMP_IDENT$ = SCOPE::PRG_IDENT
		TEMP_ITEM$ = SCOPE::PRG_ITEM

		SCOPE::PRG_IDENT = "POST"
		SCOPE::PRG_ITEM = "UNDEFINED_EMPLOYEE"

		SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_MESSAGE, &
			"Undefined EMPLOYEE " + &
			"number:  " + TRM$(TEMP$) + "     " + &
			"     Press <RETURN> to continue, <EXIT> to ABORT!!!", &
			2%, 1%, 0%, SMG$M_BOLD)

		!
		! Get input
		!
		JUNK% = 0%
		WHILE JUNK% = 0%
			JUNK% = ENTR_4ENTRY(SCOPE, &
				SCOPE::SMG_MESSAGE BY VALUE, 0% BY VALUE)
			JUNK% = ENTR_4SPECIALKEYS(SCOPE, &
				SCOPE::SMG_MESSAGE BY VALUE, &
				0% BY VALUE, JUNK% BY VALUE)
		NEXT

		SCOPE::PRG_IDENT = TEMP_IDENT$
		SCOPE::PRG_ITEM = TEMP_ITEM$

		SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

		SELECT JUNK%
		!
		! Exit
		!
		CASE 3%, SMG$K_TRM_F8, SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

			POST_STATUS% = (POST_STATUS% OR 4%)

		!
		! Normal key typed
		!
		CASE 0%, 10%, 12%, 13%, SMG$K_TRM_DO

		!
		! Bad key typed
		!
		CASE ELSE
			SCOPE::SCOPE_EXIT = JUNK%
			CALL ENTR_3BADKEY(SCOPE, SCOPE::SCOPE_EXIT)
			GOTO 1230

		END SELECT

		CALL ENTR_3MESSAGE(SCOPE, "Creating posting transmittals", 1%)

	!
	! In the forth pass, we will put all the records to the file
	!
	CASE 4%
		!
		! If this is an accrual payroll and the final date
		! is not the same as the accrual date
		! then write the data to the end date.
		!
		SELECT FILE_NAME$

		CASE "PAY"

			!
			! Don't flag "A" records since they aren't
			! posted until the final post.
			!
			IF (PR_TRN_PAY::PTYPE <> "A") OR (UPDATE_FLAG% <> 2%)
			THEN
				PR_TRN_PAY::UPDATE_FLAG = &
					(PR_TRN_PAY::UPDATE_FLAG OR &
					UPDATE_FLAG%)
			END IF

			IF UPDATE_FLAG% = 4%
			THEN
				PR_TRN_PAY::UPDATE_FLAG = &
					(PR_TRN_PAY::UPDATE_FLAG OR 2%)
			END IF

			PR_TRN_PAY::BATCH = BATCHNO$
			THIS_EMP$ = PR_TRN_PAY::EMPNUM

		CASE "DED"

			PR_TRN_DED::UPDATE_FLAG = &
				(PR_TRN_DED::UPDATE_FLAG OR UPDATE_FLAG%)
			PR_TRN_DED::BATCH = BATCHNO$

			IF UPDATE_FLAG% = 4%
			THEN
				PR_TRN_DED::UPDATE_FLAG = &
					(PR_TRN_DED::UPDATE_FLAG OR 2%)
			END IF
			THIS_EMP$ = PR_TRN_DED::EMPNUM

		CASE "CHECK"

			PR_TRN_CHECK::UPDATE_FLAG = &
				(PR_TRN_CHECK::UPDATE_FLAG OR UPDATE_FLAG%)
			PR_TRN_CHECK::BATCH = BATCHNO$

			IF UPDATE_FLAG% = 4%
			THEN
				PR_TRN_CHECK::UPDATE_FLAG = &
					(PR_TRN_CHECK::UPDATE_FLAG OR 2%)
			END IF
			THIS_EMP$ = PR_TRN_CHECK::EMPNUM

		END SELECT

		GOTO 1360 IF UPDATE_FLAG% <> 2% OR PR_FINAL.CH% = 0%

1320		!
		! As simple as that
		!
		WHEN ERROR IN
			PUT #PR_FINAL.CH%
		USE
			CALL ENTR_3MESSAGE(SCOPE, &
				"Error in adding " + FILE_NAME$ + &
				" Employee: " + THIS_EMP$ + " " + &
				ERT$(ERR), 0%)
			EXIT HANDLER
		END WHEN

1350		!
		! Put record into UTL_BATCH file in case of crash
		!
		UTL_BATCH::FILE_NAME = FILE_NAME$

		UTL_BATCH::RFA_RFA = GETRFA(PR_FINAL.CH%)

		PUT #UTL_BATCH.CH%

1360		!
		! Change the update_flag to show that this record
		! has been posted.
		!
		TEMP.CH% = PR_FINAL.CH%
		TEMP.CH% = PR_ACCRUAL.CH% IF UPDATE_FLAG% = 2%

		UPDATE #TEMP.CH%

	END SELECT

 ExitSubroutine:

	EXIT SUB

	%PAGE

 Interupt:
	!********************************************************************
	! Handle any special junk in RRR_FLAG%
	!********************************************************************
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
			CALL HELP_3MESSAGE(SCOPE, "", SCOPE::PRG_IDENT, &
				SCOPE::PRG_PROGRAM, SCOPE::PRG_ITEM)
			SMG_STATUS% = SMG$SET_CURSOR_MODE(SCOPE::SMG_PBID, 1%)

		!
		! Interupt
		!
		CASE SMG$K_TRM_F6, SMG$K_TRM_F20
			SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT( &
				SCOPE::SMG_PBID)

			CALL MENU_3INTERRUPT(SCOPE)

			SMG_STATUS% = SMG$ENABLE_UNSOLICITED_INPUT( &
				SCOPE::SMG_PBID, &
				LOC(OUTP_XUNSOL) BY VALUE, &
				LOC(SCOPE::SMG_KBID) BY VALUE)

		!
		! Exit Not allowed
		!

	END SELECT

	RRR_FLAG% = 0%

	RETURN

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	ON ERROR GO BACK

32767	END SUB
