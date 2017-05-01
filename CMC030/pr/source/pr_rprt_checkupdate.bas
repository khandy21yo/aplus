1	%TITLE "STATUS - Folder Status Report"
	%SBTTL "PR_RPRT_CHECKUPDATE"
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
	! ID:PR050
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_CHECKUPDATE/LINE
	!	$ LINK/EXECUTABLE=PR_EXE: PR_RPRT_CHECKUPDATE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_CHECKUPDATE.OBJ;*
	!
	! Author:
	!
	!	05/07/91 - J. Shad Rydalch
	!
	! Modification history:
	!
	!	05/09/91 - Kevin Handy
	!		Modified to have "xxxxNOTSET%" instead of using
	!		"NOT xxxSET%".
	!
	!	05/09/91 - Kevin Handy
	!		Modified to handle HIS files as well as TRN files.
	!
	!	05/13/92 - Kevin Handy
	!		Fixed "A" and "F" being reversed. (Oops)
	!
	!	11/30/92 - Kevin Handy
	!		Fixed bug in TRN/HIS testing.
	!
	!	04/11/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Removed unsolicited_input stuff.
	!
	!	09/10/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/04/2000 - Kevin Handt
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

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.HB"
	MAP (PR_TRN_CHECK)	PR_TRN_CHECK_CDD	PR_TRN_CHECK
	MAP (PR_HIS_CHECK)	PR_TRN_CHECK_CDD	PR_HIS_CHECK

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD		PR_TRN_PAY
	MAP (PR_HIS_PAY)	PR_TRN_PAY_CDD		PR_HIS_PAY

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.HB"
	MAP (PR_TRN_DED)	PR_TRN_DED_CDD		PR_TRN_DED
	MAP (PR_HIS_DED)	PR_TRN_DED_CDD		PR_HIS_DED

	DECLARE INTEGER CONSTANT FILE_MAX = 2000

	DIM DATA_FILE$(FILE_MAX)

	%PAGE

	ON ERROR GOTO 19000

	DEF STRING SETFLAGS(SETFLAG%)
	!
	! Use set flags to create a string to print with information
	! that is easyer to understand.
	!
	ST$ = ""

	ST$ = ST$ + "U" IF 1% AND SETFLAG%
	ST$ = ST$ + "A" IF 2% AND SETFLAG%
	ST$ = ST$ + "F"	IF 4% AND SETFLAG%

	SETFLAGS = ST$ + SPACE$(3% - LEN(ST$))

	END DEF

 Init:	!
	! Initilize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	FROM_BATCH_NO$ = DATE_STOREDATE(FROM_BATCH_NO$) ! Reformat to (YYYYMMDD)

	!++
	! Abstract:FLD01
	!	^*(01) From Folder\*
	!	.b
	!	.lm +5
	!	The ^*From Folder\* field contains the date of
	!	the payroll folder with which the report will begin printing.
	!	.b
	!	A blank field will cause the report to start
	!	with the first payroll folder date in the file.
	!	.lm -5
	!
	! Index:
	!	.x Start Payroll Date>Employee Check Audit Report
	!	.x Employee Check Audit Report>Start Payroll Date
	!
	!--

	TO_BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_BATCH_NO$ = DATE_STOREDATE(TO_BATCH_NO$) ! Reformat to (YYYYMMDD)

	!++
	! Abstract:FLD02
	!	^*(02) To Folder\*
	!	.b
	!	.lm +5
	!	The ^*To Folder\* field is to contain the date of
	!	the payroll folder with which the report is to end printing.
	!	.b
	!	A blank field will cause the report to end
	!	with the last payroll folder date in the file.
	!	.lm -5
	!
	! Index:
	!	.x End Payroll Date>Employee Check Audit Report
	!	.x Employee Check Audit Report>End Payroll Date
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

	%PAGE

 ReportTitle:
 !	SMG_STATUS% = SMG$DISABLE_UNSOLICITED_INPUT(SCOPE::SMG_PBID)

	!
	! Set up titles
	!
	TITLE$(1%) = "Payroll Check Update"
	TITLE$(2%) = "For the Payroll Folders Dated From: " + &
		PRNT_DATE(FROM_BATCH_NO$, 8%) + &
		" To: " + PRNT_DATE(TO_BATCH_NO$, 8%)
	TITLE$(3%) = ""
	!
	! Define headers
	!
	TITLE$(4%) = "            --Check--------       --Ded----------" + &
			"       --Pay----------"
	TITLE$(5%) = "Folder      Set NotSet  #RC       Set NotSet  #RC" + &
			"       Set NotSet  #RC"
	TITLE$(6%) = ""

	!	     --Check--------       --Ded----------       --Pay----------"
	!Folder      Set NotSet  #RC       Set NotSet  #RC       Set NotSet  #RC"
	!12/34/5678  AUF AUF    ####       AUF AUF    ####       AUF AUF    ####"

	!
	! Set up line layouts
	!
	LYT_LINE$ = "DFolder:11,$CS:016,$CN:020,VCR:28" + &
		"$DS:033,$DN:037,VDR:045,$PS:050,$PN:054,VPR:62"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	FOR PR_LOOP% = 1% TO DATA_FILE%

		CHECKSET% = 0%
		DEDSET% = 0%
		PAYSET% = 0%
		CHECKNOTSET% = 0%
		DEDNOTSET% = 0%
		PAYNOTSET% = 0%
		NUMCHECKREC% = 0%
		NUMDEDREC% = 0%
		NUMPAYREC% = 0%

		BATCH_NO$ = DATA_FILE$(PR_LOOP%)
		CALL ENTR_3MESSAGE(SCOPE, "Processing: " + BATCH_NO$, 1%)

17300		!
		! gather up info on CHECK folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_CHECK.OPN"

			RESET #PR_TRN_CHECK.CH%
		USE
			CONTINUE 17360 IF ERR = 5%
			FILENAME$ = "PR_TRN_CHECK"
			CONTINUE HelpError
		END WHEN

 CheckLoop:	!
		! Get next record
		!
17310		WHEN ERROR IN
			GET #PR_TRN_CHECK.CH%, REGARDLESS
		USE
			CONTINUE 17360 IF ERR = 11%
			FILENAME$ = "PR_TRN_CHECK"
			CONTINUE HelpError
		END WHEN

		NUMCHECKREC% = NUMCHECKREC% + 1%

		CHECKSET% = CHECKSET% OR PR_TRN_CHECK::UPDATE_FLAG
		CHECKNOTSET% = CHECKNOTSET% OR NOT PR_TRN_CHECK::UPDATE_FLAG

		GOTO CheckLoop

17360		CLOSE PR_TRN_CHECK.CH%
		CALL ASSG_FREECHANNEL(PR_TRN_CHECK.CH%)

		!
		! gather up info on CHECK folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_CHECK.OPN"

			RESET #PR_HIS_CHECK.CH%
		USE
			CONTINUE 17380 IF ERR = 5%
			FILENAME$ = "PR_HIS_CHECK"
			CONTINUE HelpError
		END WHEN

 CheckHisLoop:	!
		! Get next record
		!
17370		WHEN ERROR IN
			GET #PR_HIS_CHECK.CH%, REGARDLESS
		USE
			CONTINUE 17380 IF ERR = 11%
			FILENAME$ = "PR_HIS_CHECK"
			CONTINUE HelpError
		END WHEN

		NUMCHECKREC% = NUMCHECKREC% + 1%

		CHECKSET% = CHECKSET% OR PR_HIS_CHECK::UPDATE_FLAG
		CHECKNOTSET% = CHECKNOTSET% OR NOT PR_HIS_CHECK::UPDATE_FLAG

		GOTO CheckHisLoop

17380		CLOSE PR_HIS_CHECK.CH%
		CALL ASSG_FREECHANNEL(PR_HIS_CHECK.CH%)


17400		!
		! gather up info on DED folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_DED.OPN"

			RESET #PR_TRN_DED.CH%
		USE
			CONTINUE 17460 IF ERR = 5%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

 DedLoop:	!
		! Get next record
		!
17410		WHEN ERROR IN
			GET #PR_TRN_DED.CH%, REGARDLESS
		USE
			CONTINUE 17460 IF ERR = 11%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

		NUMDEDREC% = NUMDEDREC% + 1%

		DEDSET% = DEDSET% OR PR_TRN_DED::UPDATE_FLAG
		DEDNOTSET% = DEDNOTSET% OR NOT PR_TRN_DED::UPDATE_FLAG

		GOTO DedLoop

17460		CLOSE PR_TRN_DED.CH%
		CALL ASSG_FREECHANNEL(PR_TRN_DED.CH%)

		!
		! gather up info on DED folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_DED.OPN"

			RESET #PR_HIS_DED.CH%
		USE
			CONTINUE 17480 IF ERR = 5%
			FILENAME$ = "PR_HIS_DED"
			CONTINUE HelpError
		END WHEN

 DedHisLoop:	!
		! Get next record
		!
17470		WHEN ERROR IN
			GET #PR_HIS_DED.CH%, REGARDLESS
		USE
			CONTINUE 17480 IF ERR = 11%
			FILENAME$ = "PR_TRN_DED"
			CONTINUE HelpError
		END WHEN

		NUMDEDREC% = NUMDEDREC% + 1%

		DEDSET% = DEDSET% OR PR_HIS_DED::UPDATE_FLAG
		DEDNOTSET% = DEDNOTSET% OR NOT PR_HIS_DED::UPDATE_FLAG

		GOTO DedHisLoop

17480		CLOSE PR_TRN_DED.CH%
		CALL ASSG_FREECHANNEL(PR_TRN_DED.CH%)

17500		!
		! gather up info on PAY folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"

			RESET #PR_TRN_PAY.CH%
		USE
			CONTINUE 17560 IF ERR = 5%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

 PayLoop:	!
		! Get next record
		!
17510		WHEN ERROR IN
			GET #PR_TRN_PAY.CH%, REGARDLESS
		USE
			CONTINUE 17560 IF ERR = 11%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		NUMPAYREC% = NUMPAYREC% + 1%

		PAYSET% = PAYSET% OR PR_TRN_PAY::UPDATE_FLAG
		PAYNOTSET% = PAYNOTSET% OR NOT PR_TRN_PAY::UPDATE_FLAG

		GOTO PayLoop

17560		CLOSE PR_TRN_PAY.CH%
		CALL ASSG_FREECHANNEL(PR_TRN_PAY.CH%)

		!
		! gather up info on PAY folder
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
			RESET #PR_HIS_PAY.CH%
		USE
			CONTINUE 17580 IF ERR = 5%
			FILENAME$ = "PR_HIS_PAY"
			CONTINUE HelpError
		END WHEN

 PayHisLoop:	!
		! Get next record
		!
17570		WHEN ERROR IN
			GET #PR_HIS_PAY.CH%, REGARDLESS
		USE
			CONTINUE 17580 IF ERR = 11%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		NUMPAYREC% = NUMPAYREC% + 1%

		PAYSET% = PAYSET% OR PR_HIS_PAY::UPDATE_FLAG
		PAYNOTSET% = PAYNOTSET% OR NOT PR_HIS_PAY::UPDATE_FLAG

		GOTO PayHisLoop

17580		CLOSE PR_HIS_PAY.CH%
		CALL ASSG_FREECHANNEL(PR_HIS_PAY.CH%)

17590		!
		! Print out the stuff we found
		!
		TEXT$ = PRNT_DATE(BATCH_NO$, 8%) + "  " + &
			SETFLAGS(CHECKSET%) + " " + &
			SETFLAGS(CHECKNOTSET%) + "    " + &
			FORMAT$(NUMCHECKREC%, "####") + "       " + &
			SETFLAGS(DEDSET%) + " " + &
			SETFLAGS(DEDNOTSET%) + "    " + &
			FORMAT$(NUMDEDREC%, "####") + "       " + &
			SETFLAGS(PAYSET%) + " " + &
			SETFLAGS(PAYNOTSET%) + "    " + &
			FORMAT$(NUMPAYREC%, "####")

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOTO ExitProgram IF UTL_REPORTX::STAT

	NEXT PR_LOOP%

 ExitTotal:
17600	!
	! Handle end of report
	!

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
