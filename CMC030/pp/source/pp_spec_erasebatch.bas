1	%TITLE "Erase a Posted Batch"
	%SBTTL "PP_SPEC_ERASEBATCH"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
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
	!	.b
	!	.lm +5
	!	This program is used to erase a batch in the PP MONTHLY
	!	file.
	!	.lm -5
	!
	! Index:
	!	.x Erase>Posted Batch
	!	.x Posted Batch>Erase
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_SPEC_ERASEBATCH/LINE
	!	$ LINK/EXE=PP_EXE: PP_SPEC_ERASEBATCH, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_SPEC_ERASEBATCH.OBJ;*
	!
	! Author:
	!
	!	12/20/93 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE's
	!
	!	10/06/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!******************************************************************
	! External modules needed
	!******************************************************************

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs)
	!******************************************************************

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[PP.OPEN]PP_MONTHLY.HB"
	MAP	(PP_MONTHLY)	PP_MONTHLY_CDD	PP_MONTHLY

	%PAGE

	!******************************************************************
	! Take care of anything else before starting the report
	!******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Initialization:
	!******************************************************************
	! Get ready to begin
	!******************************************************************

	!
	! Initialize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GL_BATCH$ = LEFT(EDIT$(UTL_REPORTX::OPTDEF(0%), 132%), 6%)

	!++
	! Abstract:FLD01
	!	^*(01) Process Batch Number\*
	!	.b
	!	.lm +5
	!	The ^*Process Batch Number\* enters the number of the
	!	batch which is to be removed.
	!	.lm -5
	!
	! Index:
	!
	!--
	YYYY_PP$ = TRM$(UTL_REPORTX::OPTDEF(2%))

	!++
	! Abstract:FLD03
	!	^*(03) Accounting Period\*
	!	.b
	!	.lm +5
	!	The ^*Account Period\* enters the period from which
	!	the batch will be removed.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!	.x Accounting Period
	!	.x Period>Accounting
	!
	!--

	%PAGE

300	!******************************************************************
	! Open all files
	!******************************************************************

	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_MONTHLY.MOD"
	USE
		FILENAME$ = "PP_MONTHLY"
		CONTINUE HelpError
	END WHEN

	WHEN ERROR IN
		FIND #PP_MONTHLY.CH%, KEY #3% EQ GL_BATCH$
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 320 IF ERR = 11%
		FILENAME$ = "PP_MONTHLY"
		CONTINUE HelpError
	END WHEN

	TOTAL_TRAN% = 0%

310	WHEN ERROR IN
		GET #PP_MONTHLY.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 320 IF ERR = 11%
		FILENAME$ = "PP_MONTHLY"
		CONTINUE HelpError
	END WHEN

	GOTO 320 IF PP_MONTHLY::BATCH <> GL_BATCH$

	DELETE #PP_MONTHLY.CH%

	TOTAL_TRAN% = TOTAL_TRAN% + 1%

	GOTO 310

320	CLOSE #PP_MONTHLY.CH%

	%PAGE

 ReportTitle:
	!
	! Titles
	!
	TITLE$(1%) = "PP Erase Transmittal"
	TITLE$(2%) = "From Period " + YYYY_PP$
	TITLE$(3%) = "Batch #" + GL_BATCH$
	TITLE$(4%) = ""
	TITLE$(5%) = "."

	!
	! Headers
	!
	TITLE$(6%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

 GetNextRec:
17100	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	TOTAL_BATCH% = 0%

	TEXT$ = GL_BATCH$ + "   Eeased, " + &
		NUM1$(TOTAL_TRAN%) + " records"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitTotal

	%PAGE

17900	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:


 ExitProgram:
	!
	! Finish up the report
	!
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
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report GL_SPEC_MOVEBATCH
	!******************************************************************
	END
