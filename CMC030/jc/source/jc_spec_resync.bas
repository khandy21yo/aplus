1	%TITLE "Job Costing Resynchronize Program"
	%SBTTL "JC_SPEC_RESYNC"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988, 1989 BY
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
	! ID:JC0009
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Job Costing Resynchronize Program\* resynchronizes
	!	from the current period forward to the job costing ledger.
	!	.lm -5
	!
	! Index:
	!	.x Job Costing Resynchronize Program
	!	.x Resynchronize>Job Costing
	!	.x Program>Resynchronize
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS JC_SOURCE:JC_SPEC_RESYNC/LINE
	!	$ LINK/EXECUTABLE=JC_EXE: JC_SPEC_RESYNC, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE JC_SPEC_RESYNC.OBJ;*
	!
	! Author:
	!
	!	05/12/89 - B. Craig Larsen
	!
	! Modification history:
	!
	!	03/03/92 - Kevin Handy
	!		Changed "CMC$WARNING" to "CMC$_WARNING"
	!
	!	09/21/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/27/93 - Kevin Handy
	!		Created AbortedOne so that we can abort without
	!		crashing out with more errors in the abort
	!		section. (Wouldn't it be nice to also tell the user
	!		why it aborted?)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include constants and and some functions
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include CDD
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE		UTL_REPORTX_CDD		UTL_REPORTX

	!
	! External functions
	!
	EXTERNAL	LONG	FUNCTION ASSG_POSTBATCH
	EXTERNAL	LONG	FUNCTION SB_TRAN_RESYNC

	DECLARE LONG	EXIT_STATUS
	DECLARE LONG	INTR_STATUS

	DECLARE STRING	TITLE(10%)
	DECLARE STRING	BATCH_NUMBER

	%PAGE

 Init:	!==============================================================
	! Initialize
	!==============================================================

	!
	! Set user defined fields
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	SYSTEM$ = "JC"
	TITLE(1%) = "JOB  COSTING  RESYNCHRONIZE  PROTOCOL"
	TITLE(2%) = "Job Costing System"
	TITLE(3%) = ""

	TITLE(4%) = "."

	INTR_STATUS = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "SB_BALANCE", SYSTEM$, "", "")

	SELECT INTR_STATUS

	CASE CMC$_NORMAL

	CASE CMC$_WARNING

	CASE ELSE
		GOTO Aborted

	END SELECT

	!******************************************************************
	! Assign batch number and open files
	!******************************************************************
	!
	! Open up batch control file and get a batch number
	!
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "SB_BALANCE", SYSTEM$, "", "") <> CMC$_NORMAL

	GOTO Aborted IF SB_TRAN_RESYNC(OPT_OPENFILE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, SYSTEM$) <> CMC$_NORMAL

	!******************************************************************
	! Update balances and close control file
	!******************************************************************

	GOTO Interrupt IF SB_TRAN_RESYNC(OPT_UPDATE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, SYSTEM$) <> CMC$_NORMAL

	GOTO Interrupt IF SB_TRAN_RESYNC(OPT_CLOSEFILE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, SYSTEM$) <> CMC$_NORMAL

	!******************************************************************
	! Check undefined codes
	!******************************************************************

	GOTO AbortedOne IF SB_TRAN_RESYNC(OPT_CHECK, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, SYSTEM$) <> CMC$_NORMAL

	!******************************************************************
	! Complete process
	!******************************************************************

	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "", "", "")

 ExitProgram:
	!******************************************************************
	! Exit normally
	!******************************************************************

	EXIT_STATUS = SB_TRAN_RESYNC(OPT_REPORT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, SYSTEM$)

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

 Aborted:
	!******************************************************************
	! Abort process
	!******************************************************************

	IF INTR_STATUS <> CMC$_WARNING
	THEN
		GOTO Interrupt IF SB_TRAN_RESYNC(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, SYSTEM$) <> CMC$_NORMAL

		EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, TITLE(), &
			UTL_REPORTX, "", "", "", "")

		GOTO ExitProgram
	END IF

 AbortedOne:
	!******************************************************************
	! Abort process
	!******************************************************************

	IF INTR_STATUS <> CMC$_WARNING
	THEN
		EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, TITLE(), &
			UTL_REPORTX, "", "", "", "")

		GOTO ExitProgram
	END IF

 Interrupt:
	!******************************************************************
	! Interrupt process
	!******************************************************************

	EXIT_STATUS = ASSG_POSTBATCH(OPT_INTERRUPT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "", "", "")

	GOTO ExitProgram

 ! HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
 !	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
 !		"E", ERN$, FILENAME$, NUM1$(ERR))
 !
 !	UTL_REPORTX::STAT = -1%
 !
 !	GOTO ExitProgram

32767	END
