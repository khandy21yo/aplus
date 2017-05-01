1	%TITLE "Job Costing Reset Program"
	%SBTTL "JC_CLOS_RESET"
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
	! ID:JC0008
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Job Costing Reset Program\* resets the last closed
	!	period of the Job Costing ledger back one period.
	!	.lm -5
	!
	! Index:
	!	.x Job Costing Reset Program
	!	.x Program>Reset>Job Costing
	!	.x Reset>Job Costing
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS JC_SOURCE:JC_CLOS_RESET/LINE
	!	$ LINK/EXECUTABLE=JC_EXE: JC_CLOS_RESET, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE JC_CLOS_RESET.OBJ;*
	!
	! Author:
	!
	!	05/12/89 - B. Craig Larsen
	!
	! Modification history:
	!
	!	03/03/92 - Kevin Handy
	!		Changed CMC$WARNING" to "CMC$_WARNING".
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/06/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/13/2000 - Kevin Handy
	!		Lose useless error traps
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

	!
	! Declare internal variables
	!
	DECLARE UTL_REPORTX_CDD		UTL_REPORTX

	!
	! External functions
	!
	EXTERNAL	LONG	FUNCTION ASSG_POSTBATCH
	EXTERNAL	LONG	FUNCTION SB_TRAN_RESET

	DECLARE LONG	EXIT_STATUS
	DECLARE LONG	INTR_STATUS

	DECLARE STRING	TITLE(10%)
	DECLARE STRING	BATCH_NUMBER

	%PAGE

 Init:	!==============================================================
	! OPEN THE PRINT CONTROL FILE
	!==============================================================

	!
	! Set user defined fields
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	!
	! Title
	!
	TITLE(1%) = "JOB  COSTING  RESET  PROTOCOL"
	TITLE(2%) = "Job Costing System"
	TITLE(3%) = ""

	!
	! Heading
	!
	TITLE(4%) = "."

	!******************************************************************
	! Check if process has been interrupted
	!******************************************************************

	!
	! Open up batch control file and check if interrupted
	!
	INTR_STATUS = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "SB_BALANCE", "JC", &
		SB.INTER.PERIOD, "")

	SELECT INTR_STATUS
	CASE CMC$_NORMAL
		!
		! Success
		!
	CASE CMC$_WARNING
		!
		! Process was interrupted
		!
	CASE ELSE
		GOTO Aborted
	END SELECT

	!******************************************************************
	! Assign batch number and open control files
	!******************************************************************
	!
	EXIT_STATUS = SB_TRAN_RESET(OPT_OPENFILE,BATCH_NUMBER,TITLE(), &
		UTL_REPORTX, "JC")

	SELECT EXIT_STATUS
	CASE CMC$_NORMAL
		!
		! Success
		!
	CASE CMC$_WARNING
		!
		! Found batch number, go for new one
		!
		!GOTO AssignBatch

	CASE ELSE
		GOTO Aborted
	END SELECT

	GOTO Aborted IF SB_TRAN_RESET(OPT_UPDATE,BATCH_NUMBER,TITLE(), &
		UTL_REPORTX, "JC") <> CMC$_NORMAL

	!******************************************************************
	! Confirm closing and close period
	!******************************************************************

	GOTO Aborted IF SB_TRAN_RESET(OPT_CONFIRM,BATCH_NUMBER,TITLE(), &
		UTL_REPORTX, "JC") <> CMC$_NORMAL

	GOTO Aborted IF SB_TRAN_RESET(OPT_CLOSEFILE,BATCH_NUMBER,TITLE(), &
		UTL_REPORTX, "JC") <> CMC$_NORMAL


	!******************************************************************
	! Complete process
	!******************************************************************

 Complete:

	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "", "", "")

 ExitProgram:
	!******************************************************************
	! Exit normally
	!******************************************************************

	EXIT_STATUS = SB_TRAN_RESET(OPT_REPORT,BATCH_NUMBER,TITLE(), &
		UTL_REPORTX, "JC")

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

	%Page

 Aborted:
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

32767	END
