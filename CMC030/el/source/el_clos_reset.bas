1	%TITLE "Equipment Ledger Reset Program"
	%SBTTL "EL_CLOS_RESET"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	! ID:EL0008
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Equipment Ledger Reset Program\*
	!	resets the last closed period of the Equipment Ledger back one
	!	period.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS EL_SOURCE:EL_CLOS_RESET/LINE
	!	$ LINK/EXECUTABLE=EL_EXE: EL_CLOS_RESET, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE EL_CLOS_RESET.OBJ;*
	!
	! Author:
	!
	!	10/14/92 - Dan Perkins
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/04/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/21/2000 - Kevin Handy
	!		Lose useless error trapping
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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

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
	TITLE(1%) = "EQUIPMENT  LEDGER  RESET  PROTOCOL"
	TITLE(2%) = "Equipment Ledger System"
	TITLE(3%) = ""
	TITLE(4%) = "."

	!******************************************************************
	! Check if process has been interrupted
	!******************************************************************

	!
	! Open up batch control file and check if interrupted
	!
	INTR_STATUS = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "SB_BALANCE", "EL", SB.INTER.PERIOD, "")

	SELECT INTR_STATUS

	CASE CMC$_NORMAL

	CASE CMC$_WARNING

	CASE ELSE
		GOTO Aborted
	END SELECT

	!******************************************************************
	! Assign batch number and open control files
	!******************************************************************
	!
	EXIT_STATUS = SB_TRAN_RESET(OPT_OPENFILE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "EL")

	SELECT EXIT_STATUS
	CASE CMC$_NORMAL

	CASE CMC$_WARNING

	CASE ELSE
		GOTO Aborted
	END SELECT

	GOTO Aborted IF SB_TRAN_RESET(OPT_UPDATE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "EL") <> CMC$_NORMAL

	!******************************************************************
	! Confirm closing and close period
	!******************************************************************

	GOTO Aborted IF SB_TRAN_RESET(OPT_CONFIRM, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "EL") <> CMC$_NORMAL

	GOTO Aborted IF SB_TRAN_RESET(OPT_CLOSEFILE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "EL") <> CMC$_NORMAL

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

	EXIT_STATUS = SB_TRAN_RESET(OPT_REPORT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "EL")

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
