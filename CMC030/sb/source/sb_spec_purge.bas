1	%TITLE "Sub Account Purge Program"
	%SBTTL "SB_SPEC_PURGE"
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
	! ID:SB0002
	!
	! Abstract:HELP
	!	.p
	!	The ^*Sub Account Purge Program\* purges the Balance,
	!	Budget, and SubAccount files by subject and date.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SB_SOURCE:SB_SPEC_PURGE/LINE
	!	$ LINK/EXECUTABLE=SB_EXE: SB_SPEC_PURGE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SB_SPEC_PURGE.OBJ;*
	!
	! Author:
	!
	!	05/26/89 - B. Craig Larsen
	!
	! Modification history:
	!
	!	03/03/92 - Kevin Handy
	!		Changed "CMC$WARNING" to "CMC$_WARNING".
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/16/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
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

	!
	! Declare internal variables
	!
	DECLARE UTL_REPORTX_CDD		UTL_REPORTX

	!
	! External functions
	!
	EXTERNAL	LONG	FUNCTION ASSG_POSTBATCH
	EXTERNAL	LONG	FUNCTION SB_TRAN_PURGE

	DECLARE LONG	EXIT_STATUS
	DECLARE LONG	INTR_STATUS

	DECLARE STRING	TITLE(10%)
	DECLARE STRING	BATCH_NUMBER
	DECLARE STRING	SB.INTER.PERIOD

	%PAGE

 Init:	!==============================================================
	! OPEN THE PRINT CONTROL FILE
	!==============================================================

	!
	! Set user defined fields
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	TITLE(1%) = "SUBACCOUNT PURGE PROGRAM"
	TITLE(2%) = "Sub Account System"
	TITLE(3%) = ""

	TITLE(4%) = "."


	INTR_STATUS = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "SB_BALANCE", "SB", &
		SB.INTER.PERIOD, "")

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
	! Open up batch control file and get a batch number
	!
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "SB_BALANCE", "SB", &
		ICPERIOD, GLPERIOD) <> CMC$_NORMAL

	EXIT_STATUS = SB_TRAN_PURGE(OPT_OPENFILE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX)

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

	GOTO Aborted IF SB_TRAN_PURGE(OPT_DELETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX) <> CMC$_NORMAL

	!******************************************************************
	! Close period
	!******************************************************************

	GOTO Aborted IF SB_TRAN_PURGE(OPT_CLOSEFILE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX) <> CMC$_NORMAL


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

	EXIT_STATUS = SB_TRAN_PURGE(OPT_REPORT, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX)

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

	%Page

 ! HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
 !	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
 !		"E", ERN$, FILENAME$, NUM1$(ERR))
 !	UTL_REPORTX::STAT = -1%
 !	GOTO ExitProgram

32767	END
	!+-+-+
	!++
	! Abstract:FLD01
	!	^*(01) Subject\*
	!	.p
	!	The ^*Subject\* field specifies a subject which will be
	!	purged from the records.  Leaving this field blank purges all subjects.
	!
	! Index:
	!	.x Subject
	!
	!--
	!+-+-+
	!++
	! Abstract:FLD02
	!	^*(02) Limit Date\*
	!	.p
	!	The ^*Limit Date\* field enters a date which limits the
	!	purging process.  Only those records which qualify though the date will be
	!	purged from the system.
	!
	! Index:
	!	.x Limit Date
	!	.x Date>Limit
	!
	!--
