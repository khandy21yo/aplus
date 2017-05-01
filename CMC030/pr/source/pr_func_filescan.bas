1	%TITLE "Look Up All Detail Files for Payroll"
	%SBTTL "PR_FUNC_FILESCAN"
	%IDENT "V3.6a Calico"

	SUB PR_FUNC_FILESCAN(FROM_BATCH_NO$, &
		TO_BATCH_NO$, &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	!
	! COPYRIGHT (C) 1986 BY
	! Computer Management Center
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This subroutine looks up all of the payroll detail files.
	!	From this list of files a range can be selected and then
	!	return in a variable to the calling program.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Inputs:
	!	START_DATE$ = YYYYYMMDD
	!	END_DATE$ = YYYYYMMDD
	!	PR_TRN_PAY.DEV$ = {NODE}::{DEVICE}:{[USER]}
	!	PR_HIS_PAY.DEV$ = {NODE}::{DEVICE}:{[USER]}
	!
	! Outputs:
	!
	!	DATA_FILE$() = List for files with in date ranges
	!
	! Example:
	!
	!	SUB PR_FUNC_FILESCAN("19880301", &
	!			"19880331", &
	!			"$DISK3:", &
	!			"$DISK3:", &
	!			DATA_FILE$())
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_FUNC_FILESCAN/NOLINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_FUNC_FILESCAN
	!	$ DELETE PR_FUNC_FILESCAN.OBJ;*
	!
	! Author:
	!
	!	04/11/88 - Robert Peterson
	!
	! Modification history:
	!
	!	07/17/90 - Frank F. Starman
	!		Increase dimension from 200 to 601 for ARC
	!		files.
	!
	!	10/06/93 - Kevin Handy
	!		Increade dimension from 601 to 1001 for arc files.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/18/95 - Kevin Handy
	!		Increased dimension from 1000 to 1500.
	!
	!	10/22/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/13/2000 - Kevin Handy
	!		Crash when errors occur, instead of confusing calling
	!		program with it's errors.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Dimension
	!
	DIM JRL_FILE$(71%), ARC_FILE$(1501%)

	%PAGE

	!
	! Look up all journal payroll files
	!
	CALL FIND_FILE(PR_TRN_PAY.DEV$ + "PR_TRN_PAY_*.JRL", JRL_FILE$(), &
		16%, "", "")
	JRL_FILE% = VAL%(JRL_FILE$(0%))

	!
	! Reformat to YYYYMMDD
	!
	JRL_FILE$(LOOP%) = MID(JRL_FILE$(LOOP%), 12%, 8%) &
		FOR LOOP% = 1% TO JRL_FILE%

	!
	! Look up all payroll files that can be archived
	!
	CALL FIND_FILE(PR_HIS_PAY.DEV$ + "PR_HIS_PAY_*.ARC", ARC_FILE$(), &
		16%, "", "")
	ARC_FILE% = VAL%(ARC_FILE$(0%))

	!
	! Reformat to YYYYMMDD
	!
	ARC_FILE$(LOOP%) = MID(ARC_FILE$(LOOP%), 12%, 8%) &
		FOR LOOP% = 1% TO ARC_FILE%

	!******************************************************************
	! Combine journal and archive together and put into order.
	! Check to determine if batch no is within range.
	!
	! add one to loop and then
	! set last record in arc and jrl to chr$(255%)
	ARC_FILE% = ARC_FILE% + 1%
	JRL_FILE% = JRL_FILE% + 1%
	ARC_FILE$(ARC_FILE%), JRL_FILE$(JRL_FILE%) = CHR$(255%)

	DATA_FILE% = 0%
	ARC_LOOP%, JRL_LOOP% = 1%

	WHILE ARC_FILE$(ARC_LOOP%) <> JRL_FILE$(JRL_LOOP%)

		IF ARC_FILE$(ARC_LOOP%) < JRL_FILE$(JRL_LOOP%)
		THEN
			IF FROM_BATCH_NO$ <= ARC_FILE$(ARC_LOOP%) AND &
				ARC_FILE$(ARC_LOOP%) <= TO_BATCH_NO$
			THEN
				DATA_FILE% = DATA_FILE% + 1%
				DATA_FILE$(DATA_FILE%) = ARC_FILE$(ARC_LOOP%)
			END IF

			ARC_LOOP% = ARC_LOOP% + 1%
		ELSE
			IF FROM_BATCH_NO$ <= JRL_FILE$(JRL_LOOP%) AND &
				JRL_FILE$(JRL_LOOP%) <= TO_BATCH_NO$
			THEN
				DATA_FILE% = DATA_FILE% + 1%
				DATA_FILE$(DATA_FILE%) = JRL_FILE$(JRL_LOOP%)
			END IF

			JRL_LOOP% = JRL_LOOP% + 1%
		END IF
	NEXT

	DATA_FILE$(0%) = NUM1$(DATA_FILE%)

	EXIT SUB

	END SUB
