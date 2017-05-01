1	%TITLE "Look Up All Detail Files for Payroll"
	%SBTTL "PR_FIND_DETAILFILE"
	%IDENT "V3.6a Calico"

	SUB PR_FIND_DETAILFILE(FROM_BATCH_NO$, &
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
	!	Index
	!	Function
	!	Inputs
	!	START_DATE$ = YYYYYMMDD
	!	END_DATE$ = YYYYYMMDD
	!	PR_TRN_PAY.DEV$ = {NODE}::{DEVICE}:{[USER]}
	!	PR_HIS_PAY.DEV$ = {NODE}::{DEVICE}:{[USER]}
	!
	! Index:
	!
	! Option:
	!
	!
	! Outputs:
	!
	!	DATA_FILE$() = List for files with in date ranges
	!
	! Example:
	!
	!	SUB PR_FIND_DETAILFILE("19880301", &
	!		"19880331", &
	!		"$DISK3:", &
	!		"$DISK3:", &
	!		DATA_FILE$())
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_FIND_DETAILFILE/NOLINE/NOOPT
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP PR_FIND_DETAILFILE
	!	$ DELETE PR_FIND_DETAILFILE.OBJ;*
	!
	! Author:
	!
	!	04/11/88 - Robert Peterson
	!
	! Modification history:
	!
	!	11/07/89 - Kevin Handy
	!		Fixed bug wherin if there was both a journal
	!		and a history file with the same date,
	!		the lookup would stop (no errors).
	!
	!	12/28/89 - Frank F. Starman
	!		In WHILE statement change AND to OR
	!
	!	12/28/89 - Kevin Handy
	!		Modified for sharable library.
	!
	!	07/17/90 - Frank F. Starman
	!		Increase dimension for ARC files and
	!		dicrease for JRL.
	!
	!	01/27/92 - Kevin Handy
	!		Increased dimension for JRL files from
	!		61 to 101
	!
	!	10/25/92 - Kevin Handy
	!		Increased dim from 101 to 201.
	!
	!	08/10/93 - Kevin Handy
	!		Increased dimension from 601 to 1001 for arc files.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/20/95 - Kevin Handy
	!		Increase dimension for ARC_FILE$() from 1000 to 2000
	!		for DWI.
	!
	!	03/03/97 - Kevin Handy
	!		Increase dimension from 2001 to 3001.
	!
	!	03/10/97 - Kevin Handy
	!		Clean up/speed up code while looking for
	!		a subscript problem.
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/21/99 - Kevin Handy
	!		Lose error trapping (just a resume to the previous
	!		level), so that calling programs won't be as easily
	!		confused by an unexpected error.
	!
	!	06/30/99 - Kevin Handy
	!		Compile with /NOOPT to lose problems on Alpha
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Dimension
	!
	DIM JRL_FILE$(201%), ARC_FILE$(3001%)

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
	!
	ARC_FILE% = ARC_FILE% + 1%
	JRL_FILE% = JRL_FILE% + 1%
	ARC_FILE$(ARC_FILE%), JRL_FILE$(JRL_FILE%) = '255'C

	DATA_FILE% = 0%
	ARC_LOOP%, JRL_LOOP% = 1%

	WHILE (ARC_LOOP% <> ARC_FILE%) OR (JRL_LOOP% <> JRL_FILE%)

		IF ARC_FILE$(ARC_LOOP%) <= JRL_FILE$(JRL_LOOP%)
		THEN
			IF FROM_BATCH_NO$ <= ARC_FILE$(ARC_LOOP%) AND &
				ARC_FILE$(ARC_LOOP%) <= TO_BATCH_NO$
			THEN
				DATA_FILE% = DATA_FILE% + 1%
				DATA_FILE$(DATA_FILE%) = ARC_FILE$(ARC_LOOP%)
			END IF

			JRL_LOOP% = JRL_LOOP% + 1% &
				IF (ARC_FILE$(ARC_LOOP%) = JRL_FILE$(JRL_LOOP%))
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

	END SUB
