1	%TITLE "Form Feed to a Report"
	%SBTTL "OUTP_NEWPAGE"
	%IDENT "V3.6a Calico"

	SUB OUTP_NEWPAGE(UTL_REPORTX_CDD UTL_REPORTX)

	!
	!	COPYRIGHT (C) 1987 BY
	!	Computer Management Center, Idaho Falls, Idaho.
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
	!	.p
	! Parameters:
	!
	!	UTL_REPORTX
	!		The report that the form feed is sent to.
	!
	!	It returns a form feed to the report of the programmer's choice.
	!
	! Example:
	!
	!	CALL OUTP_NEWPAGE(UTL_REPORTX)
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_NEWPAGE/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP OUTP_NEWPAGE
	!	$ DELETE OUTP_NEWPAGE.OBJ;*
	!
	! AUTHOR:
	!
	!	04/27/87 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!	04/06/89 - Kevin Handy
	!		Modified handling of escape sequences
	!
	!	02/21/92 - Kevin Handy
	!		Modified to allow form feeds to printer port
	!		(requires change in READ_INITIALIZE of this
	!		same date to stop translation of form feeds
	!		to line feeds).
	!
	!	02/25/92 - Kevin Handy
	!		Modified to increment page number, and set line
	!		number to zero.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	09/01/95 - Kevin Handy
	!		Modified so that function does nothing if we
	!		are displaying on the screen.
	!
	!	10/20/95 - Kevin Handy
	!		Put into sharable library.
	!
	!	08/14/96 - Kevin Handy
	!		Lose extra '&' before 'end if'
	!
	!	11/11/97 - Kevin Handy
	!		Use constants instead of hard coded numbers
	!		in comparison to PRINTTO
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	%PAGE

	!
	! Ignore if displaying on the screen
	!
	IF UTL_REPORTX::PRINTTO <> OUTP_TODISPLAY
	THEN
		IF UTL_REPORTX::PRINTTO = OUTP_TOLOCAL
		THEN
			CALL WRIT_STRING(UTL_REPORTX::TOLOCAL, TOLOCAL$)
			PRINT #UTL_REPORTX::CHAN, TOLOCAL$;
		END IF

		IF UTL_REPORTX::NEXTPAGE = ""
		THEN
			!
			! Must print to a local printer
			!
			PRINT #UTL_REPORTX::CHAN &
				FOR I% = UTL_REPORTX::LINENO TO &
				UTL_REPORTX::PAGELEN - 1%
		ELSE
			!
			! Form feed everywhere else
			!
			CALL WRIT_STRING(UTL_REPORTX::NEXTPAGE, NEXTPAGE$)
			PRINT #UTL_REPORTX::CHAN, NEXTPAGE$;
		END IF

		IF UTL_REPORTX::PRINTTO = OUTP_TOLOCAL
		THEN
			CALL WRIT_STRING(UTL_REPORTX::TOSCREEN, TOSCREEN$)
			PRINT #UTL_REPORTX::CHAN, TOSCREEN$;
		END IF

		UTL_REPORTX::LINENO = 0%
		UTL_REPORTX::PAGENO = UTL_REPORTX::PAGENO + 1%
	END IF

	END SUB
