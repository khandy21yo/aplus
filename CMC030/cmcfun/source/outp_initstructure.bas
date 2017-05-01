1	%TITLE "Initilize Report Structure from Report"
	%SBTTL "OUTP_INITSTRUCTURE"
	%IDENT "V3.6a Calico"

	SUB OUTP_INITSTRUCTURE(UTL_REPORT_CDD UTL_REPORT, &
		UTL_REPORTX_CDD UTL_REPORTX)

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
	!	Initilizes the UTL_REPORTX structure from the
	!	UTL_REPORT record.
	!
	! Index:
	!
	! Parameters:
	!
	!	UTL_REPORT
	!		The passed file the UTL_REPORTX structure is from.
	!
	!	UTL_REPORTX
	!		The returned file that is being created by the UTL_REPORT
	!		file.
	!
	! Example:
	!
	!	CALL OUTP_INITSTRUCTURE(UTL_REPORT_CDD UTL_REPORT, &
	!		UTL_REPORTX_CDD UTL_REPORTX)
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_INITSTRUCTURE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP OUTP_INITSTRUCTURE
	!	$ DELETE OUTP_INITSTRUCTURE.OBJ;*
	!
	! AUTHOR:
	!
	!	05/07/87 - Lance Williams
	!
	! MODIFICATION HISTORY:
	!
	!	03/13/95 - Kevin Handy
	!		Added initialization for new fields:
	!		AFTERTIME, BACKGROUND, OFFSET.
	!
	!	09/09/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	11/11/97 - Kevin Handy
	!		Use VAL%(PRINTTO$) instead of trying to look
	!		up the number in a table.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	01/25/99 - Kevin Handy
	!		Add an error trap for an illegal number
	!
	!	06/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	%PAGE

	UTL_REPORTX::REPNUM	= UTL_REPORT::REPNUM
	UTL_REPORTX::PRODEV	= UTL_REPORT::PRODEV
	UTL_REPORTX::PRONAM	= UTL_REPORT::PRONAM
	UTL_REPORTX::SPOOL	= UTL_REPORT::SPOOL
	UTL_REPORTX::SPOOLFORM	= UTL_REPORT::SPOOLFORM
	UTL_REPORTX::OPTDEF(I%)	= UTL_REPORT::OPTDEF(I%) FOR I% = 0% TO 9%

	OUTPUT$	= UTL_REPORT::DEFOUT
	TEMP% = INSTR(1%, OUTPUT$, " ")
	IF TEMP%
	THEN
		PRINTTO$ = TRM$(RIGHT(UTL_REPORT::DEFOUT, TEMP% + 1%))
		OUTPUT$ = LEFT(UTL_REPORT::DEFOUT, TEMP% - 1%)
	END IF

	UTL_REPORTX::DEFOUT	= OUTPUT$
	UTL_REPORTX::ITEMGROUP(I%) = "" FOR I% = 0% TO 9%
	UTL_REPORTX::ITEM(I%)	= "" FOR I% = 0% TO 9%
	UTL_REPORTX::STARTP	= 0%
	UTL_REPORTX::ENDP	= 0%
	UTL_REPORTX::COPIES	= 0%
	UTL_REPORTX::PAGELEN	= 66%
	UTL_REPORTX::REPWIDTH	= 80%
	UTL_REPORTX::REPYN	= UTL_REPORT::REPYN
	UTL_REPORTX::REPDATE	= PRNT_FANCYDATE(DATE_TODAY)

	UTL_REPORTX::PRINTTO	= 0%

5000	IF PRINTTO$ <> ""
	THEN
		WHEN ERROR IN
			UTL_REPORTX::PRINTTO = VAL%(PRINTTO$)
		USE
			UTL_REPORTX::PRINTTO = 0%
		END WHEN
	END IF

5010	UTL_REPORTX::AUTOSCROLL	= 0%
	UTL_REPORTX::PRINTTYPE	= UTL_REPORT::PRINTTYPE
	UTL_REPORTX::PRINTINIT	= ""
	UTL_REPORTX::PRINTFINISH= ""
	UTL_REPORTX::NEXTPAGE	= ""
	UTL_REPORTX::TOLOCAL	= "/027[5i"
	UTL_REPORTX::TOSCREEN	= "/027[4i"
	UTL_REPORTX::OFFSET	= 0%
	UTL_REPORTX::AFTERTIME	= ""
	UTL_REPORTX::BACKGROUND = "N"

32767	END SUB
