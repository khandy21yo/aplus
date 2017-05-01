1	%TITLE "Batch Monitor Function"
	%SBTTL "UTL_FUNC_EXPORTREPORT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_FUNC_EXPORTREPORT(LONG EXPORT.CH, &
		UTL_REPORT_CDD UTL_REPORT)

	!
	! COPYRIGHT (C) 1998 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Generates the report settings information in an exportable
	!	ascii file, so that it is easier to send new reports to
	!	customers.
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_FUNC_EXPORTREPORT
	!	$ LIB FUNC_LIB:CMCFUN/REP UTL_FUNC_EXPORTREPORT
	!	$ DELETE UTL_FUNC_EXPORTREPORT.OBJ;*
	!
	! Author:
	!
	!	12/15/98 - Kevin Handy
	!
	! Modification history:
	!
	!	06/09/99 - Kevin Handy
	!		Lose line 19000 (Dead Code)
	!--
	%PAGE

	!
	! Define options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"

	%PAGE

 !	ON ERROR GOTO 19000

2100	!
	! Output the data
	!
	!
	! File Layout for: UTL.UTL_REPORT
	!
	! Utility Report File
	!

	PRINT #EXPORT.CH, "RN>"; TRM$(UTL_REPORT::REPNUM)
	PRINT #EXPORT.CH, "PG>"; TRM$(UTL_REPORT::PRODEV) + &
		TRM$(UTL_REPORT::PRONAM)
	PRINT #EXPORT.CH, "PD>"; UTL_REPORT::REPYN
	PRINT #EXPORT.CH, "SL>"; TRM$(UTL_REPORT::SPOOL)
	PRINT #EXPORT.CH, "OD>"; TRM$(UTL_REPORT::DEFOUT)
	PRINT #EXPORT.CH, "U1>"; TRM$(UTL_REPORT::OPTDEF(0%))
	PRINT #EXPORT.CH, "U2>"; TRM$(UTL_REPORT::OPTDEF(1%))
	PRINT #EXPORT.CH, "U3>"; TRM$(UTL_REPORT::OPTDEF(2%))
	PRINT #EXPORT.CH, "U4>"; TRM$(UTL_REPORT::OPTDEF(3%))
	PRINT #EXPORT.CH, "U5>"; TRM$(UTL_REPORT::OPTDEF(4%))
	PRINT #EXPORT.CH, "U6>"; TRM$(UTL_REPORT::OPTDEF(5%))
	PRINT #EXPORT.CH, "U7>"; TRM$(UTL_REPORT::OPTDEF(6%))
	PRINT #EXPORT.CH, "U8>"; TRM$(UTL_REPORT::OPTDEF(7%))
	PRINT #EXPORT.CH, "U9>"; TRM$(UTL_REPORT::OPTDEF(8%))
	PRINT #EXPORT.CH, "U0>"; TRM$(UTL_REPORT::OPTDEF(9%))
	PRINT #EXPORT.CH, "SF>"; TRM$(UTL_REPORT::SPOOLFORM)
	PRINT #EXPORT.CH, "PT>"; UTL_REPORT::PRINTTYPE

	PRINT #EXPORT.CH, "?SY>"; TRM$(UTL_REPORT::SYSTEM)
	PRINT #EXPORT.CH, "?SB>"; TRM$(UTL_REPORT::SUBSYS)
	PRINT #EXPORT.CH, "?RD>"; TRM$(UTL_REPORT::REPDES)
	PRINT #EXPORT.CH, "?DV>"; TRM$(UTL_REPORT::PRODEV)
	PRINT #EXPORT.CH, "?PN>"; TRM$(UTL_REPORT::PRONAM)
	PRINT #EXPORT.CH, "?CS>"; UTL_REPORT::CANSPOOL
	PRINT #EXPORT.CH, "?CD>"; UTL_REPORT::CANDISP
	PRINT #EXPORT.CH, "?CV>"; UTL_REPORT::CANDEV
	PRINT #EXPORT.CH, "?CF>"; UTL_REPORT::CANFILE
	PRINT #EXPORT.CH, "?CE>"; UTL_REPORT::CANDET
	PRINT #EXPORT.CH, "?YN>"; UTL_REPORT::REPYN
	FOR I% = 0% TO 9%
		PRINT #EXPORT.CH, "?DE(" + NUM1$(I%) + ")>"; &
			TRM$(UTL_REPORT::DESCR(I%))
		PRINT #EXPORT.CH, "?OT(" + NUM1$(I%) + ")>"; &
			TRM$(UTL_REPORT::OPTTYPE(I%))
		PRINT #EXPORT.CH, "?OL(" + NUM1$(I%) + ")>"; &
			NUM1$(UTL_REPORT::OPTLEN(I%))
		PRINT #EXPORT.CH, "?VL(" + NUM1$(I%) + ")>"; &
			TRM$(UTL_REPORT::VALID(I%))
		PRINT #EXPORT.CH, "?RQ(" + NUM1$(I%) + ")>"; &
			TRM$(UTL_REPORT::REQUIRE(I%))
		PRINT #EXPORT.CH, "?IG(" + NUM1$(I%) + ")>"; &
			TRM$(UTL_REPORT::ITEMGROUP(I%))
		PRINT #EXPORT.CH, "?IT(" + NUM1$(I%) + ")>"; &
			TRM$(UTL_REPORT::ITEM(I%))
	NEXT I%
	PRINT #EXPORT.CH, "?CT>"; TRM$(UTL_REPORT::CHAINTO)
	PRINT #EXPORT.CH, "?LR>"; TRM$(UTL_REPORT::LASTRUNDATE)
	PRINT #EXPORT.CH, "?LT>"; TRM$(UTL_REPORT::LASTRUNTIME)
	PRINT #EXPORT.CH, "?BR>"; TRM$(UTL_REPORT::BASERUNDATE)
	PRINT #EXPORT.CH, "?RF>"; TRM$(UTL_REPORT::RUNFREQ)
	PRINT #EXPORT.CH, "?RW>"; NUM1$(UTL_REPORT::REPWID)

9000	!
	! Done
	!
	GOTO ExitProgram

 !19000
	!
	! Trap Errors
	!
 !	SELECT ERL
 !	END SELECT
 !
 !	RESUME ExitProgram

 ExitProgram:
	END FUNCTION
