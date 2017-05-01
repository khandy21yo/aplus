1	%TITLE "Calculate Physical Start and End Dates for GL"
	%SBTTL "DATE_GLDATE"
	%IDENT "V3.6a Calico"

	SUB DATE_GLDATE(GL_PERIOD_CDD GL_PERIOD, &
		STRING PERIOD, &
		STRING START_DATE, &
		STRING END_DATE)

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function calculates the beginning and ending dates
	!	for a specified General Ledger period.
	!	.b
	!	^*Note:\*  Currently assumes a 12 period year (should be
	!	changed for any number of periods).
	!	Stolen from AR_CLOS_CLOSE.
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:DATE_GLDATE
	!	$ LIB FUNC_LIB:CMCFUN/REPLACE DATE_GLDATE
	!	$ DELETE DATE_GLDATE.OBJ;*
	!
	! Author:
	!
	!	12/13/88 - Kevin Handy
	!
	! Modification history:
	!
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/15/95 - Kevin Handy
	!		Change RIGHT(NUM1$()) to FORMAT$()
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"

	!
	! External functions
	!
	EXTERNAL STRING  FUNCTION DATE_INVDCODE
	EXTERNAL LONG    FUNCTION DATE_DAYCODE
	EXTERNAL STRING  FUNCTION DATE_INVMCODE
	EXTERNAL LONG    FUNCTION DATE_MONCODE

	%PAGE

	CUR_PERIOD% = VAL%(MID(PERIOD, 5%, 2%))
	YEAR$ = LEFT(PERIOD, 4%)

	!
	! Note: YYYY_PP has underscore stored in it, YYYYPP doesn't.
	!
	YYYY_PP$ = YEAR$ + "_" + FORMAT$(CUR_PERIOD%, "<0>#")
	YYYYPP$ = YEAR$ + FORMAT$(CUR_PERIOD%, "<0>#")

	!
	! Calculate the date of the last day of the period, if
	! possible.
	!
	AGE.DATE$ = LEFT(DATE_INVMCODE( &
		DATE_MONCODE(YYYYPP$) - GL_PERIOD::NEWYEAR + 1% &
		), 6%) + "20"

	!
	! Wiz-bang method of calculating last date of month.
	! Take some date in the month (say the 20'th), add enough to force
	! it into the next month, but not any farther, change the
	! date to the first of that month, then subtract one day.
	!
	! (Don't laugh, it's all I could come up with on the spur of
	! the moment, and it does work)
	!
	END_DATE = DATE_INVDCODE(DATE_DAYCODE( &
		LEFT(DATE_INVDCODE(DATE_DAYCODE(AGE.DATE$) + 20%), &
		6%) + "01") - 1%)

	START_DATE = LEFT(AGE.DATE$, 6%) + "01"

32767	END SUB
