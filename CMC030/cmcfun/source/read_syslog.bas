1	%TITLE "Translate a Logical Name"
	%SBTTL "READ_SYSLOG"
	%IDENT "V3.6a Calico"

	FUNCTION STRING READ_SYSLOG(LOGNAM1$)

	!
	! COPYRIGHT (C) 1987 BY
	!
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
	!	.p
	!	This function returns a string containing the logical
	!	equivalence name of the given logical name or the given
	!	logical name if there is no equivalence.
	!
	! Index:
	!
	! Parameters:
	!
	!	LOGNAM1$
	!		The passed logical name the user wants to translate.
	!
	!	Returned value
	!		This function returns a string containing the logical
	!		equivalence name of the given logical name.
	!
	! Example:
	!
	!	NAME$ = READ_SYSLOG("CMC$CLIPBOARD")
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:READ_SYSLOG/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP READ_SYSLOG
	!	$ DELETE READ_SYSLOG.OBJ;*
	!
	! AUTHOR:
	!
	!	12/12/86 - B. Craig Larsen
	!
	! MODIFICATION HISTORY:
	!
	!	10/19/87 - Kevin Handy
	!		Modified to go down several levels of translation,
	!		and ability to handle file names.
	!
	!	07/03/89 - Kevin Handy
	!		Modified to use a record instead of a map
	!		so will work in sharable library.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "$SSDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	EXTERNAL LONG FUNCTION SYS$TRNLNM

	DECLARE LONG SYS_STAT
	DECLARE LONG CONSTANT LNM$_STRING = 2

	RECORD Z_RECORD
		STRING LOG_BUF = 255
	END RECORD

	DECLARE Z_RECORD LOG_BUF

	!
	! Record structure for the ITEMLIST used in many sys$ calls and
	!	run time library calls.
	!
	RECORD ITEMLST
	VARIANT
	CASE
		WORD	BUFLEN
		WORD	CODE
		LONG	BUFADR
		LONG	RETLENADR
	CASE
		LONG	ENDLIST
	END VARIANT
	END RECORD ITEMLST

	DECLARE	ITEMLST	ITMLST(2)
	DECLARE	LONG	N_BYTES

	%PAGE

	!
	! Initilization
	!
	PREVIOUS$ = ""
	CURRENT$ = LOGNAM1$

	!
	! See if we have looped to the end
	!
	WHILE (PREVIOUS$ <> CURRENT$)

		PREVIOUS$ = CURRENT$

		!
		! Start translation.  Strip off name up to colon.
		!
		COLON% = INSTR(1%, CURRENT$, ":")

		IF (COLON% = 0%)
		THEN
			LLOGIC$ = CURRENT$
		ELSE
			LLOGIC$ = LEFT(CURRENT$, COLON% - 1%)
		END IF

		LLOGIC$ = EDIT$(LLOGIC$, 4% + 8% + 32% + 128% + 256%)

		GOSUB TranslateOnce

		GOTO SkipOut IF (SYS_STAT AND 1%) = 0%

		IF COLON% = 0%
		THEN
			CURRENT$ = LLOGIC$
		ELSE
			CURRENT$ = LLOGIC$ + RIGHT(CURRENT$, COLON% + 1%)
		END IF

	NEXT

 SkipOut:
	!
	! Strip off leading underscores so things are easier to match.
	!
	CURRENT$ = RIGHT(CURRENT$, 2%) IF LEFT(CURRENT$, 1%) = "_"

	READ_SYSLOG = CURRENT$
	GOTO 32767


	!*******************************************************************
	! Translation for one pass
	!*******************************************************************

 TranslateOnce:

	LOG_BUF::LOG_BUF = ""

	!
	! Equivalence string
	!
	ITMLST(0)::BUFLEN	= 255
	ITMLST(0)::CODE		= LNM$_STRING
	ITMLST(0)::BUFADR	= LOC(LOG_BUF::LOG_BUF)
	ITMLST(0)::RETLENADR	= LOC(N_BYTES)

	!
	! End of list
	!
	ITMLST(1)::ENDLIST	= 0%

	!
	! Get the sys call and convert to integer
	!
	SYS_STAT = SYS$TRNLNM &
	( &
		LNM$M_CASE_BLIND BY REF, &
		"LNM$DCL_LOGICAL" BY DESC, &
		LLOGIC$ BY DESC,, &
		ITMLST() BY REF &
	)

	SELECT SYS_STAT
	!
	! Sucessfull translation
	!
	CASE SS$_NORMAL
		LLOGIC$ = EDIT$(LOG_BUF::LOG_BUF, 4% + 8% + 32% + 128% + 256%)
		LLOGIC$ = LLOGIC$ + ":" &
			IF (COLON%) AND &
			INSTR(1%, LLOGIC$, ":") = 0% AND &
			INSTR(1%, LLOGIC$, "]") = 0%

	END SELECT

	RETURN

32767	END FUNCTION
