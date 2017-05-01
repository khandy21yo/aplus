1	%TITLE "Write Report Info into File"
	%SBTTL "OUTP_3WRITESTRUCTURE"
	%IDENT "V3.6a Calico"

	SUB OUTP_3WRITESTRUCTURE(UTL_REPORTX_CDD UTL_REPORTX, &
		WORD OUTPUT.CH, PRINTX_CDD PRINTX)

	!
	! COPYRIGHT (C) 1986 BY
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho  83404
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
	!	Writes out the UTL_REPORTX structure in a format that
	!	OUTP_INITFROMFILE can read.
	!
	! Index:
	!
	! Parameters:
	!
	!	UTL_REPORTX
	!		The passed definition of the current report file.
	!
	!	OUTPUT.CH
	!		The returned output file to write the report to.
	!
	!	Returned value
	!		This subroutine writes a report's information into a
	!		file.
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_3WRITESTRUCTURE/LINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP OUTP_3WRITESTRUCTURE
	!	$ DELETE OUTP_3WRITESTRUCTURE.OBJ;*
	!
	! Author:
	!
	!	10/20/92 - Kevin Handy
	!		Taken from OUTP_WRITESTRUCTURE and made sharable.
	!
	! Modification history:
	!
	!	03/13/95 - Kevin Handy
	!		Added code for three new fields:
	!		AFTERTIME, BACKGROUND, OFFSET.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	07/29/96 - Kevin Handy
	!		Reformat source code.
	!
	!	11/11/97 - Kevin Handy
	!		Use constants instead of hard coded numbers for
	!		PRINTTO comparisons.
	!
	!	08/19/98 - Kevin Handy
	!		Lose (SCOPE) so that it doesn't break the sharable
	!		library.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$ routines
	!
	!	06/08/99 - Kevin Handy
	!		Lose ErrorGetCh which is never called
	!
	!	07/18/2001 - Kevin Handy
	!		Only write 'SL' out once instead of twice.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:PRINT35.INC"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	!
	! External functions
	!
	EXTERNAL STRING  FUNCTION OUTP_CREATESTR
	EXTERNAL INTEGER FUNCTION FIND_3PRINTGROUPITEM
	EXTERNAL STRING  FUNCTION READ_SYSJOB

	DECLARE LONG SPOOL.CH

	%PAGE

4150	!***************************************************************
	! TEMPORARY PRINT COMMAND FILE
	!***************************************************************

	JJ$ = READ_SYSJOB

	SELECT UTL_REPORTX::PRINTTO

	CASE OUTP_TOSPOOL			! Spool
		SMG_STATUS% = LIB$GET_LUN(SPOOL.CH)
		UTL_REPORTX::DEFOUT = FNSPOOL$(JJ$, SPOOL.CH)
		OPEN UTL_REPORTX::DEFOUT FOR OUTPUT AS FILE SPOOL.CH, &
			RECORDSIZE 255%
		CLOSE SPOOL.CH
		SMG_STATUS% = LIB$FREE_LUN(SPOOL.CH)
		GOTO 4175

	END SELECT

4175	!
	! Dump out Report Settings print file
	!
	PRINT #OUTPUT.CH, "RN>"; UTL_REPORTX::REPNUM
	PRINT #OUTPUT.CH, "PG>"; TRM$(UTL_REPORTX::PRODEV) + &
		TRM$(UTL_REPORTX::PRONAM)
	PRINT #OUTPUT.CH, "SP>"; UTL_REPORTX::STARTP
	PRINT #OUTPUT.CH, "EP>"; UTL_REPORTX::ENDP
	PRINT #OUTPUT.CH, "PD>"; UTL_REPORTX::REPYN
	PRINT #OUTPUT.CH, "RD>"; UTL_REPORTX::REPDATE
	PRINT #OUTPUT.CH, "AS>"; UTL_REPORTX::AUTOSCROLL
	PRINT #OUTPUT.CH, "OD>"; UTL_REPORTX::DEFOUT
	PRINT #OUTPUT.CH, "OF>"; UTL_REPORTX::OFFSET
	PRINT #OUTPUT.CH, "AF>"; UTL_REPORTX::AFTERTIME
	PRINT #OUTPUT.CH, "BG>"; UTL_REPORTX::BACKGROUND
	PRINT #OUTPUT.CH, "XX>"; UTL_REPORTX::PRINTTO
	PRINT #OUTPUT.CH, "U1>"; UTL_REPORTX::OPTDEF(0%)
	PRINT #OUTPUT.CH, "U2>"; UTL_REPORTX::OPTDEF(1%)
	PRINT #OUTPUT.CH, "U3>"; UTL_REPORTX::OPTDEF(2%)
	PRINT #OUTPUT.CH, "U4>"; UTL_REPORTX::OPTDEF(3%)
	PRINT #OUTPUT.CH, "U5>"; UTL_REPORTX::OPTDEF(4%)
	PRINT #OUTPUT.CH, "U6>"; UTL_REPORTX::OPTDEF(5%)
	PRINT #OUTPUT.CH, "U7>"; UTL_REPORTX::OPTDEF(6%)
	PRINT #OUTPUT.CH, "U8>"; UTL_REPORTX::OPTDEF(7%)
	PRINT #OUTPUT.CH, "U9>"; UTL_REPORTX::OPTDEF(8%)
	PRINT #OUTPUT.CH, "U0>"; UTL_REPORTX::OPTDEF(9%)

	PRINT #OUTPUT.CH, "TL>"; UTL_REPORTX::TOLOCAL
	PRINT #OUTPUT.CH, "TS>"; UTL_REPORTX::TOSCREEN

	!
	! Spooler stuff
	!
	IF UTL_REPORTX::PRINTTO = OUTP_TOSPOOL
	THEN
		PRINT #OUTPUT.CH, "CP>"; UTL_REPORTX::COPIES
		PRINT #OUTPUT.CH, "SL>"; UTL_REPORTX::SPOOL
		PRINT #OUTPUT.CH, "SF>"; UTL_REPORTX::SPOOLFORM
	END IF

	!
	! Use printer stuff only if it makes sense
	!
	IF (UTL_REPORTX::PRINTTO <> OUTP_TODISPLAY) AND &
		(UTL_REPORTX::PRINTTO < 10%)
	THEN
		!
		! Create initilizier string
		!
		INTRO$ = ""
		FINUP$ = ""

		LOOP% = FIND_3PRINTGROUPITEM("II", "*", PRINTX)
		IF LOOP% > 0%
		THEN
			INTRO$ = OUTP_CREATESTR(PRINTX::SEQU(LOOP%), "*")
		END IF

		LOOP% = FIND_3PRINTGROUPITEM("ZZ", "*", PRINTX)
		IF LOOP% > 0%
		THEN
			FINUP$ = OUTP_CREATESTR(PRINTX::SEQU(LOOP%), "*")
		END IF

		FOR LOOP% = 1% TO PRINTX::GROUPS

			IF FIND_3PRINTGROUPITEM(PRINTX::GROUPX(LOOP%), &
				"*", PRINTX) = 0% OR &
				PRINTX::GROUPX(LOOP%) = "LP"
			THEN
				ITEM% = FIND_3PRINTGROUPITEM(PRINTX::GROUPX(LOOP%), &
					PRINTX::DEFLT(LOOP%), PRINTX)
				INTRO$ = INTRO$ + &
					OUTP_CREATESTR(PRINTX::SEQU(ITEM%), &
					PRINTX::DEFLT(LOOP%)) IF ITEM%
			END IF

		NEXT LOOP%

		!
		! Printer control strings
		!
		PRINT #OUTPUT.CH, "PT>"; UTL_REPORTX::PRINTTYPE
		PRINT #OUTPUT.CH, "PC>"; INTRO$
		PRINT #OUTPUT.CH, "ZZ>"; FINUP$

		FOR LOOP% = 1% TO PRINTX::GROUPS

			IF FIND_3PRINTGROUPITEM(PRINTX::GROUPX(LOOP%), &
				"*", PRINTX) = 0% &
				OR PRINTX::GROUPX(LOOP%) = "LP"
			THEN
				PRINT #OUTPUT.CH, &
					PRINTX::GROUPX(LOOP%); ">"; &
					EDIT$(PRINTX::DEFLT(LOOP%), 128%)
			END IF
		NEXT LOOP%

		!
		! Next page stuff
		!
		LOOP% = FIND_3PRINTGROUPITEM("NP", "*",PRINTX)
		IF LOOP% > 0%
		THEN
			PRINT #OUTPUT.CH, &
				"NP>"; OUTP_CREATESTR(PRINTX::SEQU(LOOP%), "*")
		END IF

	END IF

 ExitProgram:
	EXIT SUB

	%PAGE

30100	!**************************************************************
	! GET A UNIQUE SPOOLER FILENAME
	!**************************************************************
	DEF FNSPOOL$(QJN$, CHN%)

		RANDOMIZE

30110		SFILE$ = "S" + QJN$ + "_" + &
			FORMAT$(RND * 1E6, "<0>#####") + ".SPL"

		WHEN ERROR IN
			OPEN SFILE$ FOR INPUT AS FILE CHN%
		USE
			CONTINUE 30120 IF ERR = 5%
			CONTINUE 30113 IF ERR = 138%

			PRINT "XXX: ERROR# " + NUM1$(ERR)
			CONTINUE 30113

		END WHEN

		CLOSE CHN%

30113		GOTO 30110

30120		!
		! Create the file to allocate it to this job
		!
		FNSPOOL$ = SFILE$ + ""
	FNEND

32767	END SUB
