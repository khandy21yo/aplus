1	%TITLE "Convert CK from RSTS/E"
	%SBTTL "CK_CONV_CONVERT"
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
	! Abstract:HELP
	!	.p
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS CK_SOURCE:CK_CONV_CONVERT/LINE
	!	$ LINK/EXEC:CK_EXE CK_CONV_CONVERT,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE CK_CONV_CONVERT.OBJ;*
	!
	! Author:
	!
	!	06/29/92 - Kevin Handy
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

10	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.HB"
	MAP (CK_CKMNT)	CK_CKMNT_CDD	CK_CKMNT

	EXTERNAL STRING  FUNCTION DATE_STOREDATE

	CALL ASSG_CHANNEL(ASCII.CH%, STAT%)

250	OPEN "BANKBL.RMS" FOR INPUT AS FILE ASCII.CH%, &
		RECORDSIZE 512%, &
		ACCESS READ

270	%INCLUDE "SOURCE:[CK.OPEN]CK_CKMNT.CRE"

	BANK$(I%) = "" FOR I% = 1% TO 8%

300	LINPUT #ASCII.CH%, INP$

	IT% = VAL(MID(INP$, 2%, 1%))

	GOSUB 2000 IF IT% = 1%		! Start of a new record?

	BANK$(IT%) = RIGHT(INP$, 4%)

	GOTO 300

2000	!
	! Slap down a couple of records as necessary.
	!
	CK_CKMNT::BANK_ACCT	= BANK$(2%)
	RSET CK_CKMNT::CKNUM	= BANK$(1%)
	CK_CKMNT::GLDATE	= DATE_STOREDATE(BANK$(8%))
	CK_CKMNT::ETYPE		= BANK$(3%)

	IF BANK$(5%) <> ""
	THEN
		CK_CKMNT::STYPE	= "G"
		CK_CKMNT::CKDAT	= DATE_STOREDATE(BANK$(4%))
		CK_CKMNT::CKAMT = VAL(BANK$(5%))

		PUT #CK_CKMNT.CH%
	END IF

	IF BANK$(7%) <> ""
	THEN
		CK_CKMNT::STYPE	= "B"
		CK_CKMNT::CKDAT	= DATE_STOREDATE(BANK$(6%))
		CK_CKMNT::CKAMT = VAL(BANK$(7%))

		PUT #CK_CKMNT.CH%
	END IF

	BANK$(I%) = "" FOR I% = 1% TO 8%

	RETURN

32767	END
