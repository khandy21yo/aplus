1	%TITLE "Convert AR Customers from MicroData"
	%SBTTL "AR_CONV_MDCUSTOM"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	!	.B
	!	.LM +5
	!	.LM -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_CONV_MDCUSTOM/LINE
	!	$ LINK/EXEC:AR_EXE AR_CONV_MDCUSTOM,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_CONV_MDCUSTOM.OBJ;*
	!
	! Author:
	!
	!	06/20/91 - Kevin Handy
	!
	! Modification history:
	!
	!	08/27/91 - Kevin Handy
	!		Default customer type to "01"
	!
	!	09/05/91 - Kevin Handy
	!		More defaults on customers, read onset date from
	!		file.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/04/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Lose line 800 (Dead code)
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

10	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)		AR_35CUSTOM_CDD	AR_35CUSTOM
	MAP (AR_35CUSTOM_BLANK)		AR_35CUSTOM_CDD	AR_35CUSTOM_BLANK

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTACT.HB"
	MAP (AR_CONTACT)		AR_CONTACT_CDD	AR_CONTACT
	MAP (AR_CONTACT_BLANK)		AR_CONTACT_CDD	AR_CONTACT_BLANK

	%PAGE

	CALL ASSG_CHANNEL(ARSYS.CH%,STAT%)

	!
	! Open the keyboard
	!
	CALL READ_INITIALIZE

	INPUT "For store number"; STORENO$

	START_DATE% = DATE_DAYCODE("19671231")

200	! RESUME LINE

 !	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.CRE"
	!======================================================================
	! AR_35CUSTOM file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_35CUSTOM.CH%, STAT%)
	CALL READ_DEVICE("AR_35CUSTOM",AR_35CUSTOM.DEV$, STAT%)
	CALL READ_PROTECTION("AR_35CUSTOM",AR_35CUSTOM.PRO$,STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$,STAT%)
	CALL WRIT_CURPROTECTION(AR_35CUSTOM.PRO$, STAT%)

	AR_35CUSTOM.NAME$ = AR_35CUSTOM.DEV$+"AR_35CUSTOM.MAS"

	OPEN AR_35CUSTOM.NAME$ FOR OUTPUT AS FILE AR_35CUSTOM.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_35CUSTOM, &
		BUFFER 32%, &
		PRIMARY KEY &
			AR_35CUSTOM::CUSNUM, &
		ALTERNATE KEY &
		( &
			AR_35CUSTOM::TTYPE, &
			AR_35CUSTOM::CUSNUM &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			AR_35CUSTOM::CATEGORY, &
			AR_35CUSTOM::CUSNUM &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			AR_35CUSTOM::ALPSRT, &
			AR_35CUSTOM::CUSNUM &
		)	CHANGES, &
		ACCESS MODIFY, ALLOW NONE

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)


 !	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTACT.CRE"

	!======================================================================
	! AR_CONTACT file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AR_CONTACT.CH%, STAT%)
	CALL READ_DEVICE("AR_CONTACT",AR_CONTACT.DEV$, STAT%)
	CALL READ_PROTECTION("AR_CONTACT",AR_CONTACT.PRO$,STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$,STAT%)
	CALL WRIT_CURPROTECTION(AR_CONTACT.PRO$, STAT%)

	AR_CONTACT.NAME$ = AR_CONTACT.DEV$+"AR_CONTACT.MAS"

	OPEN AR_CONTACT.NAME$ FOR OUTPUT AS FILE AR_CONTACT.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AR_CONTACT, &
		BUFFER 32%, &
		PRIMARY KEY &
			AR_CONTACT::CUSNUM &
			DUPLICATES, &
		ACCESS MODIFY, ALLOW NONE

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)


250	OPEN ARSYS_ASC.DEV$ + "CUS.ASC" FOR INPUT AS FILE ARSYS.CH%, &
		RECORDSIZE 512%, &
		ACCESS READ

	AR_35CUSTOM_BLANK::CUSNUM	= ""
	AR_35CUSTOM_BLANK::CUSNAM	= ""
	AR_35CUSTOM_BLANK::TTYPE	= "01"
	AR_35CUSTOM_BLANK::CATEGORY	= ""
	AR_35CUSTOM_BLANK::BDATE	= ""
	AR_35CUSTOM_BLANK::SSTATUS	= "A"
	AR_35CUSTOM_BLANK::EDATE	= ""
	AR_35CUSTOM_BLANK::ADD1		= ""
	AR_35CUSTOM_BLANK::ADD2		= ""
	AR_35CUSTOM_BLANK::ADD3		= ""
	AR_35CUSTOM_BLANK::CITY		= ""
	AR_35CUSTOM_BLANK::STATE	= ""
	AR_35CUSTOM_BLANK::ZIP		= ""
	AR_35CUSTOM_BLANK::COUNTRY	= ""
	AR_35CUSTOM_BLANK::COUNTY	= ""
	AR_35CUSTOM_BLANK::PHONE	= ""
	AR_35CUSTOM_BLANK::METHOD	= "O"
	AR_35CUSTOM_BLANK::STMTFLG	= "Y"
	AR_35CUSTOM_BLANK::ALPSRT	= ""
	AR_35CUSTOM_BLANK::SERCHRG	= "Y"
	AR_35CUSTOM_BLANK::TAXCODE	= "ID"
	AR_35CUSTOM_BLANK::TAXEXEMP	= ""
	AR_35CUSTOM_BLANK::LOCATION	= "100"
	AR_35CUSTOM_BLANK::TERMS	= ""
	AR_35CUSTOM_BLANK::CARRIER	= ""
	AR_35CUSTOM_BLANK::SALESMAN	= ""
	AR_35CUSTOM_BLANK::CREDITLIM	= 0.0
	AR_35CUSTOM_BLANK::DISCOUNT	= 0.0
	AR_35CUSTOM_BLANK::BACKORDER	= "Y"
	AR_35CUSTOM_BLANK::TAXFLAG	= "Y"

	AR_CONTACT_BLANK::CUSNUM	= ""
	AR_CONTACT_BLANK::CONTACT_NAME	= ""
	AR_CONTACT_BLANK::TITLE		= ""
	AR_CONTACT_BLANK::PHONE		= ""
	AR_CONTACT_BLANK::EXTENSION	= ""

	AR_35CUSTOM = AR_35CUSTOM_BLANK
	AR_CONTACT = AR_CONTACT_BLANK
	A$(I%) = "" FOR I% = 1% TO 4%

550	INPUT LINE #ARSYS.CH%, INP$
	INP$ = EDIT$(INP$, 4%)

560	if right(inp$, len(inp$)-1%) = "<>"
	then
		INPUT LINE #ARSYS.CH%, INP1$
		INP$ = LEFT(inp$, LEN(INP$)-2%) + EDIT$(INP1$, 4%)
		goto 560
	end if

	GOTO 550 IF INP$ = ""

	I2% = INSTR(1%, INP$, ">")
	FLD$ = SEG$(INP$, 2%, I2%-1%)
	DTA$ = RIGHT(INP$, I2%+1%)

 ! PRINT FLD$; TAB(20%); DTA$

	SELECT FLD$

	CASE "X"
		AR_35CUSTOM::CUSNUM = RIGHT(DTA$, 8%)
		THISSTORE$ = MID(DTA$, 5%, 3%)
		STORELIST$ = STORELIST$ + "!" + THISSTORE$ &
			IF INSTR(1%, STORELIST$, THISSTORE$) = 0%

	CASE "CLIENT"
		AR_35CUSTOM::CUSNAM = DTA$
		AR_35CUSTOM::ALPSRT = DTA$

	CASE "ADD1"
		A$(1%) = DTA$

	CASE "ADD2"
		A$(2%) = DTA$

	CASE "ADD3"
		A$(3%) = DTA$

	CASE "ADDR-4"
		A$(4%) = DTA$

	CASE "ZIP"
		AR_35CUSTOM::ZIP = DTA$
		ZIP$ = DTA$

	CASE "7"
		AR_CONTACT::CONTACT_NAME = DTA$

	CASE "8"
		I% = INSTR(1%, DTA$, "-")
		WHILE I%
			DTA$ = LEFT(DTA$, I% - 1%) + RIGHT(DTA$, I% + 1%)
			I% = INSTR(1%, DTA$, "-")
		NEXT
		AR_35CUSTOM::PHONE = DTA$

	CASE "13"
		AR_35CUSTOM::CREDITLIM = VAL(DTA$)

	CASE "TERMS"
		AR_35CUSTOM::TERMS = DTA$

	CASE "TYPE"
		AR_35CUSTOM::TTYPE = DTA$

	CASE "SM"
		AR_35CUSTOM::SALESMAN = DTA$

	CASE "TX-JUR"
		AR_35CUSTOM::TAXCODE = DTA$

	CASE "23"
		AR_35CUSTOM::BDATE = DATE_INVDCODE(START_DATE% + VAL%(DTA$))

600	CASE "END"

		IF STORENO$ <> THISSTORE$
		THEN
			AR_35CUSTOM = AR_35CUSTOM_BLANK
			AR_CONTACT = AR_CONTACT_BLANK
			A$(I%) = "" FOR I% = 1% TO 4%
			GOTO 550
		END IF

		!
		! Strip off extra zip code
		!
		FOR I% = 1% to 4%
			X% = LEN(A$(I%))-LEN(ZIP$)
			IF RIGHT(A$(I%), X%+1%) = ZIP$
			THEN
				A$(I%) = TRM$(LEFT(A$(I%), X%))
			END IF
		NEXT I%

		!
		! Look for state stuck to end of a city
		!
		z% = 0%
		z% = i% if a$(i%) <> "" for i% = 1% to 4%

		if z%
		then
			!
			! Lose period stuck on end.
			!
			IF RIGHT(A$(Z%),LEN(A$(Z%))) = "."
			THEN
				A$(Z%) = LEFT(A$(Z%), LEN(A$(Z%)) - 1%)
			END IF

			!
			! Check for "xxxx, xx" format
			!
			if mid(a$(z%),len(a$(z%))-3%,1%) = ","
			then
				ar_35custom::state = right(a$(z%), len(a$(z%))-1%)
				ar_35custom::city = left(a$(z%), len(a$(z%))-4%)
				a$(z%) = ""
			end if

			IF RIGHT(A$(Z%),LEN(A$(Z%))-6%) = ", IDAHO"
			then
				ar_35custom::state = "ID"
				ar_35custom::city = left(a$(z%), len(a$(z%))-7%)
				a$(z%) = ""
			end if

			IF RIGHT(A$(Z%),LEN(A$(Z%))-5%) = ", UTAH"
			then
				ar_35custom::state = "UT"
				ar_35custom::city = left(a$(z%), len(a$(z%))-6%)
				a$(z%) = ""
			end if

		end if

		!
		! Stick in addresses
		!
		AR_35CUSTOM::ADD1 = A$(1%)
		AR_35CUSTOM::ADD2 = A$(2%)
		AR_35CUSTOM::ADD3 = A$(3%)
		AR_35CUSTOM::CITY = A$(4%) &
			IF (AR_35CUSTOM::CITY = "")

690		!
		! Write out record
		!
		PUT #AR_35CUSTOM.CH%
 !	PRINT AR_35CUSTOM::CUSNUM

		IF NOT(AR_CONTACT::CONTACT_NAME = "")
		THEN
			AR_CONTACT::CUSNUM = AR_35CUSTOM::CUSNUM
			AR_CONTACT::PHONE = AR_35CUSTOM::PHONE
			PUT #AR_CONTACT.CH%
		END IF

695		A$(I%) = "" FOR I% = 1% TO 4%
		AR_35CUSTOM = AR_35CUSTOM_BLANK
		AR_CONTACT = AR_CONTACT_BLANK

 !	CASE ELSE
 !		PRINT "Undefined code '"; FLD$; "'"
	END SELECT

	GOTO 550

 !800
	!
	! Done
	!
 !	CLOSE AR_35CUSTOM.CH%
 !
 !	PRINT "Store list"; STORELIST$
 !
 !	GOTO 32767

	%PAGE

19000	!*******************************************************************
	! Trap Errors
	!*******************************************************************

	SELECT ERL

	CASE 550%

	CASE 690%
		PRINT "%Put"; AR_35CUSTOM::CUSNUM; " "; ERR; " "; ERT$(ERR)
		RESUME 695

	END SELECT

	ON ERROR GOTO 0

32767	END
