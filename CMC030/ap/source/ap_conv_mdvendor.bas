1	%TITLE "Convert AP VENDORS from MicroData"
	%SBTTL "AP_CONV_MDVENDOR"
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
	!	.p
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_CONV_MDVENDOR/LINE
	!	$ LINK/EXEC:AP_EXE AP_CONV_MDVENDOR,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_CONV_MDVENDOR.OBJ;*
	!
	! Author:
	!
	!	06/20/91 - Kevin Handy
	!
	! Modification history:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/09/99 - Kevin Handy
	!		Lose line 800 (Dead Code)
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

10	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE


	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP (AP_VENDOR)		AP_VENDOR_CDD	AP_VENDOR
	MAP (AP_VENDOR_BLANK)	AP_VENDOR_CDD	AP_VENDOR_BLANK

	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTACT.HB"
	MAP (AP_CONTACT)		AP_CONTACT_CDD	AP_CONTACT
	MAP (AP_CONTACT_BLANK)		AP_CONTACT_CDD	AP_CONTACT_BLANK


	CALL ASSG_CHANNEL(APSYS.CH%,STAT%)
	!
	! Open the keyboard
	!
	CALL READ_INITIALIZE

	INPUT "For store number"; STORENO$

200	! RESUME LINE

 !	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.CRE"
	!======================================================================
	! AP_VENDOR file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AP_VENDOR.CH%, STAT%)
	CALL READ_DEVICE("AP_VENDOR",AP_VENDOR.DEV$, STAT%)
	CALL READ_PROTECTION("AP_VENDOR",AP_VENDOR.PRO$,STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$,STAT%)
	CALL WRIT_CURPROTECTION(AP_VENDOR.PRO$, STAT%)

	AP_VENDOR.NAME$ = AP_VENDOR.DEV$ + "AP_VENDOR.MAS"

	OPEN AP_VENDOR.NAME$ FOR OUTPUT AS FILE AP_VENDOR.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_VENDOR, &
		BUFFER 32%, &
		PRIMARY KEY &
			AP_VENDOR::VENNUM, &
		ALTERNATE KEY &
			AP_VENDOR::VENNAM &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AP_VENDOR::ALPSRT &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AP_VENDOR::STATE &
			DUPLICATES CHANGES, &
		ALTERNATE KEY &
			AP_VENDOR::ZIP &
			DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW NONE

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)


 !	%INCLUDE "SOURCE:[AP.OPEN]AP_CONTACT.CRE"

	!======================================================================
	! AP_CONTACT file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(AP_CONTACT.CH%, STAT%)
	CALL READ_DEVICE("AP_CONTACT",AP_CONTACT.DEV$, STAT%)
	CALL READ_PROTECTION("AP_CONTACT",AP_CONTACT.PRO$,STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$,STAT%)
	CALL WRIT_CURPROTECTION(AP_CONTACT.PRO$, STAT%)

	AP_CONTACT.NAME$ = AP_CONTACT.DEV$+"AP_CONTACT.MAS"

	OPEN AP_CONTACT.NAME$ FOR OUTPUT AS FILE AP_CONTACT.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AP_CONTACT, &
		BUFFER 32%, &
		PRIMARY KEY &
			AP_CONTACT::CUSNUM &
			DUPLICATES, &
		ACCESS MODIFY, ALLOW NONE

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)


250	OPEN APSYS_ASC.DEV$ + "VEND.ASC" FOR INPUT AS FILE APSYS.CH%, &
		RECORDSIZE 512%, &
		ACCESS READ

	AP_VENDOR_BLANK::VENNUM	= ""
	AP_VENDOR_BLANK::VENNAM	= ""
	AP_VENDOR_BLANK::ADD1		= ""
	AP_VENDOR_BLANK::ADD2		= ""
	AP_VENDOR_BLANK::CITY		= ""
	AP_VENDOR_BLANK::STATE	= ""
	AP_VENDOR_BLANK::ZIP		= ""
	AP_VENDOR_BLANK::COUNTRY	= ""
	AP_VENDOR_BLANK::PHONE	= ""

	AP_VENDOR_BLANK::POADD1		= ""
	AP_VENDOR_BLANK::POADD2		= ""
	AP_VENDOR_BLANK::POCITY		= ""
	AP_VENDOR_BLANK::POSTATE	= ""
	AP_VENDOR_BLANK::POZIP		= ""
	AP_VENDOR_BLANK::POCOUNTRY	= ""
	AP_VENDOR_BLANK::POPHONE	= ""

	AP_VENDOR_BLANK::PURGE		= "N"
	AP_VENDOR_BLANK::FEDID		= ""
	AP_VENDOR_BLANK::FLG1099	= "N"
	AP_VENDOR_BLANK::DUEDAYS	= 0%
	AP_VENDOR_BLANK::DUEDATE	= ""
	AP_VENDOR_BLANK::DISDAYS	= 0%
	AP_VENDOR_BLANK::DISDATE	= ""
	AP_VENDOR_BLANK::DISCPER	= 0%
	AP_VENDOR_BLANK::ALPSRT		= ""

	AP_CONTACT_BLANK::CUSNUM	= ""
	AP_CONTACT_BLANK::CONTACT_NAME	= ""
	AP_CONTACT_BLANK::TITLE		= ""
	AP_CONTACT_BLANK::PHONE		= ""
	AP_CONTACT_BLANK::EXTENSION	= ""

	AP_VENDOR = AP_VENDOR_BLANK
	AP_CONTACT = AP_CONTACT_BLANK
	A$(I%) = "" FOR I% = 1% TO 9%

550	INPUT LINE #APSYS.CH%, INP$
	INP$ = EDIT$(INP$, 4%)

560	if right(inp$, len(inp$)-1%) = "<>"
	then
		INPUT LINE #APSYS.CH%, INP1$
		INP$ = LEFT(inp$, LEN(INP$)-2%) + EDIT$(INP1$, 4%)
		goto 560
	end if

	GOTO 550 IF INP$ = ""

	I2% = INSTR(1%, INP$, ">")
	FLD$ = SEG$(INP$, 2%, I2%-1%)
	DTA$ = RIGHT(INP$, I2%+1%)

 ! PRINT FLD$; TAB(20%); DTA$

	SELECT FLD$

		CASE "0", "VEND#"
			AP_VENDOR::VENNUM = RIGHT(DTA$, 8%)
			THISSTORE$ = MID(DTA$, 5%, 3%)
			STORELIST$ = STORELIST$ + "!" + THISSTORE$ &
				IF INSTR(1%, STORELIST$, THISSTORE$) = 0%

		CASE "1", "TYPE"
			AP_VENDOR::VENNAM = DTA$
			AP_VENDOR::ALPSRT = dta$

		CASE "2", "ADDRESS"
			A$(1%) = DTA$

		CASE "3", "ADDRESS2"
			A$(2%) = DTA$

		CASE "4", "CITY"
			A$(3%) = DTA$

		CASE "5", "ZIPCODE"
			AP_VENDOR::ZIP = DTA$
			ZIP$ = DTA$

		CASE "17", "PHONE"
			I% = INSTR(1%, DTA$, "-")
			WHILE I%
				DTA$ = LEFT(DTA$, I% - 1%) + RIGHT(DTA$, I% + 1%)
				I% = INSTR(1%, DTA$, "-")
			NEXT
			AP_VENDOR::PHONE = DTA$

		CASE "18", "CONTACT"
			AP_CONTACT::CONTACT_NAME = DTA$

		CASE "19", "1099FLG"
			AP_VENDOR::FLG1099 = DTA$

		CASE "20", "S.S.NO"
			AP_VENDOR::FEDID = DTA$

600		CASE "END"

			IF STORENO$ <> THISSTORE$
			THEN
				AP_VENDOR = AP_VENDOR_BLANK
				AP_CONTACT = AP_CONTACT_BLANK
				A$(I%) = "" FOR I% = 1% TO 9%
				GOTO 550
			END IF

			!
			! Look for state stuck to end of a city
			!
			z% = 0%
			z% = i% if a$(i%) <> "" for i% = 1% to 4%

			if z%
			then
				if mid(a$(z%),len(a$(z%))-3%,1%) = ","
				then
					AP_VENDOR::state = right(a$(z%), len(a$(z%))-1%)
					AP_VENDOR::city = left(a$(z%), len(a$(z%))-4%)
					a$(z%) = ""
				end if

				IF RIGHT(A$(Z%),LEN(A$(Z%))-6%) = ", IDAHO"
				then
					AP_VENDOR::state = "ID"
					AP_VENDOR::city = left(a$(z%), len(a$(z%))-7%)
					a$(z%) = ""
				end if

			end if

			!
			! Stick in addresses
			!
			AP_VENDOR::ADD1 = A$(1%)
			AP_VENDOR::ADD2 = A$(2%)
			AP_VENDOR::CITY = A$(3%) &
				IF (AP_VENDOR::CITY = "")

690			!
			! Write out record
			!
			AP_VENDOR::POADD1	= AP_VENDOR::ADD1
			AP_VENDOR::POADD2	= AP_VENDOR::ADD2
			AP_VENDOR::POCITY	= AP_VENDOR::CITY
			AP_VENDOR::POSTATE	= AP_VENDOR::STATE
			AP_VENDOR::POZIP	= AP_VENDOR::ZIP
			AP_VENDOR::POCOUNTRY	= ""
			AP_VENDOR::POPHONE	= AP_VENDOR::PHONE

			PUT #AP_VENDOR.CH%

 !			PRINT AP_VENDOR::VENNUM

693			IF NOT(AP_CONTACT::CONTACT_NAME = "")
			THEN
				AP_CONTACT::CUSNUM = AP_VENDOR::VENNUM
				AP_CONTACT::PHONE = AP_VENDOR::PHONE
				PUT #AP_CONTACT.CH%
			END IF

695			A$(I%) = "" FOR I% = 1% TO 9%
			AP_VENDOR = AP_VENDOR_BLANK
			AP_CONTACT = AP_CONTACT_BLANK

		CASE ELSE
 !			PRINT "Undefined code '"; FLD$; "'"
	END SELECT

	GOTO 550

 !800
	!
	! Done
	!
 !	CLOSE AP_VENDOR.CH%
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
			PRINT "%Put"; AP_VENDOR::VENNUM; " "; ERR; " "; ERT$(ERR)
			RESUME 695

	END SELECT

	ON ERROR GOTO 0

32767	END
