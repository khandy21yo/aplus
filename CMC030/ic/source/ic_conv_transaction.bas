1	%TITLE "Convert from RSTS/E to VMS"
	%SBTTL "IC_CONV_TRANSACTION"
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
	!	.b
	!	.lm +5
	!	This program is used in the conversion from RSTS/E
	!	to VMS.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_CONV_TRANSACTION/LINE
	!	$ LINK/EXECUTABLE=IC_EXE: IC_CONV_TRANSACTION, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_CONV_TRANSACTION.OBJ;*
	!
	! Author:
	!
	!	08/13/92 - Dan Perkins
	!
	! Modification history:
	!
	!	09/21/92 - Kevin Handy
	!		Clean up (check)
	!
	!	10/26/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/30/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/18/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

10	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[IC.OPEN]IC_TRANSACTION.HB"
	MAP (IC_TRANSACTION)	IC_TRANSACTION_CDD	IC_TRANSACTION

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	DECLARE			AR_35CUSTOM_CDD		AR_35CUSTOM_EXAM

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION AR_EXAM_CUSTOM
	EXTERNAL REAL   FUNCTION PC_READ_COST
	EXTERNAL REAL   FUNCTION PC_READ_PRICE

	!*******************************************************************
	! Initilize File to Convert
	!*******************************************************************

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(ICRMS3.CH%, STAT%)

	CALL READ_INITIALIZE

	CALL READ_DEVICE("ICRMS3_ASC", ICRMS3_ASC.DEV$, STAT%)

	!
	! Open input file
	!
300	WHEN ERROR IN
		OPEN ICRMS3_ASC.DEV$ + "ICRMS3.ASC" FOR INPUT AS FILE ICRMS3.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "File to convert is missing", 0%)
		CONTINUE ExitProgram
	END WHEN


	!
	! Process the input file
	!
	FILEFLAG% = 0%

1000	WHEN ERROR IN
		INPUT LINE #ICRMS3.CH%, INP$
	USE
		CONTINUE ExitProgram
	END WHEN

	GOTO 1000 IF INP$ = ""

	INP$ = EDIT$(INP$, 4%)
	I2%  = INSTR(1%, INP$, ">")
	FLD$ = SEG$(INP$, 2%, I2% - 1%)
	DTA$ = RIGHT(INP$, I2% + 1%)

	SELECT FLD$

	CASE "STARTFILE IC_TRANSACTION"
		YYYYPP$ = LEFT(DTA$, 6%)
		OPEN_FLAG$ = RIGHT(DTA$, 7%)

		CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
			"9;22", "Confirm converting IC_TRANSACTION_" + &
			YYYYPP$ + " file", &
			"N", 16%, "'", "N"), -1%)

		IF CONF$ = "Y"
		THEN
			GOSUB 3000
			GOSUB InitICTransactionRec
			WORKFILE% = 1%
			FILEFLAG% = -1%
		END IF

	CASE "ENDFILE IC_TRANSACTION"
		CLOSE #IC_TRANSACTION.CH%
		CALL ASSG_FREECHANNEL(IC_TRANSACTION.CH%)
		FILEFLAG% = 0%

	END SELECT

	ON WORKFILE% GOSUB 10000 IF FILEFLAG%

	GOTO 1000

3000	!
	! Create IC_TRANSACTION file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating new IC_TRANSACTION file", 1%)

 !	KILL IC_35BALANCE.DEV$ + "IC_TRANSACTION_" + YYYYPP$ + ".LED" &
 !		IF OPEN_FLAG$ = ""

	SMG_STATUS% = LIB$DELETE_FILE(IC_35BALANCE.DEV$ + &
		"IC_TRANSACTION_" + YYYYPP$ + ".LED;*") &
		IF OPEN_FLAG$ = ""

3010	!======================================================================
	! IC_TRANSACTION file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(IC_TRANSACTION.CH%, STAT%)
	CALL READ_DEVICE("IC_TRANSACTION", IC_TRANSACTION.DEV$, STAT%)
	CALL READ_PROTECTION("IC_TRANSACTION", IC_TRANSACTION.PRO$, STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$, STAT%)
	CALL WRIT_CURPROTECTION(IC_TRANSACTION.PRO$, STAT%)

	IC_TRANSACTION.NAME$ = IC_TRANSACTION.DEV$ + "IC_TRANSACTION_" + &
		YYYYPP$ + ".LED"

	OPEN IC_TRANSACTION.NAME$ AS FILE IC_TRANSACTION.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP IC_TRANSACTION, &
		BUFFER 32%, &
		PRIMARY KEY &
		( &
			IC_TRANSACTION::PRODUCT, &
			IC_TRANSACTION::LOCATION, &
			IC_TRANSACTION::TRANS_DATE &
		)	DUPLICATES, &
		ALTERNATE KEY &
		( &
			IC_TRANSACTION::BATCH, &
			IC_TRANSACTION::PRODUCT &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			IC_TRANSACTION::SUBACCOUNT, &
			IC_TRANSACTION::PRODUCT &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			IC_TRANSACTION::CROSS_REF, &
			IC_TRANSACTION::PRODUCT &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			IC_TRANSACTION::LOCATION, &
			IC_TRANSACTION::PRODUCT &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW NONE

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)

	RETURN

10000	SELECT FLD$

	CASE "ENDRECORD"
		IF IC_TRANSACTION::COST = 0.0
		THEN
			IC_TRANSACTION::COST = PC_READ_COST( &
				IC_TRANSACTION::PRODUCT, &
				IC_TRANSACTION::LOCATION, &
				"19920801", &
				"")
		END IF

		IF IC_TRANSACTION::PRICE = 0.0
		THEN
			IF IC_TRANSACTION::CROSS_REF <> ""
			THEN
				IF AR_EXAM_CUSTOM( &
					IC_TRANSACTION::CROSS_REF, &
					AR_35CUSTOM_EXAM) = CMC$_NORMAL
				THEN
					XTYPE$ = AR_35CUSTOM_EXAM::TTYPE
				ELSE
					XTYPE$ = "DE"
				END IF
			END IF

			IC_TRANSACTION::PRICE = PC_READ_PRICE( &
				IC_TRANSACTION::PRODUCT, &
				IC_TRANSACTION::LOCATION, &
				XTYPE$, &
				"19920801", "", "", "")
		END IF

		WHEN ERROR IN
			PUT #IC_TRANSACTION.CH% IF ERRFLAG% = 0%
		USE
			FILENAME$ = "IC_TRANSACTION"
			CONTINUE HelpError
		END WHEN


		GOSUB InitICTransactionRec

	CASE "PRODUCT"
		IC_TRANSACTION::PRODUCT		= DTA$

	CASE "LOCATION"
		IC_TRANSACTION::LOCATION	= DTA$

	CASE "TRANS_DATE"
		IC_TRANSACTION::TRANS_DATE	= DTA$

	CASE "PRIMARY_REF"
		DTA$ = "" IF DTA$ = "0"
		IC_TRANSACTION::PRIMARY_REF	= DTA$

	CASE "CROSS_REF"
		IC_TRANSACTION::CROSS_REF	= DTA$

	CASE "SUBACCOUNT"
		IC_TRANSACTION::SUBACCOUNT	= DTA$

	CASE "LOT"
		IC_TRANSACTION::LOT		= DTA$

	CASE "STATIONMAN"
		IC_TRANSACTION::STATIONMAN	= DTA$

	CASE "TYPE_A"

		SELECT DTA$

		CASE "A"
			DTA$ = "RQ"

		CASE "C"
			DTA$ = "AD"

		CASE "G"
			DTA$ = "WR"

		CASE "M"
			DTA$ = "MA"

		CASE "O"
			DTA$ = "SO"

		CASE "P"
			DTA$ = "PO"

		CASE "R"
			DTA$ = "RE"

		CASE "S"
			DTA$ = "SA"

		CASE "T"
			DTA$ = "TR"

		CASE "W"
			DTA$ = "WO"

		END SELECT

		IC_TRANSACTION::TYPE_A		= DTA$

	CASE "QUANTITY_A"
		WHEN ERROR IN
			IC_TRANSACTION::QUANTITY_A	= VAL(DTA$)
		USE
			IC_TRANSACTION::QUANTITY_A	= 0.0
		END WHEN


	CASE "TYPE_B"
		SELECT DTA$

		CASE "A"
			DTA$ = "RQ"

		CASE "C"
			DTA$ = "AD"

		CASE "G"
			DTA$ = "WR"

		CASE "M"
			DTA$ = "MA"

		CASE "O"
			DTA$ = "SO"

		CASE "P"
			DTA$ = "PO"

		CASE "R"
			DTA$ = "RE"

		CASE "S"
			DTA$ = "SA"

		CASE "T"
			DTA$ = "TR"

		CASE "W"
			DTA$ = "WO"

		END SELECT

		IC_TRANSACTION::TYPE_B		= DTA$

	CASE "QUANTITY_B"
		WHEN ERROR IN
			IC_TRANSACTION::QUANTITY_B	= VAL(DTA$)
		USE
			IC_TRANSACTION::QUANTITY_B	= 0.0
		END WHEN

	CASE "COST"
		WHEN ERROR IN
			IC_TRANSACTION::COST		= VAL(DTA$)
		USE
			IC_TRANSACTION::COST		= 0.0
		END WHEN

	CASE "PRICE"
		WHEN ERROR IN
			IC_TRANSACTION::PRICE		= VAL(DTA$)
		USE
			IC_TRANSACTION::PRICE		= 0.0
		END WHEN


	CASE "TRANSACCT"
		IC_TRANSACTION::TRANSACCT	= DTA$

	CASE "POSTDATE"
		IC_TRANSACTION::POSTDATE	= DTA$

	CASE "POSTTIME"
		IC_TRANSACTION::POSTTIME	= DTA$

	CASE "BATCH"
		IC_TRANSACTION::BATCH		= DTA$

	END SELECT

	RETURN

 InitICTransactionRec:
	IC_TRANSACTION::PRODUCT		= ""
	IC_TRANSACTION::LOCATION	= ""
	IC_TRANSACTION::TRANS_DATE	= ""
	IC_TRANSACTION::PRIMARY_REF	= ""
	IC_TRANSACTION::CROSS_REF	= ""
	IC_TRANSACTION::SUBACCOUNT	= ""
	IC_TRANSACTION::LOT		= ""
	IC_TRANSACTION::STATIONMAN	= ""
	IC_TRANSACTION::TYPE_A		= ""
	IC_TRANSACTION::QUANTITY_A	= 0.0
	IC_TRANSACTION::TYPE_B		= ""
	IC_TRANSACTION::QUANTITY_B	= 0.0
	IC_TRANSACTION::COST		= 0.0
	IC_TRANSACTION::PRICE		= 0.0
	IC_TRANSACTION::TRANSACCT	= ""
	IC_TRANSACTION::POSTDATE	= "19920801"
	IC_TRANSACTION::POSTTIME	= "000000"
	IC_TRANSACTION::BATCH		= "CONV"

	ERRFLAG% = 0%

	RETURN

 ExitProgram:
	!*******************************************************************
	! Exit program
	!*******************************************************************

	CLOSE #ICRMS3.CH%
	CALL ASSG_FREECHANNEL(ICRMS3.CH%)

	GOTO 32767

	%PAGE

 HelpError:
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	RESUME HelpError

32767	END
