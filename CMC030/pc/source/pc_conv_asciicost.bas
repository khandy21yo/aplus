1	%TITLE "Customer Name/Address Maintenance"
	%SBTTL "PC_CONV_ASCIICOST"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2002 BY
	!
	! Software Solutions, Inc.
	! Idaho Falls, Idaho  83402
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
	! Software Solutions, Inc. assumes no responsibility for the
	! use or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_CONV_ASCIICOST/LINE
	!	$ LINK/EXEC:PC_EXE PC_CONV_ASCIICOST,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_CONV_ASCIICOST.OBJ;*
	!
	! Author:
	!
	!	10/11/2002 - Kevin Handy
	!
	! Modification history:
	!
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Maps
	!
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT) PD_PRODUCT_CDD PD_PRODUCT

	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP (PC_COST) PC_COST_CDD PC_COST

100	LINPUT "SOURCE FILE: "; INFILE$
	OPEN INFILE$ FOR INPUT AS FILE 1%

110	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.MOD"

120	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.MOD"

900	LOCATION$ = "010"
	COSTDATE$ = "20021010"

1000	!
	! Get one price from input file
	!
	WHEN ERROR IN
		LINPUT #1%, TEXT$
	USE
		CONTINUE 5000 IF ERR = 11%
		EXIT HANDLER
	END WHEN

1005	FOR LOOP% = 1% TO 5%

		I% = INSTR(1%, TEXT$, "	")
		I% = LEN(TEXT$) + 1% IF I% = 0%

		VALUE$(LOOP%) = TRM$(LEFT(TEXT$, I% - 1%))
		TEXT$ = RIGHT(TEXT$, I% + 1%)

	NEXT LOOP%

	IF INSTR(1%, INFILE$, "ONE")
	THEN
		PRODUCT$ = STRING$(6% - LEN(VALUE$(1%)), ASCII("0")) + &
			VALUE$(1%)
	ELSE
		PRODUCT$ = VALUE$(1%)
	END IF

	PRICE$ = VALUE$(5%)

	WHEN ERROR IN
		PRICE = VAL(PRICE$)
	USE
		PRINT "BAD PRICE FOR "; PRODUCT$; " - "; VALUE$(5%)
	END WHEN

1010	!
	! See if the product number really exists
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, KEY #0% EQ PRODUCT$, REGARDLESS
	USE
		PRINT "I";
		PRINT IF CCPOS(0%) >= 50%
		CONTINUE 1000
	END WHEN

1200	!
	! Look for any prices that will supercede this one
	!
	WHEN ERROR IN
		GET #PC_COST.CH%, &
			KEY #0% EQ PRODUCT$ + LOCATION$ + COSTDATE$
	USE
		PRINT "X";
		PRINT IF CCPOS(0%) >= 50%
		CONTINUE 1290
	END WHEN

1210	PC_COST::PRODUCT = PRODUCT$
	PC_COST::LOCATION = LOCATION$
	PC_COST::EFFDATE = COSTDATE$
	PC_COST::COST = PRICE

	UPDATE #PC_COST.CH%

	PRINT "U";
	PRINT IF CCPOS(0%) >= 50%

	GOTO 1000

1290	PC_COST::PRODUCT = PRODUCT$
	PC_COST::LOCATION = LOCATION$
	PC_COST::EFFDATE = COSTDATE$
	PC_COST::COST = PRICE

	PUT #PC_COST.CH%

	PRINT "+";
	PRINT IF CCPOS(0%) >= 50%

	GOTO 1000

5000	CLOSE PC_COST.CH%

32767	END
