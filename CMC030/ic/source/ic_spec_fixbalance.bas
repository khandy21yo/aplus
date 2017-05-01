1	%TITLE "Try to recover a balance file"
	%SBTTL "IC_SPEC_FIXBALANCE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2002 BY
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
	! ID:IC028
	!
	! Abstract:HELP
	!
	!	Tries to recover records from the balance file when there
	!	are bad records in the file.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_SPEC_FIXBALANCE
	!	$ LINK/EXE=IC_EXE: IC_SPEC_FIXBALANCE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_SPEC_FIXBALANCE.OBJ;*
	!
	! Author:
	!
	!	02/05/2002 - Kevin Handy
	!
	! Modification history:
	!
	!--

	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)


	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.HB"
	MAP (IC_35BALANCE) IC_35BALANCE_CDD IC_35BALANCE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT) PD_PRODUCT_CDD PD_PRODUCT


1000	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"

2000	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.OPN"

	IC_35BALANCE.CH_OLD% = IC_35BALANCE.CH%
	IC_35BALANCE.CH% = 0%

2010	IC_35BALANCE.DEV$ = "$DISK2:[ICKBI.TEMP]"
 !	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.CRE"
	!======================================================================
	! IC_35BALANCE file (create, open read/write)
	!======================================================================

	CALL ASSG_CHANNEL(IC_35BALANCE.CH%, STAT%)
	CALL READ_DEVICE('IC_35BALANCE',IC_35BALANCE.DEV$, STAT%)
	CALL READ_PROTECTION('IC_35BALANCE', IC_35BALANCE.PRO$, STAT%)
	CALL READ_CURPROTECTION(OLD_PROT$, STAT%)
	CALL WRIT_CURPROTECTION(IC_35BALANCE.PRO$, STAT%)

	IC_35BALANCE.NAME$ = IC_35BALANCE.DEV$ + "IC_35BALANCE.HIS"

	OPEN IC_35BALANCE.NAME$ AS FILE IC_35BALANCE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP IC_35BALANCE, &
		BUFFER 32%, &
		PRIMARY KEY &
		( &
			IC_35BALANCE::PRODUCT, &
			IC_35BALANCE::LOCATION, &
			IC_35BALANCE::TRANSTYPE &
		), &
		ALTERNATE KEY &
		( &
			IC_35BALANCE::LOCATION, &
			IC_35BALANCE::PRODUCT &
		)	DUPLICATES CHANGES, &
		ACCESS MODIFY, ALLOW NONE

	CALL WRIT_CURPROTECTION(OLD_PROT$, STAT%)


	IC_35BALANCE.CH_NEW% = IC_35BALANCE.CH%
	IC_35BALANCE.CH% = 0%

	RESET #PD_PRODUCT.CH%

3000	!
	! Loop through product file
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%
	USE
		PRINT "END OF PRODUCT FILE"
		CONTINUE 9000
	END WHEN

	PRINT PD_PRODUCT::PRODUCT_NUM

4000	WHEN ERROR IN
		FIND #IC_35BALANCE.CH_OLD%, KEY #0% EQ PD_PRODUCT::PRODUCT_NUM
	USE
		CONTINUE 3000
	END WHEN

4100	WHEN ERROR IN
		GET #IC_35BALANCE.CH_OLD%, REGARDLESS
	USE
		PRINT "ERROR", ERR, " IN PRODUCT ", PD_PRODUCT::PRODUCT_NUM
		CONTINUE 3000
	END WHEN


4200	IF IC_35BALANCE::PRODUCT = PD_PRODUCT::PRODUCT_NUM
	THEN
		PUT #IC_35BALANCE.CH_NEW%
		GOTO 4100
	ELSE
		GOTO 3000
	END IF

9000	!
	! Finally done
	!
	CLOSE IC_35BALANCE.CH_NEW%

32767	END
