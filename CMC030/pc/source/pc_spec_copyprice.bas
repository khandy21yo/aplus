1	%TITLE "Special to copy PLAZ PRICEs to ROBI location"
	%SBTTL "PC_SPEC_COPYPRICE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2005 BY
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
	!	.p
	!	Copies PLAZ PRICEs to ROBI location
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_SPEC_COPYPRICE/LINE
	!	$ LINK/EXE=PC_EXE: PC_SPEC_COPYPRICE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_SPEC_COPYPRICE.OBJ;*
	!
	! Author:
	!
	!	05/09/2005 - Kevin Handy
	!
	! Modification history:
	!
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map file
	!
	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.HB"
	MAP	(PC_PRICE)	PC_PRICE_CDD	PC_PRICE


100	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.MOD"

1000	RESET #PC_PRICE.CH%, KEY #1%

2000	!
	! Scan for plaza PRICEs
	!
	WHEN ERROR IN
		GET #PC_PRICE.CH%
	USE
		PRINT "Finished "; ERT$(ERR)
		CONTINUE 9000
	END WHEN

2100	GOTO 2000 IF PC_PRICE::LOCATION <> "PLAZ"

	!
	! Does it already exist?
	!
	PC_PRICE::LOCATION = "ROBI"

	WHEN ERROR IN
		FIND #PC_PRICE.CH%, &
			KEY #0% EQ PC_PRICE::PCTYPE + &
			PC_PRICE::PRODUCT_NUM + &
			PC_PRICE::LOCATION + &
			PC_PRICE::XDATE, &
			REGARDLESS
	USE
		CONTINUE 2200
	END WHEN

	PRINT "|";

	GOTO 2900

2200	!
	! Try to add new one with ROBI location
	!
	PRINT IF CCPOS(0%) >= 50%

	WHEN ERROR IN
		PUT #PC_PRICE.CH%
	USE
		PRINT "-";
		CONTINUE 2900
	END WHEN

	PRINT "+";

2900	!
	! Get next item
	!
	WHEN ERROR IN
		GET #PC_PRICE.CH%, &
			KEY #1% GT PC_PRICE::PCTYPE + &
			PC_PRICE::PRODUCT_NUM + &
			PC_PRICE::LOCATION + &
			PC_PRICE::XDATE
	USE
		PRINT "Done "; ERT$(ERR)
		CONTINUE 9000
	END WHEN

	GOTO 2000

9000	CLOSE #PC_PRICE.CH%

32767	END
