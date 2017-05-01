1	%TITLE "Special to copy PLAZ costs to ROBI location"
	%SBTTL "PC_SPEC_COPYCOST"
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
	!	Copies PLAZ costs to ROBI location
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_SPEC_COPYCOST/LINE
	!	$ LINK/EXE=PC_EXE: PC_SPEC_COPYCOST, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PC_SPEC_COPYCOST.OBJ;*
	!
	! Author:
	!
	!	05/01/2005 - Kevin Handy
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
	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.HB"
	MAP	(PC_COST)	PC_COST_CDD	PC_COST


100	%INCLUDE "SOURCE:[PC.OPEN]PC_COST.MOD"

1000	RESET #PC_COST.CH%

2000	!
	! Scan for plaza costs
	!
	WHEN ERROR IN
		GET #PC_COST.CH%
	USE
		PRINT "Finished "; ERT$(ERR)
		CONTINUE 9000
	END WHEN

2100	GOTO 2000 IF PC_COST::LOCATION <> "PLAZ"

	!
	! Does it already exist?
	!
	PC_COST::LOCATION = "ROBI"

	WHEN ERROR IN
		FIND #PC_COST.CH%, &
			KEY #0% EQ PC_COST::PRODUCT + &
			PC_COST::LOCATION + &
			PC_COST::EFFDATE, &
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
		PUT #PC_COST.CH%
	USE
		PRINT "-";
		CONTINUE 2900
	END WHEN

	PRINT "+";

2900	!
	! Get next item
	!
	WHEN ERROR IN
		GET #PC_COST.CH%, &
			KEY #0% GT PC_COST::PRODUCT + &
			PC_COST::LOCATION + &
			PC_COST::EFFDATE
	USE
		PRINT "Done "; ERT$(ERR)
		CONTINUE 9000
	END WHEN

	GOTO 2000

9000	CLOSE #PC_COST.CH%

32767	END
