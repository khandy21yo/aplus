1	%TITLE "Search for a Group/Item (Shareable version)"
	%SBTTL "FIND_3PRINTGROUPITEM"
	%IDENT "V3.6a Calico"

	FUNCTION INTEGER FIND_3PRINTGROUPITEM(GROUP$, ITEM$, &
		PRINTX_CDD PRINTX)

	!
	! COPYRIGHT (C) 1992 BY
	! Computer Management Center, Idaho Falls, Idaho  83404
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
	!	.lm +5
	!	This function searches for a group/item and returns the
	!	position in the PRINT.ITEM$() array where the actual
	!	information is stored.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	GROUP$
	!		This passed string is the group the user is searching for.
	!
	!	ITEM$
	!		This passed string is the item the user is looking for.
	!
	!
	!	This function returns the position of the group/item in
	!	the PRINT.ITEM$() array
	!
	! Example:
	!
	!	GROUP% = FIND_3PRINTGROUPITEM("RQ","*")
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:FIND_3PRINTGROUPITEM/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP FIND_3PRINTGROUPITEM
	!	$ DELETE FIND_3PRINTGROUPITEM.OBJ;*
	!
	! Author:
	!
	!	10/20/92 - Kevin Handy
	!		Taken from FIND_PRNTGROUPITEM, and modified to
	!		pass PRINTX as a structure instead of through
	!		MAP variables.
	!
	! Modification History:
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

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:PRINT35.INC"

	!
	! External functions
	!
	EXTERNAL INTEGER FUNCTION COMP_STRING(STRING, STRING)

	%PAGE

	!
	! Search for group
	!
	GOTO 1000 IF GROUP$ = PRINTX::GROUPX(I%) &
		FOR I% = 1% TO PRINTX::GROUPS

	FIND_3PRINTGROUPITEM = 0%
	EXIT FUNCTION

1000	!
	! Search for item within group
	!
	GOTO 2000 IF COMP_STRING(TRM$(ITEM$), TRM$(PRINTX::ITEM(J%))) &
		FOR J% = (PRINTX::GROUPP(I%) AND 2047%) TO &
			(PRINTX::GROUPP(I% + 1%) AND 2047%) - 1%

	FIND_3PRINTGROUPITEM = 0%
	EXIT FUNCTION

2000	!
	! We have found the group/item
	!
	FIND_3PRINTGROUPITEM = J%

	END FUNCTION
