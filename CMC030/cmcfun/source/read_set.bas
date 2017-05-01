1	%TITLE "Get Device Name from SET File"
	%SBTTL "READ_SET"
	%IDENT "V3.6a Calico"

	FUNCTION STRING READ_SET(GROUP$, ITEM$)
	!
	! COPYRIGHT (C) 1986 BY
	! Computer Management Center
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function reads the SET file to get the device
	!	which the file exists on. Will default device if it
	!	does not exist in the file.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	GROUP$
	!		The passed file name the user wants to get the device off
	!		of.
	!
	!	ITEM$
	!		The particular passed section of data on that file.
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:READ_SET/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP READ_SET
	!	$ DELETE READ_SET.OBJ;*
	!
	! AUTHOR:
	!
	!	09/26/84 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!	07/19/85 - Kevin Handy
	!		Modified to read SET file instead of DEVICE file.
	!
	!	08/12/88 - Kevin Handy
	!		Modified to open set file if it hasn't been yet.
	!
	!	04/02/92 - Frank F. Starman
	!		Return utl_set record thru an argument.
	!		Change function from string to long.
	!
	!	04/22/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/17/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/23/97 - Kevin Handy
	!		Use integer for #key
	!
	!	01/13/98 - Kevin Handy
	!		Lost extra parameter that Frank had created
	!		on 04/02/92, but which was never used.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/04/99 - Kevin Handy
	!		Change field 'PROGRAM' to 'PROGRAMNAME'
	!
	!	06/14/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.HB"
	MAP (UTL_SET) UTL_SET_CDD UTL_SET

	COM (CH_UTL_SET) &
		UTL_SET.CH%, &
		UTL_SET.READONLY%

	DECLARE LONG EXIT_STATUS

	!
	! Assume undefined
	!
	EXIT_STATUS = CMC$_UNDEFINED
	V$ = ""

100	!
	! Open set file if hasn't been done yet
	!
	IF (UTL_SET.CH% = 0%)
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_SET.OPN"
		USE
			UTL_SET.CH% = 0%
			CALL ASSG_FREECHANNEL(UTL_SET.CH%)
			EXIT_STATUS = CMC$_UNTERROR
			CONTINUE 2000
		END WHEN

		UTL_SET.READONLY% = -1%
	END IF

	!
	! Try to read device name from DEVICE
	!
	A$ = SPACE$(LEN(UTL_SET::PROGRAMNAME))
	LSET A$ = GROUP$
	B$ = SPACE$(LEN(UTL_SET::ITEM))
	LSET B$ = ITEM$

1000	WHEN ERROR IN
		GET #UTL_SET.CH%, KEY #0% EQ A$ + B$, REGARDLESS
	USE
		EXIT_STATUS = CMC$_UNDEFINED
		CONTINUE 2000
	END WHEN

	V$ = UTL_SET::SDATA
	EXIT_STATUS = CMC$_NORMAL

2000	READ_SET = V$
	END FUNCTION
