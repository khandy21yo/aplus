1	%TITLE "Get Code List"
	%SBTTL "READ_CODELIST"
	%IDENT "V3.6a Calico"

	SUB READ_CODELIST(REFERENCE$, &
		CODE_TITLE$, &
		CODE_LIST$(), &
		STATUS%)

	!
	!	COPYRIGHT (C) 1987 BY
	!	Computer Management Center
	!	Idaho Falls, Idaho
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
	! Computer Management Center
	!
	! Computer Management Center assumes no responsibility for the use
	! or reliability of its software on equipment which is not supported
	! by Computer Management Center.
	!
	!++
	!
	! Abstract:HELP
	!	.lm +5
	!	.b
	!	This function returns the code title and a code list from
	!	the EDI table for a specific reference _#.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	REFERENCE$
	!		EDI element reference #, Format=######
	!
	!	CODE_TITLE$
	!		Returned code title
	!
	!	CODE_LIST$()
	!		Returned code list
	!
	!	STATUS%
	!		Returned : 0 = Successful, -1 = Failed
	!
	! Example:
	!
	!	CALL READ_CODELIST("000320", &
	!		CODE_TITLE$, &
	!		CODE_LIST$(), &
	!		STATUS%)
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:READ_CODELIST/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP READ_CODELIST
	!	$ DELETE READ_CODELIST.OBJ;*
	!
	! Author:
	!
	!	03/15/88 - Robert Peterson
	!
	! Modification history:
	!
	!	03/12/92 - Kevin Handy
	!		Unwrapped error trapping
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/23/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/30/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_EDI_DATAELEMENT.HB"
	MAP (UTL_EDI_DATAELEMENT) UTL_EDI_DATAELEMENT_CDD UTL_EDI_DATAELEMENT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_EDI_CODELIST.HB"
	MAP (UTL_EDI_CODELIST) UTL_EDI_CODELIST_CDD UTL_EDI_CODELIST

	%PAGE

	ON ERROR GOTO 19000

	REFERENCE$ = LEFT(REFERENCE$ + SPACE$(6%), 6%)

	CODE_LOOP% = 0%
	MMAX% = 6%
	STATUS% = 0%

300	!
	! Open the EDI Data Element
	!
	IF UTL_EDI_DATAELEMENT.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_EDI_DATAELEMENT.OPN"
		USE
			CONTINUE 310 IF ERR = 5%
			EXIT HANDLER
		END WHEN
	END IF

310	!
	! Open the EDI Code List
	!
	IF UTL_EDI_CODELIST.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_EDI_CODELIST.OPN"
		USE
			CONTINUE 1000 IF ERR = 5%
			EXIT HANDLER
		END WHEN
	END IF

1000	!
	! Get the title from the data element file
	!
	WHEN ERROR IN
		GET #UTL_EDI_DATAELEMENT.CH%, KEY #0% EQ REFERENCE$, REGARDLESS
	USE
		STATUS% = -1%
		CONTINUE 3000 IF ERR = 155% OR ERR = 9%
		EXIT HANDLER
	END WHEN

	STATUS% = 0%

	CODE_TITLE$ = UTL_EDI_DATAELEMENT::TITLE

	MMAX% = UTL_EDI_DATAELEMENT::MMAX

2000	!
	! Get the code list from the code list file
	!
	STATUS% = -1%

	WHEN ERROR IN
		FIND #UTL_EDI_CODELIST.CH%, KEY #0% GE REFERENCE$, REGARDLESS
	USE
		CONTINUE 3000 IF ERR = 155% OR ERR = 9%
		EXIT HANDLER
	END WHEN

	STATUS% = 0%

2030	WHEN ERROR IN
		GET #UTL_EDI_CODELIST.CH%, REGARDLESS
	USE
		CONTINUE 3000 IF ERR = 11%
		EXIT HANDLER
	END WHEN

	GOTO 3000 IF UTL_EDI_CODELIST::REFERENCE <> REFERENCE$

	CODE_LOOP% = CODE_LOOP% + 1%

	CODE_LIST$(CODE_LOOP%) = &
		LEFT(UTL_EDI_CODELIST::CODE + SPACE$(6%), MMAX%) + " " + &
		EDIT$(UTL_EDI_CODELIST::DESCR, 140%)

	GOTO 2030

3000	CODE_LIST$(0%) = NUM1$(CODE_LOOP%)

	CLOSE UTL_EDI_DATAELEMENT.CH%, UTL_EDI_CODELIST.CH%

	CALL ASSG_FREECHANNEL(UTL_EDI_DATAELEMENT.CH%)
	CALL ASSG_FREECHANNEL(UTL_EDI_CODELIST.CH%)

 ExitSub:

	EXIT SUB

	%Page

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	ON ERROR GO BACK

	END SUB
