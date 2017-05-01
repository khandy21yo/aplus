1	%TITLE "Read Inventory Balance File"
	%SBTTL "IC_READ_35BALANCE"
	%IDENT "V3.6a Calico"

	FUNCTION LONG IC_READ_35BALANCE(XPRODUCT$, &
		XLOCATION$, &
		REAL XBALANCE(,))

	!
	! COPYRIGHT (C) 1987, 1988 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho
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
	!	.p
	!	This function returns Product location balances
	!
	! Index:
	!
	! Option:
	!
	! Input:
	!
	!	XPRODUCT$ is a product number
	!	XLOCATION$ is a location number
	!	XBALANCE(3%,3%)  balance array for each class and record type
	!		XBALANCE(X%,1%) = Begin Balance
	!		XBALANCE(X%,2%) = Posted Balance
	!		XBALANCE(X%,3%) = Running Balance
	!		XBALANCE(1%,X%) = On Hand
	!		XBALANCE(2%,X%) = Allocated
	!		XBALANCE(3%,X%) = On Order
	!
	! Output:
	!
	!	Balance array
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_READ_35BALANCE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP IC_READ_35BALANCE
	!	$ DELETE IC_READ_35BALANCE.OBJ;*
	!
	! Author:
	!
	!	05/11/88 - Frank F. Starman
	!
	! Modification history:
	!
	!	07/19/88 - Frank F. Starman
	!		Do not need transaction type file
	!
	!	08/09/90 - Frank F. Starman
	!		Change from subroutine to the function.
	!
	!	11/21/90 - Val Allen
	!		Changed to put the transaction type file back in
	!
	!	12/18/90 - Val James Allen
	!		Changed to drive off of the IC_35BALANCE field
	!		TRANSTYPE
	!
	!	01/02/91 - Frank F. Starman
	!		Read a new file layout (without period field).
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards.
	!		Lose extra parameter to ASSG_FREECHANNEL.
	!
	!	09/05/95 - Kevin Handy
	!		Reformat closer to 80 columns.
	!
	!	10/18/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	03/23/98 - Kevin Handy
	!		Put an IF around UTL_TRANSTYPE lookup.
	!		Should reduce disk access slightly.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/27/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	01/09/2000 - Kevin Handy
	!		Load the UTL_TRANSTYPE file into an array instead of
	!		doing searches into the indexed file. Let's see if
	!		that makes it run faster.
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	COM (CH_IC_35BALANCE_READ) IC_35BALANCE.CH%
	COM (CH_UTL_TRANSTYPE_READ) UTL_TRANSTYPE.CH%

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.HB"
	MAP (IC_35BALANCE)	IC_35BALANCE_CDD	IC_35BALANCE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.HB"
	MAP (UTL_TRANSTYPE)	UTL_TRANSTYPE_CDD	UTL_TRANSTYPE

	MAP (XXX_UTL_TRANSTYPE) &
		LONG UTL_TRANSTYPE_COUNT, &
		UTL_TRANSTYPE_CDD UTL_TRANSTYPE_ARRAY(20%)

	DECLARE LONG EXIT_STATUS

	%PAGE

	ON ERROR GOTO 19000

	!
	! Set initial value
	!
	XBALANCE(I%, J%) = 0.0 &
		FOR I% = 0% TO 3% &
		FOR J% = 0% TO 3%

	!
	! Assume cannot find any balance
	!
	EXIT_STATUS = CMC$_WARNING

1000	IF IC_35BALANCE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "IC_35BALANCE"
			CONTINUE HelpError
		END WHEN
	END IF

1010	IF UTL_TRANSTYPE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_TRANSTYPE.OPN"
		USE
			CONTINUE ExitFunction IF ERR = 5%
			FILENAME$ = "UTL_TRANSTYPE"
			CONTINUE HelpError
		END WHEN

		UTL_TRANSTYPE_COUNT = 0%
	END IF

	!
	! LOAD IN THE TRABSTYPE_ARRAY (once only)
	!
	IF UTL_TRANSTYPE_COUNT = 0%
	THEN
		RESET #UTL_TRANSTYPE.CH%

		WHILE (UTL_TRANSTYPE_COUNT < 20%)
			WHEN ERROR IN
				GET #UTL_TRANSTYPE.CH%
				UTL_TRANSTYPE_ARRAY(UTL_TRANSTYPE_COUNT) = &
					UTL_TRANSTYPE
				UTL_TRANSTYPE_COUNT = UTL_TRANSTYPE_COUNT + 1%
			USE
				CONTINUE 2000
			END WHEN
		NEXT
	END IF

2000	WHEN ERROR IN
		FIND #IC_35BALANCE.CH%, &
			KEY #0% EQ XPRODUCT$ + XLOCATION$, &
			REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11% OR ERR = 155% OR ERR = 9%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	WHEN ERROR IN
		GET #IC_35BALANCE.CH%, REGARDLESS
	USE
		CONTINUE ExitFunction IF ERR = 11% OR ERR = 155% OR ERR = 9%
		FILENAME$ = "IC_35BALANCE"
		CONTINUE HelpError
	END WHEN

	GOTO ExitFunction &
		IF IC_35BALANCE::PRODUCT <> XPRODUCT$ OR &
		IC_35BALANCE::LOCATION <> XLOCATION$

	X% = 0%

2010	IF UTL_TRANSTYPE::CODE <> IC_35BALANCE::TRANSTYPE
	THEN
 !		WHEN ERROR IN
 !			GET #UTL_TRANSTYPE.CH%, &
 !				KEY #0% EQ IC_35BALANCE::TRANSTYPE, &
 !				REGARDLESS
 !		USE
 !			CONTINUE CreateArray IF ERR = 155%
 !			FILENAME$ = "UTL_TRANSTYPE"
 !			CONTINUE HelpError
 !		END WHEN

		!
		! Try this code to lookup the table to see if it runs faster
		!
		FOR I% = 0% TO UTL_TRANSTYPE_COUNT - 1%

			IF UTL_TRANSTYPE_ARRAY(I%)::CODE = &
				IC_35BALANCE::TRANSTYPE
			THEN
				UTL_TRANSTYPE = UTL_TRANSTYPE_ARRAY(I%)
				X% = VAL%(UTL_TRANSTYPE::CLASS)
				GOTO CreateArray
			END IF

			IF UTL_TRANSTYPE_ARRAY(I%)::CODE > &
				IC_35BALANCE::TRANSTYPE
			THEN
				GOTO CreateArray
			END IF
		NEXT I%

		X% = 0%
		GOTO CreateArray
	END IF

	X% = VAL%(UTL_TRANSTYPE::CLASS)

 CreateArray:
	XBALANCE(X%, 1%) = XBALANCE(X%, 1%) + IC_35BALANCE::BBALANCE
	XBALANCE(X%, 2%) = XBALANCE(X%, 2%) + IC_35BALANCE::PBALANCE
	XBALANCE(X%, 3%) = XBALANCE(X%, 3%) + IC_35BALANCE::RBALANCE

	EXIT_STATUS = CMC$_NORMAL

	GOTO GetNextRec

 ExitFunction:
	IC_READ_35BALANCE = EXIT_STATUS
	EXIT FUNCTION

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	CALL ASSG_FREECHANNEL(IC_35BALANCE.CH%)
	CALL ASSG_FREECHANNEL(UTL_TRANSTYPE.CH%)
	IC_35BALANCE.CH% = 0%
	UTL_TRANSTYPE.CH% = 0%

	EXIT_STATUS = CMC$_UNTERROR
	GOTO ExitFunction

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

	END FUNCTION
