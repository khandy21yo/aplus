1	%TITLE "Read Product Price"
	%SBTTL "PC_READ_PRICE"
	%IDENT "V3.6a Calico"

	FUNCTION REAL PC_READ_PRICE(XPRODUCT$, XLOCATION$, XTYPE$, &
		XDATE$, XTIME$, EFFDATE$, EFFTIME$)

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
	! Computer Management Center assumes no responsibility for the use
	! or reliability of its software on equipment which is not supported
	! by Computer Management Center.
	!
	!++
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function returns price from the Price file and
	!	effective date and time
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Input:
	!
	!	XPRODUCT$ is a product number
	!	XLOCATION$   is a location number
	!	XTYPE$   is price type
	!	XDATE$   is date
	!	XTIME$   is time
	!
	! Output:
	!
	!	price of a product
	!	EFFDATE$   is eff date
	!	EFFTIME$   is eff time
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS PC_SOURCE:PC_READ_PRICE/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PC_READ_PRICE
	!	$ DELETE PC_READ_PRICE.OBJ;*
	!
	! Author:
	!
	!	08/05/87 - Frank Starman
	!
	! Modification history:
	!
	!	02/19/88 - Frank Starman
	!		Method is a four character string
	!		Open Price cost control file is in this function
	!
	!	04/09/88 - Frank Starman
	!		Read PC Method file
	!
	!	06/23/88 - Frank Starman
	!		Read price only
	!
	!	05/28/92 - Frank F. Starman
	!		Ignore time.
	!
	!	04/08/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	05/31/95 - Kevin Handy
	!		Don't free UTL_PROFILE channel, since we don't
	!		close the channel. This should speed up price
	!		lookups a lot since it now won't open the file
	!		every time this function is called.
	!
	!	06/01/95 - Kevin Handy
	!		Modified to use local copies of the date and time,
	!		so that this function doesn't change information
	!		possibly coming from a file.
	!
	!	02/14/96 - Kevin Handy
	!		Reformat source code.
	!
	!	04/02/97 - Kevin Handy
	!		Remove commented out code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	08/02/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	COM (CH_PC_PRICE_READ) PC_PRICE.CH%
	COM (CH_UTL_PROFILE_READ) UTL_PROFILE.CH%

	%PAGE

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.HB"
	MAP	(PC_PRICE)	PC_PRICE_CDD	PC_PRICE
	MAP	(PC_PRICE_INIT)	PC_PRICE_CDD	PC_PRICE_INIT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.HB"
	MAP	(UTL_PROFILE)	UTL_PROFILE_CDD	UTL_PROFILE

	DECLARE REAL PRICE

	!
	! Set initial value
	!
	PC_PRICE_INIT = PC_PRICE

	!
	! Make local copies of these variables so we don't change records
	! in an existing file
	!
	XDATE1$ = XDATE$ + ""
	XTIME1$ = XTIME$ + ""

	PRICE = 0.0
	INIT.LOCATION$ = XLOCATION$
	XDATE1$ = DATE_TODAY IF XDATE1$ = ""
	XTIME1$ = "000000" ! Ignore time
	EFFDATE$ = "00000000"
	EFFTIME$ = "000000"

1000	IF PC_PRICE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PC.OPEN]PC_PRICE.OPN"
		USE
			CONTINUE Price IF ERR = 5%
			FILENAME$ = "PC_PRICE"
			CONTINUE HelpError
		END WHEN
	END IF

1100	IF UTL_PROFILE.CH% <= 0%
	THEN
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_PROFILE.OPN"
			GET #UTL_PROFILE.CH%, RECORD 1%, REGARDLESS
		USE
			CONTINUE 2000 IF ERR = 5%
			FILENAME$ = "UTL_PROFILE"
			CONTINUE HelpError
		END WHEN
	END IF

2000	WHEN ERROR IN
		FIND #PC_PRICE.CH%, &
			KEY #1% EQ XTYPE$ + XPRODUCT$ + INIT.LOCATION$, &
			REGARDLESS
	USE
		CONTINUE Price IF ERR = 9%

		IF ERR = 155%
		THEN
			IF INIT.LOCATION$ = UTL_PROFILE::DEFLOCATION
			THEN
				CONTINUE Price
			ELSE
				INIT.LOCATION$ = UTL_PROFILE::DEFLOCATION
				RETRY
			END IF
		END IF
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	WHEN ERROR IN
		GET #PC_PRICE.CH%, REGARDLESS
	USE
		CONTINUE Price IF ERR = 11% OR ERR = 9%
		FILENAME$ = "PC_PRICE"
		CONTINUE HelpError
	END WHEN

	GOTO Price IF PC_PRICE::PRODUCT_NUM <> XPRODUCT$ OR &
		PC_PRICE::LOCATION <> INIT.LOCATION$ OR &
		PC_PRICE::PCTYPE <> XTYPE$

	GOTO Price IF PC_PRICE::XDATE > XDATE1$ OR &
		PC_PRICE::XDATE = XDATE1$ AND PC_PRICE::XTIME > XTIME1$

	PRICE = PC_PRICE::PRICECOST
	EFFDATE$ = PC_PRICE::XDATE
	EFFTIME$ = PC_PRICE::XTIME

	GOTO GetNextRec

 Price:
	PC_READ_PRICE = PRICE
	PC_PRICE = PC_PRICE_INIT

	EXIT FUNCTION

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO Price

	END FUNCTION
