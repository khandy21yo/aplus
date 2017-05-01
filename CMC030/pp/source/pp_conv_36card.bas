1	!
	! Special program to convert PP_CARD from old to new format
	!
	! History:
	!	10/26/96 - Kevin Handy
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.HB"
	MAP (PP_CARD) PP_CARD_CDD PP_CARD

	!
	! File Layout for: PP.PP_CARD
	!
	! Pacific Pride Card Number File
	!
	RECORD PP_CARD_OLD_CDD
		! Element = CUSTOMER
		! Description = Customer Number
		STRING CUSNUM = 10
		! Element =
		! Description = Card Number
		STRING CARD = 8
		! Element =
		! Description = Card Type (1=drv,2=2nd drv,3=veh)
		STRING CTYPE = 1
		! Element = DESCRIPTION
		! Description = Description
		STRING DESCRIPTION = 40
		! Element =
		! Description = Beginning Odometer Reading
		GFLOAT ODOMETER
		! Element =
		! Description = Pacific Pride Customer Number
		STRING SYSCUS = 6
		! Element =
		! Description = Discount Code
		STRING DISCOUNT = 4
	END RECORD

	MAP (PP_CARD_OLD) PP_CARD_OLD_CDD PP_CARD_OLD

100 !	NAME "PP_CARD.MAS" AS "PP_CARD.MAS_OLD"

120	%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.CRE"

200	!======================================================================
	! PP_CARD file (open read only)
	!======================================================================

	CALL ASSG_CHANNEL(PP_CARD_OLD.CH%, STAT%)
	PP_CARD_OLD.NAME$ = PP_CARD.DEV$ + "PP_CARD.MAS_OLD"

	OPEN PP_CARD_OLD.NAME$ FOR INPUT AS FILE PP_CARD_OLD.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PP_CARD_OLD, &
		PRIMARY KEY &
		( &
			PP_CARD_OLD::CUSNUM, &
			PP_CARD_OLD::CARD &
		), &
		ALTERNATE KEY &
		( &
			PP_CARD_OLD::SYSCUS, &
			PP_CARD_OLD::CARD &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			PP_CARD_OLD::CARD, &
			PP_CARD_OLD::CUSNUM &
		)	DUPLICATES CHANGES, &
		ACCESS READ, ALLOW MODIFY


300	RESET #PP_CARD_OLD.CH%

320	GET #PP_CARD_OLD.CH%

330	PP_CARD::CUSNUM		= PP_CARD_OLD::CUSNUM
	PP_CARD::CARD		= PP_CARD_OLD::CARD
	PP_CARD::CTYPE		= PP_CARD_OLD::CTYPE
	PP_CARD::DESCRIPTION	= PP_CARD_OLD::DESCRIPTION
	PP_CARD::ODOMETER	= PP_CARD_OLD::ODOMETER
	PP_CARD::SYSCUS		= PP_CARD_OLD::SYSCUS
	PP_CARD::DISCOUNT	= PP_CARD_OLD::DISCOUNT

	PUT #PP_CARD.CH%

	GOTO 320

32767	END
