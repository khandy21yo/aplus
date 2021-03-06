1	!
	! Special program to convert PP_monthly from old to new format
	!
	! History:
	!	10/26/96 - Kevin Handy
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[PP.OPEN]PP_monthly.HB"
	MAP (PP_monthly) PP_monthly_CDD PP_monthly

	!
	! File Layout for: PP.PP_MONTHLY
	!
	! Pacific Pride Monthly Transaction File
	!
	RECORD PP_MONTHLY_OLD_CDD
		! Element = CUSTOMER
		! Description = Customer Number
		STRING CUSNUM = 10
		! Element = CARD
		! Description = Pacific Pride Vehicle Card Number
		STRING VEHICLE = 8
		! Element = CARD
		! Description = Pacific Pride Driver Card Number
		STRING DRIVER = 8
		! Element = DATE
		! Description = Transaction Date (YYYYMMDD)
		STRING TRANDATE = 8
		! Element = TIME
		! Description = Transaction Time (HHMMSS)
		STRING TRANTIME = 6
		! Element =
		! Description = Host Number
		STRING HOST = 3
		! Element =
		! Description = Site Number
		STRING SITE = 2
		! Element =
		! Description = Site Type
		STRING STYPE = 1
		! Element = PRODUCT
		! Description = Product Number
		STRING PRODUCT = 14
		! Element = UOM
		! Description = Unit of measurement
		STRING UOM = 2
		! Element =
		! Description = Quantity Sold
		GFLOAT QUANTITY
		! Element =
		! Description = Odometer reading
		GFLOAT ODOM
		! Element =
		! Description =
		STRING SLTYPE = 1
		! Element =
		! Description =
		STRING FTYPE = 1
		! Element =
		! Description = Selling Price
		GFLOAT SELLPRICE
		! Element =
		! Description = Transaction Cost
		GFLOAT TRANCOST
		! Element =
		! Description = Misc. keyboard entry
		STRING MISCKEYB = 9
		! Element =
		! Description =
		STRING TRNTYPE = 2
		! Element =
		! Description = Discount
		STRING DISCOUNT = 4
		! Element = DATE
		! Description = icb Date (YYYYMMDD)
		STRING ICBDATE = 8
		! Element =
		! Description = Transaction number
		STRING TRNNUM = 5
		! Element =
		! Description = Sales Tax Rate
		GFLOAT STAXRATE
		! Element =
		! Description = Pump Number
		STRING PUMP = 2
		! Element =
		! Description =
		STRING BUYFRAN = 3
		! Element = DATE
		! Description = Capture Date (YYYYMMDD)
		STRING CAPDATE = 8
		! Element = TIME
		! Description = Capture Time (HHMMSS)
		STRING CAPTIME = 6
		! Element =
		! Description =
		STRING POSTBNUM = 4
		! Element =
		! Description =
		STRING TRANSOURCE = 1
		! Element =
		! Description =
		STRING EDITACT = 1
		! Element =
		! Description =
		STRING JULIANDAY = 3
		! Element =
		! Description =
		STRING RSTATION = 1
		! Element = STATE
		! Description = State
		STRING STATE = 2
		! Element = BATCH
		! Description = Batch number used for process (post,clos
		STRING BATCH = 6
		! Element =
		! Description = PP Customer Number
		STRING IDENTITY = 6
	END RECORD

	MAP (PP_monthly_OLD) PP_monthly_OLD_CDD PP_monthly_OLD

	input "Period (YYYYPP)"; YYYY_PP$

100 !	NAME "PP_monthly_" + YYYY_PP$ + ".LED" AS &
 !		"PP_monthly_" + YYYY_PP$ + ".LED_OLD"

120	%INCLUDE "SOURCE:[PP.OPEN]PP_monthly.CRE"

200	!======================================================================
	! PP_MONTHLY file (open read only)
	!======================================================================

	CALL ASSG_CHANNEL(PP_MONTHLY_OLD.CH%, STAT%)

	PP_MONTHLY_OLD.NAME$ = &
		PP_MONTHLY.DEV$ + "PP_MONTHLY_" + YYYY_PP$ + ".LED_OLD"

	OPEN PP_MONTHLY_OLD.NAME$ FOR INPUT AS FILE PP_MONTHLY_OLD.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PP_MONTHLY_OLD, &
		PRIMARY KEY &
		( &
			PP_MONTHLY_OLD::CUSNUM, &
			PP_MONTHLY_OLD::VEHICLE, &
			PP_MONTHLY_OLD::TRANDATE, &
			PP_MONTHLY_OLD::TRANTIME &
		)	DUPLICATES, &
		ALTERNATE KEY &
		( &
			PP_MONTHLY_OLD::CUSNUM, &
			PP_MONTHLY_OLD::DRIVER, &
			PP_MONTHLY_OLD::TRANDATE, &
			PP_MONTHLY_OLD::TRANTIME &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			PP_MONTHLY_OLD::HOST, &
			PP_MONTHLY_OLD::SITE, &
			PP_MONTHLY_OLD::STYPE, &
			PP_MONTHLY_OLD::TRANDATE, &
			PP_MONTHLY_OLD::TRANTIME &
		)	DUPLICATES CHANGES, &
		ALTERNATE KEY &
		( &
			PP_MONTHLY_OLD::BATCH, &
			PP_MONTHLY_OLD::CUSNUM &
		)	DUPLICATES CHANGES, &
		ACCESS READ, ALLOW MODIFY


300	RESET #PP_monthly_OLD.CH%

320	GET #PP_monthly_OLD.CH%

330
	pp_monthly::CUSNUM	= pp_monthly_old::CUSNUM
	pp_monthly::VEHICLE	= pp_monthly_old::VEHICLE
	pp_monthly::DRIVER	= pp_monthly_old::DRIVER
	pp_monthly::TRANDATE	= pp_monthly_old::TRANDATE
	pp_monthly::TRANTIME	= pp_monthly_old::TRANTIME
	pp_monthly::HOST	= pp_monthly_old::HOST
	pp_monthly::SITE	= pp_monthly_old::SITE
	pp_monthly::STYPE	= pp_monthly_old::STYPE
	pp_monthly::PRODUCT	= pp_monthly_old::PRODUCT
	pp_monthly::UOM		= pp_monthly_old::UOM
	pp_monthly::QUANTITY	= pp_monthly_old::QUANTITY
	pp_monthly::ODOM	= pp_monthly_old::ODOM
	pp_monthly::SLTYPE	= pp_monthly_old::SLTYPE
	pp_monthly::FTYPE	= pp_monthly_old::FTYPE
	pp_monthly::SELLPRICE	= pp_monthly_old::SELLPRICE
	pp_monthly::TRANCOST	= pp_monthly_old::TRANCOST
	pp_monthly::MISCKEYB	= pp_monthly_old::MISCKEYB
	pp_monthly::TRNTYPE	= pp_monthly_old::TRNTYPE
	pp_monthly::DISCOUNT	= pp_monthly_old::DISCOUNT
	pp_monthly::ICBDATE	= pp_monthly_old::ICBDATE
	pp_monthly::TRNNUM	= pp_monthly_old::TRNNUM
	pp_monthly::STAXRATE	= pp_monthly_old::STAXRATE
	pp_monthly::PUMP	= pp_monthly_old::PUMP
	pp_monthly::BUYFRAN	= pp_monthly_old::BUYFRAN
	pp_monthly::CAPDATE	= pp_monthly_old::CAPDATE
	pp_monthly::CAPTIME	= pp_monthly_old::CAPTIME
	pp_monthly::POSTBNUM	= pp_monthly_old::POSTBNUM
	pp_monthly::TRANSOURCE	= pp_monthly_old::TRANSOURCE
	pp_monthly::EDITACT	= pp_monthly_old::EDITACT
	pp_monthly::JULIANDAY	= pp_monthly_old::JULIANDAY
	pp_monthly::RSTATION	= pp_monthly_old::RSTATION
	pp_monthly::STATE	= pp_monthly_old::STATE
	pp_monthly::BATCH	= pp_monthly_old::BATCH
	pp_monthly::IDENTITY	= pp_monthly_old::IDENTITY

	PUT #PP_monthly.CH%

	GOTO 320

32767	END
