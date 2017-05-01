	!
	! File Layout for: AD.AD_CLASS on 21-May-01
	!
	! Asset Classes
	!

	RECORD AD_CLASS_CDD
		! Element =
		!   Description = Asset class
		STRING ASSCLASS = 6
		! Element =
		!   Description = Description
		STRING DESCRIPTION = 40
		! Element =
		!   Description = Class life
		WORD LIFE
		! Element =
		!   Description = General depreciation system
		WORD GDS
		! Element =
		!   Description = Alternative depreciation system
		WORD ADS
	END RECORD
