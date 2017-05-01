	!
	! File Layout for: AD.AD_CONVENTION on 21-May-01
	!
	! Convention Description
	!

	RECORD AD_CONVENTION_CDD
		! Element =
		!   Description = Convention code
		STRING CONVENTION = 2
		! Element =
		!   Description = Description
		STRING DESCRIPTION = 40
		! Element =
		!   Description = Number months dep in the first year
		WORD COEFF
		! Element =
		!   Description = Specification (regardless,..)
		STRING SPECIFIC = 1
	END RECORD
