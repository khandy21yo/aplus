	!
	! File Layout for: PP.PP_DISCOUNT on 21-May-01
	!
	! Discount Table
	!

	RECORD PP_DISCOUNT_CDD
		! Element =
		!   Description = Discount Code (A,I??)
		STRING CODE = 4
		! Element = DESCRIPTION
		!   Description = Description
		STRING DESCRIPTION = 40
		! Element =
		!   Description = Method (V=Volume,N=Nonvolume)
		STRING METHOD = 1
		! Element =
		!   Description = Over Amount
		GFLOAT OVER(9)
		! Element =
		!   Description = Rate
		GFLOAT RATE(9)
	END RECORD
