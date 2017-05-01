	!
	! File Layout for: UTL.UTL_TERMS on 21-May-01
	!
	! Terms Definition File
	!

	RECORD UTL_TERMS_CDD
		! Element =
		!   Description = Terms Code
		STRING CODE = 2
		! Element =
		!   Description = Description
		STRING DESCR = 40
		! Element =
		!   Description = Discount percentage
		GFLOAT DISCOUNT
		! Element =
		!   Description = Days until become due
		WORD DUEDAYS
		! Element =
		!   Description = Day of month becomes due
		STRING DUEDATE = 2
		! Element =
		!   Description = Days discount is good for
		WORD DISCOUNTDAYS
		! Element =
		!   Description = Day discount becomes due
		STRING DISCOUNTDATE = 2
	END RECORD
