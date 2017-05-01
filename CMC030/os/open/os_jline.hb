	!
	! File Layout for: OS.OS_JLINE on 21-May-01
	!
	! Lowest line level journal
	!

	RECORD OS_JLINE_CDD
		! Element = ORDNUM
		!   Description = Transaction key
		STRING ORDNUM = 10
		! Element = LINE
		!   Description = Line
		STRING JLINE = 4
		! Element = LINE
		!   Description = Line
		STRING LLINE = 4
		! Element = CATEGORY
		!   Description = Category for this item
		STRING CATEGORY = 4
		! Element = PRODUCT
		!   Description = Product Number Selected
		STRING PRODUCT = 14
		! Element = QUANTITY
		!   Description = Quantity
		GFLOAT QUANTITY
		! Element =
		!   Description = Quoted price
		GFLOAT PRICE
	END RECORD
