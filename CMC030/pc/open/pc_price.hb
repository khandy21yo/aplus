	!
	! File Layout for: PC.PC_PRICE on 21-May-01
	!
	! Product Price
	!

	RECORD PC_PRICE_CDD
		! Element = PRODUCT_NUM
		!   Description = Product number
		STRING PRODUCT_NUM = 14
		! Element = LOCATION
		!   Description = Location number
		STRING LOCATION = 4
		! Element = PCTYPE
		!   Description = Price cost type
		STRING PCTYPE = 2
		! Element = DATE
		!   Description = Date (MMDDYYYY)
		STRING XDATE = 8
		! Element = TIME
		!   Description = Time (HHMMSS)
		STRING XTIME = 6
		! Element = AMOUNT
		!   Description = Dollar amount.
		GFLOAT PRICECOST
	END RECORD
