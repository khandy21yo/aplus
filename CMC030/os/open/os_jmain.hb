	!
	! File Layout for: OS.OS_JMAIN on 21-May-01
	!
	! Main Part Screen
	!

	RECORD OS_JMAIN_CDD
		! Element = TRANKEY
		!   Description = Transaction key
		STRING ORDNUM = 10
		! Element = LINE
		!   Description = Line
		STRING JLINE = 4
		! Element =
		!   Description = Class
		STRING CLASS = 8
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = QUANTITY
		!   Description = Quantity
		GFLOAT QUANTITY
		! Element =
		!   Description = Qty Invoiced
		GFLOAT INVOICED
		! Element =
		!   Description = Qty Completed
		GFLOAT COMPLETED
		! Element = PRICE
		!   Description = Price Per
		GFLOAT PRICE
		! Element = DESCRIPTION
		!   Description = Description
		STRING DESCRIPTION = 40
	END RECORD
