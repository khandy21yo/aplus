	!
	! File Layout for: PO.PO_RECLINE on 21-May-01
	!
	! Purchase Order Receiver Line Journal
	!

	RECORD PO_RECLINE_CDD
		! Element = PO
		!   Description = Purchase order number
		STRING PO = 10
		! Element =
		!   Description = Purchase Order Line
		STRING PO_LINE = 4
		! Element = DESCRIPTION
		!   Description = Description
		STRING DESCRIPTION = 40
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = UOM
		!   Description = Unit of measurement
		STRING UOM = 2
		! Element =
		!   Description = Receive Quantity
		GFLOAT RECQTY
		! Element =
		!   Description = Canceled Quantity
		GFLOAT CANQTY
		! Element = FLAG
		!   Description = Line Flag = Y if Line in Register
		STRING LINEFLAG = 1
	END RECORD
