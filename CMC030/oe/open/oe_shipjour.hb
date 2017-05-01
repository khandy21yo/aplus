	!
	! File Layout for: OE.OE_SHIPJOUR on 21-May-01
	!
	! Shipping Journal Header File
	!

	RECORD OE_SHIPJOUR_CDD
		! Element = ORDNUM
		!   Description = Order Number
		STRING ORDNUM = 10
		! Element = DATE
		!   Description = Shipping Date (YYYYMMDD)
		STRING SHIPDATE = 8
		! Element = CARRIER
		!   Description = Carrier Code (Ship Via)
		STRING SHIPVIA = 2
		! Element = OPERATOR
		!   Description = Operator
		STRING OPERATOR = 10
		! Element =
		!   Description = Notes
		STRING NOTES(3) = 40
		! Element = SHIPNO
		!   Description = Packing List Release Number
		STRING SHIPNO = 2
	END RECORD
