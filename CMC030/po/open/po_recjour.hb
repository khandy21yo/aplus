	!
	! File Layout for: PO.PO_RECJOUR on 21-May-01
	!
	! PO Receiver Journal Header
	!

	RECORD PO_RECJOUR_CDD
		! Element = PO
		!   Description = Purchase order number
		STRING PO = 10
		! Element = DATE
		!   Description = Receive Date (YYYYMMDD)
		STRING RECDATE = 8
		! Element = REFNO
		!   Description = Reference number
		STRING REFNO = 16
		! Element = OPERATOR
		!   Description = Operator
		STRING OPERATOR = 10
	END RECORD
