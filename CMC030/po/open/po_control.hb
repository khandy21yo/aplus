	!
	! File Layout for: PO.PO_CONTROL on 21-May-01
	!
	! Purchase Order Controling File
	!

	RECORD PO_CONTROL_CDD
		! Element =
		!   Description = Last Purchase Order number
		STRING LAST_PO = 10
		! Element =
		!   Description = Formula for calculating re-order qty
		STRING LOAD_FORMULA = 10
	END RECORD
