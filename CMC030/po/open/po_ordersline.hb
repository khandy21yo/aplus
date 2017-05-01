	!
	! File Layout for: PO.PO_ORDERSLINE on 21-May-01
	!
	! PO Subline Journal File
	!

	RECORD PO_ORDERSLINE_CDD
		! Element = PO
		!   Description = Purchase order number
		STRING PO = 10
		! Element = LINE
		!   Description = Line
		STRING PO_LINE = 4
		! Element =
		!   Description = Our Quanity
		GFLOAT OUR_QTY
		! Element = DATE
		!   Description = Expected Date (YYYYMMDD)
		STRING RECEIVEDATE = 8
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING GL_ACCOUNT = 18
		! Element = SUBACCT
		!   Description = Sub account (job number)
		STRING SUBACCT = 10
		! Element =
		!   Description = Notes
		STRING NOTES(1) = 40
	END RECORD
