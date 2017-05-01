	!
	! File Layout for: TV.TV_CART_INVENTORY on 21-May-01
	!
	! TV Cart Inventory Master File
	!

	RECORD TV_CART_INVENTORY_CDD
		! Element =
		!   Description = House Cart number
		STRING CARTNUM = 10
		! Element =
		!   Description = Production name
		STRING PRONAME = 40
		! Element =
		!   Description = Customer name
		STRING CUSNUM = 10
		! Element =
		!   Description = Agency number
		STRING AGENCY_NUM = 10
		! Element =
		!   Description = Title
		STRING TITLE = 40
		! Element =
		!   Description = Length
		STRING LENGTH = 6
		! Element =
		!   Description = Date in
		STRING DATE_IN = 8
		! Element =
		!   Description = Date out
		STRING DATE_OUT = 8
	END RECORD
