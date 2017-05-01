	!
	! File Layout for: WP.WP_CONTROL on 21-May-01
	!
	! WIP Controlling File
	!

	RECORD WP_CONTROL_CDD
		! Element = ORDNUM
		!   Description = Order Number
		STRING ORDNUM = 10
		! Element = DATE
		!   Description = Last Purge Date (YYYYMMDD)
		STRING PURGDATE = 8
		! Element =
		!   Description = Activity Flag
		STRING STATUS_FLAG = 1
		! Element = REQNUM
		!   Description = Requisition Number
		STRING REQNUM = 10
		! Element = ACCOUNT
		!   Description = Inventory Material Price Variance
		STRING INVMATPVAR = 18
		! Element = ACCOUNT
		!   Description = Inventory Material Usage Variance
		STRING INVMATUVAR = 18
		! Element = ACCOUNT
		!   Description = Inventory Labor Rate Variance
		STRING INVLABRVAR = 18
		! Element = ACCOUNT
		!   Description = Inventory Labor Efficiency Variance
		STRING INVLABEVAR = 18
		! Element = ACCOUNT
		!   Description = Inventory Buÿrdn Variance
		STRING INVBURVAR = 18
		! Element = ACCOUNT
		!   Description = Equipment Material Price Variance
		STRING EQMATPVAR = 18
		! Element = ACCOUNT
		!   Description = Equipment Material Usage Variance
		STRING EQMATUVAR = 18
		! Element = ACCOUNT
		!   Description = Equipment Labor Rate Variance
		STRING EQLABRVAR = 18
		! Element = ACCOUNT
		!   Description = Equipment Labor Efficiency Variance
		STRING EQLABEVAR = 18
		! Element = ACCOUNT
		!   Description = Equipment Burden Variance
		STRING EQBURVAR = 18
	END RECORD
