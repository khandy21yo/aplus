	!
	! File Layout for: IC.IC_CONTROL on 21-May-01
	!
	! Inventory Control
	!

	RECORD IC_CONTROL_CDD
		! Element = ERA
		!   Description = Era code
		STRING ERA = 2
		! Element = PERIOD
		!   Description = Last period closed
		STRING PERIOD = 6
		! Element = CONTROLFLAG
		!   Description = Status flag in the control files
		STRING CONTROLFLAG = 1
	END RECORD
