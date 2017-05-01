	!
	! File Layout for: PR.PR_OPER on 21-May-01
	!
	! Operations Table
	!

	RECORD PR_OPER_CDD
		! Element = OPERATION
		!   Description = Operation
		STRING OPER = 8
		! Element = DATE
		!   Description = Date
		STRING EFFDATE = 8
		! Element =
		!   Description = Piece rate
		GFLOAT PIECE_RATE
		! Element =
		!   Description = Hourly rate
		GFLOAT HOUR_RATE
	END RECORD
