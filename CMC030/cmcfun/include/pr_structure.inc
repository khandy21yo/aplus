	!
	! Pr structures
	!
	RECORD PR_TAXES_STRUCT

		STRING	TTYPE	= 2%	! FW,FI,SW,SX,...
		STRING	CODE	= 2%	! ID,NY,NJ,PA,...

		GFLOAT	TAXABLE(4%)	! Tax basis amount
		GFLOAT	REPORTABLE(4%)	! Reportable
		GFLOAT	TAX(4%)		! Amount of tax
		GFLOAT	WKWRK(4%)	! Weeks worked

	END RECORD

