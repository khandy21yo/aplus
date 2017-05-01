	!
	! File Layout for: AD.AD_DEPCLASS on 21-May-01
	!
	! Depreciation Class Definition Table
	!

	RECORD AD_DEPCLASS_CDD
		! Element = DEPCLASS
		!   Description = Depreciation class code
		STRING DEPCLASS = 4
		! Element =
		!   Description = Description
		STRING DESCRIPTION = 40
		! Element = PROPTYPE
		!   Description = Property type code
		STRING PROPTYPE = 2
		! Element = DEPMETHOD
		!   Description = Depreciation method
		STRING DEPMETHOD = 4
		! Element =
		!   Description = Recovery period
		STRING YEARS = 4
		! Element = DEPCONV
		!   Description = First year convention code
		STRING FYCONV = 2
		! Element = DEPCONV
		!   Description = Disposition convention code
		STRING DYCONV = 2
		! Element = OPTTABLE
		!   Description = Depreciation optional table code
		STRING OPTTABLE = 6
		! Element = CEILTABLE
		!   Description = Ceiling table code
		STRING CEILTABLE = 6
		! Element =
		!   Description = Adjust basis by salvage (Y,N)
		STRING SALVFACTOR = 1
		! Element =
		!   Description = Adjust basis by bonus (Y,N)
		STRING BONUSFACTOR = 1
		! Element =
		!   Description = Adjust basis by ITC (Y,N)
		STRING ITCFACTOR = 1
	END RECORD
