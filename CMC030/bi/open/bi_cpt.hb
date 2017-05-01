	!
	! File Layout for: BI.BI_CPT on 21-May-01
	!
	! Current Procedural Terminology Codes
	!

	RECORD BI_CPT_CDD
		! Element = CPT
		!   Description = Current Procedural Terminology Code
		STRING CPT = 5
		! Element = DESCRIPTION
		!   Description = Description
		STRING DESCRIPTION = 40
		! Element = CPTTYPE
		!   Description = CPT Type
		STRING CPTTYPE = 2
		! Element = CATEGORY
		!   Description = CPT Category
		STRING CATEGORY = 4
		! Element =
		!   Description = Rate Flag (F,R,T)
		STRING RATEFLAG = 1
	END RECORD
