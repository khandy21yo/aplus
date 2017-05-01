	!
	! File Layout for: PR.PR_36TAX_TABLE on 21-May-01
	!
	! Payroll Tax Table
	!

	RECORD PR_36TAX_TABLE_CDD
		! Element =
		!   Description = Federal,State,County,Munic.,District
		STRING AUTH = 1
		! Element =
		!   Description = Used only if AUTH <> "Federal"
		STRING CODE = 2
		! Element =
		!   Description =
		STRING TSTATUS = 1
		! Element =
		!   Description = Employer FICA percentage OASDI
		GFLOAT FICA_EMPR_PCT
		! Element =
		!   Description = Employee FICA percent OASDI
		GFLOAT FICA_EMPE_PCT
		! Element =
		!   Description = FICA limit OASDI
		GFLOAT FICA_LIMIT
		! Element =
		!   Description = Federal or <blank>
		STRING CALC_BASIS = 1
		! Element =
		!   Description =
		GFLOAT BASIS_PCT
		! Element =
		!   Description =
		GFLOAT OT_ANL_MIN
		! Element =
		!   Description =
		GFLOAT OT_ANL_PCT
		! Element =
		!   Description =
		GFLOAT OT_ANL_MAX
		! Element =
		!   Description =
		GFLOAT STD_WH
		! Element =
		!   Description =
		GFLOAT ADJ_GRS_PCT
		! Element =
		!   Description =
		GFLOAT MIN_STD_ADJ
		! Element =
		!   Description =
		GFLOAT MAX_STD_ADJ
		! Element =
		!   Description =
		GFLOAT PR_EX
		! Element =
		!   Description =
		GFLOAT OVER(11)
		! Element =
		!   Description =
		GFLOAT TAXAMT(11)
		! Element =
		!   Description =
		GFLOAT PLUS(11)
		! Element =
		!   Description =
		GFLOAT SUI_MIN
		! Element =
		!   Description =
		GFLOAT SUI_PCT
		! Element =
		!   Description =
		GFLOAT SUI_MAX
		! Element =
		!   Description =
		GFLOAT OT_DED_MAX
		! Element =
		!   Description = Employer FICA HI percentage
		GFLOAT FICA_EMPR_PCT_HI
		! Element =
		!   Description = Employee FICA HI percentage
		GFLOAT FICA_EMPE_PCT_HI
		! Element =
		!   Description = FICA HI limit
		GFLOAT FICA_LIMIT_HI
		! Element =
		!   Description = Personal Exemption for Additional Exempt
		GFLOAT PR_EX_ADD
		! Element =
		!   Description = Federal Exemption Threshold
		GFLOAT THRESHOLD
		! Element =
		!   Description = Federal Exemption Threshold Rate
		GFLOAT THRESHOLD_RATE
		! Element =
		!   Description = Low income Exemption
		GFLOAT LOW_INCOME
		! Element =
		!   Description = Credit for Federal Witholding
		GFLOAT FED_CREDIT_PCT
	END RECORD
