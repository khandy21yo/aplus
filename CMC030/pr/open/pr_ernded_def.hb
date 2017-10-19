	!
	! File Layout for: PR.PR_ERNDED_DEF on 21-May-01
	!
	! Payroll ERNDED Definition File
	!

	RECORD PR_ERNDED_DEF_CDD
		! Element =
		!   Description = Payment,Deduction,noncompensaTion,Memo
		STRING ETYPE = 1
		! Element =
		!   Description = ERNDED code
		STRING CODE = 2
		! Element =
		!   Description = Description of ERNDED
		STRING DESCR = 30
		! Element =
		!   Description = Debit/Credit account for ERNDED
		STRING DRCR_ACCT = 18
		! Element =
		!   Description = Accrual account to credir if accrual
		STRING ACCRUAL_ACCT = 18
		! Element =
		!   Description = Post to GL in summary (Y/N)
		STRING SUMMARY = 1
		! Element =
		!   Description = Subject to federal taxes (Y/N)
		STRING TAXABLE_FWH = 1
		! Element =
		!   Description = Subject to fica employee taxes
		STRING TAXABLE_FIE = 1
		! Element =
		!   Description = Subject to FICA employer taxes (Y/N)
		STRING TAXABLE_FIR = 1
		! Element =
		!   Description = Subject to federal unempl. taxes (Y/N)
		STRING TAXABLE_FUI = 1
		! Element =
		!   Description = Subject to state taxes (Y/N)
		STRING TAXABLE_SWH = 1
		! Element =
		!   Description = Subject to state unempl. taxes (Y/N)
		STRING TAXABLE_SUI = 1
		! Element =
		!   Description = Subject to other state taxes (Y/N)
		STRING TAXABLE_OST = 1
		! Element =
		!   Description = Subject to city taxes (Y/N)
		STRING TAXABLE_CWH = 1
		! Element =
		!   Description = Subject to county taxes (Y/N)
		STRING TAXABLE_DWH = 1
		! Element =
		!   Description = Subject to school taxes (Y/N)
		STRING TAXABLE_EWH = 1
		! Element =
		!   Description = Reportable to federal (Y/N)
		STRING REPORTABLE_FWH = 1
		! Element =
		!   Description = Reportable to SOC. SEC. admin (Y/N0
		STRING REPORTABLE_FIE = 1
		! Element =
		!   Description = Reportable to SOC. SEC. admin
		STRING REPORTABLE_FIR = 1
		! Element =
		!   Description = Reportable to federal unemployment
		STRING REPORTABLE_FUI = 1
		! Element =
		!   Description = Reportable to state
		STRING REPORTABLE_SWH = 1
		! Element =
		!   Description = Reportable to state employement
		STRING REPORTABLE_SUI = 1
		! Element =
		!   Description = Reportable to state
		STRING REPORTABLE_OST = 1
		! Element =
		!   Description = Reportable to city
		STRING REPORTABLE_CWH = 1
		! Element =
		!   Description = Reportable to county
		STRING REPORTABLE_DWH = 1
		! Element =
		!   Description = Reportable to school
		STRING REPORTABLE_EWH = 1
		! Element = FLAG
		!   Description = Subject to wc
		STRING SUBJ_WC = 1
		! Element =
		!   Description = Location to be displayed on W2's
		STRING W2LOCATION = 4
	END RECORD