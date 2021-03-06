1	!
	! Special program to convert PP_site_product from old to new format
	!
	! History:
	!	10/26/96 - Kevin Handy
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[PP.OPEN]PP_site_product.HB"
	MAP (PP_site_product) PP_site_product_CDD PP_site_product

	!
	! File Layout for: PP.PP_SITE_PRODUCT
	!
	! Products Available at this Site
	!

	RECORD PP_SITE_PRODUCT_OLD_CDD
		! Element =
		! Description = Host Number
		STRING HOST = 3
		! Element =
		! Description = site code
		STRING SITE = 2
		! Element =
		! Description = Site Type
		STRING STYPE = 1
		! Element = PRODUCT
		! Description = Product Number
		STRING PRODUCT = 14
		! Element =
		! Description = Federal INTP
		STRING FED_INTP = 1
		! Element =
		! Description = Federal Rate
		GFLOAT FED_RATE
		! Element = ACCOUNT
		! Description = Federal General Ledger Account Number
		STRING FED_ACCOUNT = 18
		! Element =
		! Description = State INTP
		STRING STA_INTP = 1
		! Element =
		! Description = State Rate
		GFLOAT STA_RATE
		! Element = ACCOUNT
		! Description = State General Ledger Account Number
		STRING STA_ACCOUNT = 18
		! Element =
		! Description = County INTP
		STRING COU_INTP = 1
		! Element =
		! Description = County Rate
		GFLOAT COU_RATE
		! Element = ACCOUNT
		! Description = County General Ledger Account Number
		STRING COU_ACCOUNT = 18
		! Element =
		! Description = City INTP
		STRING CTY_INTP = 1
		! Element =
		! Description = City Rate
		GFLOAT CTY_RATE
		! Element = ACCOUNT
		! Description = City General Ledger Account Number
		STRING CTY_ACCOUNT = 18
		! Element =
		! Description = Sales Tax INTP
		STRING STX_INTP = 1
		! Element =
		! Description = Sales Tax Rate
		GFLOAT STX_RATE
		! Element = ACCOUNT
		! Description = Sales Tax General Ledger Account Number
		STRING STX_ACCOUNT = 18
		! Element = DATE
		! Description = Effective Date (YYYYMMDD)
		STRING EFFDATE = 8
	END RECORD

	MAP (PP_site_product_OLD) PP_site_product_OLD_CDD PP_site_product_OLD

100 !	NAME "PP_site_product.MAS" AS "PP_site_product.MAS_OLD"

120	%INCLUDE "SOURCE:[PP.OPEN]PP_site_product.CRE"

200	!======================================================================
	! PP_SITE_PRODUCT file (open read only)
	!======================================================================

	CALL ASSG_CHANNEL(PP_SITE_PRODUCT_OLD.CH%, STAT%)

	PP_SITE_PRODUCT_OLD.NAME$ = &
		PP_SITE_PRODUCT.DEV$ + "PP_SITE_PRODUCT.MAS_OLD"

	OPEN PP_SITE_PRODUCT_OLD.NAME$ FOR INPUT AS FILE &
		PP_SITE_PRODUCT_OLD.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PP_SITE_PRODUCT_OLD, &
		PRIMARY KEY &
		( &
			PP_SITE_PRODUCT_OLD::HOST, &
			PP_SITE_PRODUCT_OLD::SITE, &
			PP_SITE_PRODUCT_OLD::STYPE, &
			PP_SITE_PRODUCT_OLD::PRODUCT &
		), &
		ACCESS READ, ALLOW MODIFY


300	RESET #PP_site_product_OLD.CH%

320	GET #PP_site_product_OLD.CH%

330	pp_site_product::HOST		= pp_site_product_OLD::HOST
	pp_site_product::SITE		= pp_site_product_OLD::SITE
	pp_site_product::STYPE		= pp_site_product_OLD::STYPE
	pp_site_product::PRODUCT	= pp_site_product_OLD::PRODUCT
	pp_site_product::FED_INTP	= pp_site_product_OLD::FED_INTP
	pp_site_product::FED_RATE	= pp_site_product_OLD::FED_RATE
	pp_site_product::FED_ACCOUNT	= pp_site_product_OLD::FED_ACCOUNT
	pp_site_product::STA_INTP	= pp_site_product_OLD::STA_INTP
	pp_site_product::STA_RATE	= pp_site_product_OLD::STA_RATE
	pp_site_product::STA_ACCOUNT	= pp_site_product_OLD::STA_ACCOUNT
	pp_site_product::COU_INTP	= pp_site_product_OLD::COU_INTP
	pp_site_product::COU_RATE	= pp_site_product_OLD::COU_RATE
	pp_site_product::COU_ACCOUNT	= pp_site_product_OLD::COU_ACCOUNT
	pp_site_product::CTY_INTP	= pp_site_product_OLD::CTY_INTP
	pp_site_product::CTY_RATE	= pp_site_product_OLD::CTY_RATE
	pp_site_product::CTY_ACCOUNT	= pp_site_product_OLD::CTY_ACCOUNT
	pp_site_product::STX_INTP	= pp_site_product_OLD::STX_INTP
	pp_site_product::STX_RATE	= pp_site_product_OLD::STX_RATE
	pp_site_product::STX_ACCOUNT	= pp_site_product_OLD::STX_ACCOUNT
	pp_site_product::EFFDATE	= pp_site_product_OLD::EFFDATE

	PUT #PP_site_product.CH%

	GOTO 320

32767	END
