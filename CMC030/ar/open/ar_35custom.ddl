DEFINE RECORD CDD$TOP.AR.AR_35CUSTOM

        DESCRIPTION IS /*Customer Address File*/.

        AR_35CUSTOM_CDD STRUCTURE.

        /* Element = CUSNUM
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = COMPNAME
        Description = Person or Company Name */
        CUSNAM                  DATATYPE IS TEXT SIZE IS 50.

        /* Element = CUSTYPE
        Description = Customer type */
        TTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = CATEGORY
        Description = Category */
        CATEGORY                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = Onset Date (YYYYMMDD) */
        BDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element = ASTATUS
        Description = Activity status */
        SSTATUS                 DATATYPE IS TEXT SIZE IS 1.

        /* Element = DATE
        Description = Ending Date (YYYYMMDD) */
        EDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element = ADDRESS
        Description = Address line 1 */
        ADD1                    DATATYPE IS TEXT SIZE IS 25.

        /* Element = ADDRESS
        Description = Address line 2 */
        ADD2                    DATATYPE IS TEXT SIZE IS 25.

        /* Element = ADDRESS
        Description = Address line 3 */
        ADD3                    DATATYPE IS TEXT SIZE IS 25.

        /* Element = CITY
        Description = City */
        CITY                    DATATYPE IS TEXT SIZE IS 15.

        /* Element = STATE
        Description = State */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = ZIP
        Description = Zip code */
        ZIP                     DATATYPE IS TEXT SIZE IS 10.

        /* Element = COUNTRY
        Description = Country */
        COUNTRY                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = COUNTY
        Description = County */
        COUNTY                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = PHONE
        Description = Phone number */
        PHONE                   DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = O-open item, B-balance forward */
        METHOD                  DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Statement (Y/N) */
        STMTFLG                 DATATYPE IS TEXT SIZE IS 1.

        /* Element = ALFASORT
        Description = Alpha Sort Key */
        ALPSRT                  DATATYPE IS TEXT SIZE IS 15.

        /* Element = YESNO
        Description = Service Charge (Yes or No Flag) */
        SERCHRG                 DATATYPE IS TEXT SIZE IS 1.

        /* Element = TAXCODE
        Description = Tax code */
        TAXCODE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = TAXEXEMPT
        Description = Tax Exampt Permit Number */
        TAXEXEMP                DATATYPE IS TEXT SIZE IS 15.

        /* Element = LOCATION
        Description = Primary Location */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = TERMS
        Description = Terms */
        TERMS                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = CARRIER
        Description = Carrier Code (Ship Via) */
        CARRIER                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = SALESMAN
        Description = Salesperson number */
        SALESMAN                DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Credit Limit */
        CREDITLIM               DATATYPE IS G_FLOATING.

        /* Element = DISCOUNT
        Description = Discount percentage */
        DISCOUNT                DATATYPE IS G_FLOATING.

        /* Element = YESNO
        Description = Accept Backorders Flag (Y/N) */
        BACKORDER               DATATYPE IS TEXT SIZE IS 1.

        /* Element = TAXFLAG
        Description = Tax Flag */
        TAXFLAG                 DATATYPE IS TEXT SIZE IS 1.

        END AR_35CUSTOM_CDD STRUCTURE.

END AR_35CUSTOM.
