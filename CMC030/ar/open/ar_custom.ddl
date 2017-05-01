DEFINE RECORD CDD$TOP.AR.AR_CUSTOM

        DESCRIPTION IS /*Accounts Receivable Customer File*/.

        AR_CUSTOM_CDD STRUCTURE.

        /* Element = CUSNUM
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Customer name */
        CUSNAM                  DATATYPE IS TEXT SIZE IS 50.

        /* Element =
        Description = Customer address 1 */
        ADD1                    DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Customer address 2 */
        ADD2                    DATATYPE IS TEXT SIZE IS 21.

        /* Element =
        Description = Customer city */
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

        /* Element =
        Description = Unused */
        FILLER1                 DATATYPE IS TEXT SIZE IS 4.

        /* Element = PHONE
        Description = Phone number */
        PHONE                   DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = O-open item, B-balance forward */
        METHOD                  DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Statement (y/n) */
        STMTFLG                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Alpha sort field */
        ALPSRT                  DATATYPE IS TEXT SIZE IS 15.

        /* Element =
        Description = Service charge (y/n) */
        SERCHRG                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Purge flag (Y or N) */
        PURGE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = UNUSED SPACE */
        FILLER                  DATATYPE IS TEXT SIZE IS 9.

        END AR_CUSTOM_CDD STRUCTURE.

END AR_CUSTOM.
