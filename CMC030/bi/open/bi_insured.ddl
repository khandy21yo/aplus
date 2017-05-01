DEFINE RECORD CDD$TOP.BI.BI_INSURED

        DESCRIPTION IS /*Insured File*/.

        BI_INSURED_CDD STRUCTURE.

        /* Element = CUSNUM
        Description = Insured Number */
        INSURED                 DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Insured name */
        INSNAME                 DATATYPE IS TEXT SIZE IS 50.

        /* Element =
        Description = Insured address, line 1 */
        ADD1                    DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Insured address, line 2 */
        ADD2                    DATATYPE IS TEXT SIZE IS 21.

        /* Element =
        Description = Insured city */
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
        Description = Alpha sort field */
        ALPSRT                  DATATYPE IS TEXT SIZE IS 15.

        /* Element = REFNO
        Description = Reference number */
        REFNO                   DATATYPE IS TEXT SIZE IS 16.

        /* Element = DATE
        Description = Birthdate (YYYYMMDD) */
        BIRTHDATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element = SEX
        Description = Sex */
        SEX                     DATATYPE IS TEXT SIZE IS 1.

        /* Element = DATE
        Description = Onset date (YYYYMMDD) */
        ONSETDATE               DATATYPE IS TEXT SIZE IS 8.

        END BI_INSURED_CDD STRUCTURE.

END BI_INSURED.
