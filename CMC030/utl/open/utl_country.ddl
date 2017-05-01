DEFINE RECORD CDD$TOP.UTL.UTL_COUNTRY

        DESCRIPTION IS /*Country Definitions*/.

        UTL_COUNTRY_CDD STRUCTURE.

        /* Element = COUNTRY
        Description = Country */
        COUNTRY                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        END UTL_COUNTRY_CDD STRUCTURE.

END UTL_COUNTRY.
