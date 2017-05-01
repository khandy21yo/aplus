DEFINE RECORD CDD$TOP.UTL.UTL_STATE

        DESCRIPTION IS /*State/Providence/...*/.

        UTL_STATE_CDD STRUCTURE.

        /* Element = COUNTRY
        Description = Country */
        COUNTRY                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = STATE
        Description = State */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = FIPS Code */
        FIPS                    DATATYPE IS TEXT SIZE IS 2.

        END UTL_STATE_CDD STRUCTURE.

END UTL_STATE.
