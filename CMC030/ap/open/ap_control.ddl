DEFINE RECORD CDD$TOP.AP.AP_CONTROL

        DESCRIPTION IS /*Accounts Payable Control File*/.

        AP_CONTROL_CDD STRUCTURE.

        /* Element =
        Description = */
        AP_ACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = */
        DISCLOST_ACCT           DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = */
        CASH_ACCT               DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = */
        LAST_TRANKEY            DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        LAST_CKNUM              DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Retention cycle */
        RETAIN                  DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Last period closed */
        LASTPERCLOSE            DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Retain 1099 history only (Y/N) */
        RETAIN_1099_ONLY        DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Year */
        YEAR                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Closing flag 0-nostate,1-close,2-reset */
        CLOSEFLAG               DATATYPE IS TEXT SIZE IS 1.

        END AP_CONTROL_CDD STRUCTURE.

END AP_CONTROL.
