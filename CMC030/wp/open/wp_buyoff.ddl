DEFINE RECORD CDD$TOP.WP.WP_BUYOFF

        DESCRIPTION IS /*Manufacturing WIP Buyoff Journal Header File*/.

        WP_BUYOFF_CDD STRUCTURE.

        /* Element =
        Description = WIP Job Number */
        JOB                     DATATYPE IS TEXT SIZE IS 10.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        /* Element = LOCATION
        Description = From Location number */
        TOLOCATION              DATATYPE IS TEXT SIZE IS 4.

        /* Element = OPERATOR
        Description = Operator */
        OPERATOR                DATATYPE IS TEXT SIZE IS 10.

        END WP_BUYOFF_CDD STRUCTURE.

END WP_BUYOFF.
