DEFINE RECORD CDD$TOP.AP.AP_PJH

        DESCRIPTION IS /*Purchase Journal Header*/.

        AP_PJH_CDD STRUCTURE.

        /* Element =
        Description = */
        TRANKEY                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        VENNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = */
        TRANKEY_DATE            DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        INVNUM                  DATATYPE IS TEXT SIZE IS 15.

        /* Element =
        Description = */
        INVDAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        INVAMT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        CODE_1099               DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = */
        AMT_1099                DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        USE_JOB_NUM             DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = */
        USE_AMT                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        DISCDAT                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        DISCAMT                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        DUEDAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        PONUM                   DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = */
        AP_ACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = */
        CASH_ACCT               DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = */
        CKNUM                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        CKDAT                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        DESCR                   DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = */
        CKAMT                   DATATYPE IS G_FLOATING.

        END AP_PJH_CDD STRUCTURE.

END AP_PJH.
