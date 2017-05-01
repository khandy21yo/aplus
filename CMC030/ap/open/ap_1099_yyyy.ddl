DEFINE RECORD CDD$TOP.AP.AP_1099_YYYY

        DESCRIPTION IS /*1099 Annual Work File*/.

        AP_1099_YYYY_CDD STRUCTURE.

        /* Element =
        Description = Vendor Number */
        VENNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Transaction key */
        TRANKEY                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Invoice number */
        INVNUM                  DATATYPE IS TEXT SIZE IS 15.

        /* Element =
        Description = Invoice Date */
        INVDAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Check Number */
        CKNUM                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Check date */
        CKDAT                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = 1099 Amount */
        AMT1099                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Check Amount */
        CKAMT                   DATATYPE IS G_FLOATING.

        END AP_1099_YYYY_CDD STRUCTURE.

END AP_1099_YYYY.
