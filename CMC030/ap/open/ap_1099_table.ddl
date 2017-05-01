DEFINE RECORD CDD$TOP.AP.AP_1099_TABLE

        DESCRIPTION IS /*1099 Definition Table*/.

        AP_1099_TABLE_CDD STRUCTURE.

        /* Element =
        Description = Table code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Table Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Base amount */
        BASEAMT                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Form Number */
        FRMNUM                  DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Form location */
        FRMLOC                  DATATYPE IS TEXT SIZE IS 2.

        END AP_1099_TABLE_CDD STRUCTURE.

END AP_1099_TABLE.
