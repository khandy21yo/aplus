DEFINE RECORD CDD$TOP.TV.TV_COMMER_CHARGE

        DESCRIPTION IS /*Commercial Charge File*/.

        TV_COMMER_CHARGE_CDD STRUCTURE.

        /* Element =
        Description = Form Number */
        FRMNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Billable date */
        BILL_DATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Dollar Amount */
        AMOUNT                  DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCTNO                  DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = GL period */
        PERIOD                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = CO-OP Sponser Number */
        COOP                    DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Billed flag (Y/N) */
        BILL_FLAG               DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Bill flag (% - percent, $ - dollars) */
        BILL_TYPE               DATATYPE IS TEXT SIZE IS 1.

        END TV_COMMER_CHARGE_CDD STRUCTURE.

END TV_COMMER_CHARGE.
