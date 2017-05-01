DEFINE RECORD CDD$TOP.AR.AR_CUSBAL

        DESCRIPTION IS /*Customer Balances*/.

        AR_CUSBAL_CDD STRUCTURE.

        /* Element = CUSNUM
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Credit Limit */
        CREDIT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Aging periods */
        AGING                   ARRAY 0:4 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Future amount */
        FUTURE                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = YTD Service charge */
        YTDSERVICE              DATATYPE IS G_FLOATING.

        /* Element =
        Description = PTD Sales */
        LAST_PAID               DATATYPE IS G_FLOATING.

        /* Element =
        Description = YTD Sales */
        YTDSALES                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Service charge */
        CHARGE                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Last Service Charge date */
        LAST_CHARGE             DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Last Payment date */
        LAST_PAYMENT            DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Last update */
        LAST_UPDATE             DATATYPE IS TEXT SIZE IS 8.

        END AR_CUSBAL_CDD STRUCTURE.

END AR_CUSBAL.
