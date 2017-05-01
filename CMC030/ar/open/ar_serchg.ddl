DEFINE RECORD CDD$TOP.AR.AR_SERCHG

        DESCRIPTION IS /*Service Charge Definition*/.

        AR_SERCHG_CDD STRUCTURE.

        /* Element =
        Description = Country */
        COUNTRY                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = State */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        SCREV                   DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Service charge percentage */
        SERCHG                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Minimum chargeable */
        MINIMUM                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Dollar amount charge */
        DOLLAR                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Start Period for Service Charge */
        SPERIOD                 DATATYPE IS SIGNED WORD.

        END AR_SERCHG_CDD STRUCTURE.

END AR_SERCHG.
