DEFINE RECORD CDD$TOP.PP.PP_CONTROL

        DESCRIPTION IS /*Pacific Pride Control File*/.

        PP_CONTROL_CDD STRUCTURE.

        /* Element = CUSTOMER
        Description = System Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = ACCOUNT
        Description = A/R General Ledger Account Number */
        AR_ACCOUNT              DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Host Number */
        HOST_NUM                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Dsicount Days */
        DIS_DAYS                DATATYPE IS TEXT SIZE IS 2.

        /* Element = DATE
        Description = Date (YYYYMMDD) */
        INVDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Last Invoice Number */
        LAST_INV                DATATYPE IS TEXT SIZE IS 8.

        END PP_CONTROL_CDD STRUCTURE.

END PP_CONTROL.
