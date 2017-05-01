DEFINE RECORD CDD$TOP.BC.BC_BILLH

        DESCRIPTION IS /*Billing to Customer Billing Journal Header*/.

        BC_BILLH_CDD STRUCTURE.

        /* Element =
        Description = Order number */
        ORDER                   DATATYPE IS TEXT SIZE IS 8.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = CUSTOMER
        Description = Customer Number */
        SHPNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Salesman */
        SALEMAN                 DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = Date (YYYYMMDD) */
        ORDERDATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Ship Via */
        SHPVIA                  DATATYPE IS TEXT SIZE IS 20.

        /* Element = INVOICE
        Description = Invoice number */
        INVNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Terms */
        TERMS                   DATATYPE IS TEXT SIZE IS 16.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = REFNUM
        Description = Reference Number */
        REFNUM                  DATATYPE IS TEXT SIZE IS 20.

        END BC_BILLH_CDD STRUCTURE.

END BC_BILLH.
