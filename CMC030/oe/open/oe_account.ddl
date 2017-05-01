DEFINE RECORD CDD$TOP.OE.OE_ACCOUNT

        DESCRIPTION IS /*Sales Order Account Table*/.

        OE_ACCOUNT_CDD STRUCTURE.

        /* Element = CUSTTYPE
        Description = Customer type */
        CUSTTYPE                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Sales Order Type */
        ORDTYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = ACCOUNT
        Description = AR GL Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Order Discount GL Account Number */
        DISACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Freight General Ledger Account Number */
        FRACCT                  DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Handling GL Account Number */
        HANDLING                DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Seles Account Number */
        SALES                   DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = COS Account Number */
        COSACCT                 DATATYPE IS TEXT SIZE IS 18.

        END OE_ACCOUNT_CDD STRUCTURE.

END OE_ACCOUNT.
