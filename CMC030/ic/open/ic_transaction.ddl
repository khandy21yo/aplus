DEFINE RECORD CDD$TOP.IC.IC_TRANSACTION

        DESCRIPTION IS /*Inventory Transaction*/.

        IC_TRANSACTION_CDD STRUCTURE.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = Date (MMDDYYYY) */
        TRANS_DATE              DATATYPE IS TEXT SIZE IS 8.

        /* Element = REFNO
        Description = Reference number */
        PRIMARY_REF             DATATYPE IS TEXT SIZE IS 16.

        /* Element = XREF
        Description = Cross reference */
        CROSS_REF               DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Subaccount */
        SUBACCOUNT              DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Lot number */
        LOT                     DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Salesman,Operator etc */
        STATIONMAN              DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Transaction type */
        TYPE_A                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = UNIT
        Description = Unit amount */
        QUANTITY_A              DATATYPE IS G_FLOATING.

        /* Element =
        Description = Transaction type */
        TYPE_B                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = UNIT
        Description = Unit amount */
        QUANTITY_B              DATATYPE IS G_FLOATING.

        /* Element =
        Description = Inventory cost */
        COST                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Price Amount */
        PRICE                   DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = Transaction General Ledger Account Numbe */
        TRANSACCT               DATATYPE IS TEXT SIZE IS 18.

        /* Element = DATE
        Description = Date (MMDDYYYY) */
        POSTDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Time (HHMMSS) */
        POSTTIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element = BATCH
        Description = Batch number used for posting */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        END IC_TRANSACTION_CDD STRUCTURE.

END IC_TRANSACTION.
