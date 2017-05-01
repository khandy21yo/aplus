DEFINE RECORD CDD$TOP.IC.IC_JOURNAL

        DESCRIPTION IS /*Inventory Journal*/.

        IC_JOURNAL_CDD STRUCTURE.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = Date (MMDDYYYY) */
        TRANS_DATE              DATATYPE IS TEXT SIZE IS 8.

        /* Element = REF
        Description = Primary Reference */
        PRIMARY_REF             DATATYPE IS TEXT SIZE IS 8.

        /* Element = REF
        Description = Secondary Reference. */
        SECONDARY_REF           DATATYPE IS TEXT SIZE IS 8.

        /* Element = XREF
        Description = Cross reference */
        CROSS_REF               DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Subaccount */
        SUBACCOUNT              DATATYPE IS TEXT SIZE IS 10.

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
        Description = Unit Cost */
        COST                    DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = Trans General ledger account number */
        EXPACCT                 DATATYPE IS TEXT SIZE IS 18.

        /* Element = LOCATION
        Description = Location number */
        TOLOCATION              DATATYPE IS TEXT SIZE IS 4.

        END IC_JOURNAL_CDD STRUCTURE.

END IC_JOURNAL.
