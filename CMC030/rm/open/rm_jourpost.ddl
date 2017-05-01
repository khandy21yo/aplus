DEFINE RECORD CDD$TOP.RM.RM_JOURPOST

        DESCRIPTION IS /*Restaurant Journal Posting File*/.

        RM_JOURPOST_CDD STRUCTURE.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = Date */
        STARTDATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element = TRANSTYPE
        Description = Transaction type code from entry */
        TRANSTYPE               DATATYPE IS TEXT SIZE IS 2.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = DATE
        Description = Actual date (YYYYMMDD) */
        ACTDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element = TRANSTYPE
        Description = Transaction type code */
        TTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Quantity */
        QUANTITY                DATATYPE IS G_FLOATING.

        /* Element =
        Description = Price */
        PRICE                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Sequential number */
        SEQNUM                  DATATYPE IS TEXT SIZE IS 4.

        END RM_JOURPOST_CDD STRUCTURE.

END RM_JOURPOST.
