DEFINE RECORD CDD$TOP.RM.RM_JOURPROD

        DESCRIPTION IS /*Restaurant Journal Product File*/.

        RM_JOURPROD_CDD STRUCTURE.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = Start date */
        STARTDATE               DATATYPE IS TEXT SIZE IS 8.

        /* Element = TRANSTYPE
        Description = Transaction type code */
        TRANSTYPE               DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Sequential number */
        SEQNUM                  DATATYPE IS TEXT SIZE IS 4.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Product price */
        PRICE                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Daily quantity */
        QUANTITY                ARRAY 0:6 DATATYPE IS G_FLOATING.

        END RM_JOURPROD_CDD STRUCTURE.

END RM_JOURPROD.
