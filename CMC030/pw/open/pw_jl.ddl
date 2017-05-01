DEFINE RECORD CDD$TOP.PW.PW_JL

        DESCRIPTION IS /*PW Journal Line*/.

        PW_JL_CDD STRUCTURE.

        /* Element = ORDNUM
        Description = Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Window (1,2) */
        WINDOW                  DATATYPE IS TEXT SIZE IS 1.

        /* Element = LINE
        Description = Line */
        JLINE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = PRODUCT
        Description = Product Number */
        PRONUM                  DATATYPE IS TEXT SIZE IS 14.

        /* Element = LOCATION
        Description = Store number */
        STONUM                  DATATYPE IS TEXT SIZE IS 4.

        /* Element = LOT
        Description = Lot Number */
        LOTNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = DESCRIPTION
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCNUM                  DATATYPE IS TEXT SIZE IS 18.

        /* Element = QUANTITY
        Description = Quantity */
        QTY                     DATATYPE IS G_FLOATING.

        /* Element =
        Description = Price */
        PRICE                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Weight */
        POUNDS                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Extension */
        EXT                     DATATYPE IS G_FLOATING.

        /* Element = PCTYPE
        Description = Price Flag */
        PRTYPE                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = VENDOR
        Description = Vendor Number */
        VENNUM                  DATATYPE IS TEXT SIZE IS 10.

        END PW_JL_CDD STRUCTURE.

END PW_JL.
