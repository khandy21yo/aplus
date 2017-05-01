DEFINE RECORD CDD$TOP.PO.PO_RECLINE

        DESCRIPTION IS /*Purchase Order Receiver Line Journal*/.

        PO_RECLINE_CDD STRUCTURE.

        /* Element = PO
        Description = Purchase order number */
        PO                      DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Purchase Order Line */
        PO_LINE                 DATATYPE IS TEXT SIZE IS 4.

        /* Element = DESCRIPTION
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = UOM
        Description = Unit of measurement */
        UOM                     DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Receive Quantity */
        RECQTY                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Canceled Quantity */
        CANQTY                  DATATYPE IS G_FLOATING.

        /* Element = FLAG
        Description = Line Flag = Y if Line in Register */
        LINEFLAG                DATATYPE IS TEXT SIZE IS 1.

        END PO_RECLINE_CDD STRUCTURE.

END PO_RECLINE.
