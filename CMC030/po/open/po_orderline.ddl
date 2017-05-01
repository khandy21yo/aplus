DEFINE RECORD CDD$TOP.PO.PO_ORDERLINE

        DESCRIPTION IS /*PO journal line file*/.

        PO_ORDERLINE_CDD STRUCTURE.

        /* Element = PO
        Description = Purchase order number */
        PO                      DATATYPE IS TEXT SIZE IS 10.

        /* Element = LINE
        Description = Line */
        PO_LINE                 DATATYPE IS TEXT SIZE IS 4.

        /* Element = PRODUCT
        Description = Our Product Number */
        OUR_PRODUCT             DATATYPE IS TEXT SIZE IS 14.

        /* Element = UOM
        Description = Our Unit of measurement */
        OUR_UOM                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = PRODUCT
        Description = Vendors Product Number */
        VEN_PRODUCT             DATATYPE IS TEXT SIZE IS 14.

        /* Element = DESCRIPTION
        Description = Product Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Expected price */
        VEN_PRICE               DATATYPE IS G_FLOATING.

        END PO_ORDERLINE_CDD STRUCTURE.

END PO_ORDERLINE.
