DEFINE RECORD CDD$TOP.PO.PO_CALCORDER

        DESCRIPTION IS /*Calculate PO Order Journal*/.

        PO_CALCORDER_CDD STRUCTURE.

        /* Element = VENDOR
        Description = Vendor Number */
        VENDOR                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = 10 Quanities used in calculating order */
        QUANITY                 ARRAY 0:10 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Quanity to order */
        ORDER                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Cost from vendor */
        COST                    DATATYPE IS G_FLOATING.

        END PO_CALCORDER_CDD STRUCTURE.

END PO_CALCORDER.
