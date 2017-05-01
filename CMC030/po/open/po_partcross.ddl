DEFINE RECORD CDD$TOP.PO.PO_PARTCROSS

        DESCRIPTION IS /*Vendor Part Cross Reference*/.

        PO_PARTCROSS_CDD STRUCTURE.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = VENDOR
        Description = Vendor Number */
        VENDOR                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = PRODUCT
        Description = Product Number */
        VENPROD                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = UOM
        Description = Unit of measurement */
        VENUOM                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Vendor Conversion Factor */
        VENFAC                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Our conversion vactor */
        FACTOR                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Product Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Lead Time */
        LEAD                    DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Minimum order quantity */
        MINQTY                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Priority Code */
        PRIORITY                DATATYPE IS TEXT SIZE IS 1.

        END PO_PARTCROSS_CDD STRUCTURE.

END PO_PARTCROSS.
