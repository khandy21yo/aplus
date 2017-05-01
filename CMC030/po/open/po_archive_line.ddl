DEFINE RECORD CDD$TOP.PO.PO_ARCHIVE_LINE

        DESCRIPTION IS /*Purchase Order Archive Line*/.

        PO_ARCHIVE_LINE_CDD STRUCTURE.

        /* Element = PO
        Description = Purchase order number */
        PO                      DATATYPE IS TEXT SIZE IS 10.

        /* Element = LINE
        Description = Line */
        PO_LINE                 DATATYPE IS TEXT SIZE IS 4.

        /* Element = VENDOR
        Description = Vendor Number */
        VENDOR                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = LOCATION
        Description = Location number */
        FROMLOCATION            DATATYPE IS TEXT SIZE IS 4.

        /* Element = PRODUCT
        Description = Product Number */
        OUR_PRODUCT             DATATYPE IS TEXT SIZE IS 14.

        /* Element = UOM
        Description = Unit of measurement */
        OUR_UOM                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Our Conversion Factor */
        OUR_FACTOR              DATATYPE IS G_FLOATING.

        /* Element = PRODUCT
        Description = Product Number */
        VEN_PRODUCT             DATATYPE IS TEXT SIZE IS 14.

        /* Element = DESCRIPTION
        Description = Description */
        VEN_DESCRIPTION         DATATYPE IS TEXT SIZE IS 40.

        /* Element = UOM
        Description = Unit of measurement */
        VEN_UOM                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Vendors conversion factor */
        VEN_FACTOR              DATATYPE IS G_FLOATING.

        /* Element =
        Description = Discount percentage */
        VEN_DISCOUNT            DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        GL_ACCOUNT              DATATYPE IS TEXT SIZE IS 18.

        /* Element = SUBACCT
        Description = Sub account (job number) */
        SUBACCT                 DATATYPE IS TEXT SIZE IS 10.

        /* Element = PO_TYPE
        Description = Purchase Order Type */
        PO_TYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Open/Closed Flag */
        OPEN_CLOSE              DATATYPE IS TEXT SIZE IS 1.

        END PO_ARCHIVE_LINE_CDD STRUCTURE.

END PO_ARCHIVE_LINE.
