DEFINE RECORD CDD$TOP.PO.PO_REG_LINE

        DESCRIPTION IS /*Purchase Order Register Lines*/.

        PO_REG_LINE_CDD STRUCTURE.

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
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = UOM
        Description = Unit of measurement */
        UOM                     DATATYPE IS TEXT SIZE IS 2.

        /* Element = DESCRIPTION
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element = PO_TYPE
        Description = Purchase Order Type */
        PO_TYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Open/Closed Flag */
        OPEN_CLOSE              DATATYPE IS TEXT SIZE IS 1.

        /* Element = DATE
        Description = Order Date (YYYYMMDD) */
        ORDDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element = BATCH
        Description = Batch number used for process (post,clos */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element = PERIOD
        Description = Fiscal year (YYYY) and Cycle (PP) */
        PERIOD                  DATATYPE IS TEXT SIZE IS 6.

        END PO_REG_LINE_CDD STRUCTURE.

END PO_REG_LINE.
