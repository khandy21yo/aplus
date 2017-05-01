DEFINE RECORD CDD$TOP.PP.PP_SITE_PRODUCT

        DESCRIPTION IS /*Products Available at this Site*/.

        PP_SITE_PRODUCT_CDD STRUCTURE.

        /* Element =
        Description = Host Number */
        HOST                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = site code */
        SITE                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Site Type */
        STYPE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Federal INTP */
        FED_INTP                DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Federal Rate */
        FED_RATE                DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = Federal General Ledger Account Number */
        FED_ACCOUNT             DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = State INTP */
        STA_INTP                DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = State Rate */
        STA_RATE                DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = State General Ledger Account Number */
        STA_ACCOUNT             DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = County INTP */
        COU_INTP                DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = County Rate */
        COU_RATE                DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = County General Ledger Account Number */
        COU_ACCOUNT             DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = City INTP */
        CTY_INTP                DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = City Rate */
        CTY_RATE                DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = City General Ledger Account Number */
        CTY_ACCOUNT             DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = Sales Tax INTP */
        STX_INTP                DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Sales Tax Rate */
        STX_RATE                DATATYPE IS G_FLOATING.

        /* Element = ACCOUNT
        Description = Sales Tax General Ledger Account Number */
        STX_ACCOUNT             DATATYPE IS TEXT SIZE IS 18.

        /* Element = DATE
        Description = Effective Date (YYYYMMDD) */
        EFFDATE                 DATATYPE IS TEXT SIZE IS 8.

        END PP_SITE_PRODUCT_CDD STRUCTURE.

END PP_SITE_PRODUCT.
