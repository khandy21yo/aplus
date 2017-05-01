DEFINE RECORD CDD$TOP.AD.AD_CEILING

        DESCRIPTION IS /*Cost Recovery Ceiling Table*/.

        AD_CEILING_CDD STRUCTURE.

        /* Element = CEILTABLE
        Description = Ceiling table code */
        CEILTABLE               DATATYPE IS TEXT SIZE IS 6.

        /* Element = DATE
        Description = Effective Date (YYYYMMDD) */
        EFFDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Dimension (1 or 2) */
        DIMEN                   DATATYPE IS TEXT SIZE IS 1.

        END AD_CEILING_CDD STRUCTURE.

END AD_CEILING.
