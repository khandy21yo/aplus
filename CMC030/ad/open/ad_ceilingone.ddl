DEFINE RECORD CDD$TOP.AD.AD_CEILINGONE

        DESCRIPTION IS /*One Dimensional Optional Ceiling Table*/.

        AD_CEILINGONE_CDD STRUCTURE.

        /* Element = OPTTABLE
        Description = Depreciation optional table code */
        OPTTABLE                DATATYPE IS TEXT SIZE IS 6.

        /* Element = DATE
        Description = Effective Date (YYYYMMDD) */
        EFFDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Deprecition year */
        DEP_YEAR                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Ceiling amount */
        CEILING                 DATATYPE IS G_FLOATING.

        END AD_CEILINGONE_CDD STRUCTURE.

END AD_CEILINGONE.
