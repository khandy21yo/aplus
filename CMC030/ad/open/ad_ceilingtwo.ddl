DEFINE RECORD CDD$TOP.AD.AD_CEILINGTWO

        DESCRIPTION IS /*Two Dimensional Optional Ceiling Table*/.

        AD_CEILINGTWO_CDD STRUCTURE.

        /* Element = OPTTABLE
        Description = Depreciation optional table code */
        OPTTABLE                DATATYPE IS TEXT SIZE IS 6.

        /* Element = DATE
        Description = Effective Date (YYYYMMDD) */
        EFFDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Depreciation years */
        DEP_YEAR                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Ceiling amount */
        CEILING                 ARRAY 0:12 DATATYPE IS G_FLOATING.

        END AD_CEILINGTWO_CDD STRUCTURE.

END AD_CEILINGTWO.
