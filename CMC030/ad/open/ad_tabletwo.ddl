DEFINE RECORD CDD$TOP.AD.AD_TABLETWO

        DESCRIPTION IS /*Two Dimensional Optional Depreciation Table*/.

        AD_TABLETWO_CDD STRUCTURE.

        /* Element = OPTTABLE
        Description = Depreciation optional table code */
        OPTTABLE                DATATYPE IS TEXT SIZE IS 6.

        /* Element = DATE
        Description = Effective Date (YYYYMMDD) */
        EFFDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Number of year */
        YEARS                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Depreciation years */
        DEP_YEAR                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Percentage */
        PERCENTAGE              ARRAY 0:12 DATATYPE IS SIGNED WORD.

        END AD_TABLETWO_CDD STRUCTURE.

END AD_TABLETWO.
