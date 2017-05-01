DEFINE RECORD CDD$TOP.AD.AD_TABLEONE

        DESCRIPTION IS /*One Dimensional Optional Depreciation Table*/.

        AD_TABLEONE_CDD STRUCTURE.

        /* Element = OPTTABLE
        Description = Depreciation optional table code */
        OPTTABLE                DATATYPE IS TEXT SIZE IS 6.

        /* Element = DATE
        Description = Effective Date (YYYYMMDD) */
        EFFDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Number of years */
        YEARS                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Deprecition year */
        DEP_YEAR                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Percentage */
        PERCENTAGE              DATATYPE IS SIGNED WORD.

        END AD_TABLEONE_CDD STRUCTURE.

END AD_TABLEONE.
