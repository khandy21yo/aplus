DEFINE RECORD CDD$TOP.AD.AD_TABLE

        DESCRIPTION IS /*Optional Depreciation Tables*/.

        AD_TABLE_CDD STRUCTURE.

        /* Element = OPTTABLE
        Description = Depreciation optional table code */
        OPTTABLE                DATATYPE IS TEXT SIZE IS 6.

        /* Element = DATE
        Description = Effective Date (YYYYMMDD) */
        EFFDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Depreciation life */
        YEARS                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Dimension (1 or 2) */
        DIMEN                   DATATYPE IS TEXT SIZE IS 1.

        END AD_TABLE_CDD STRUCTURE.

END AD_TABLE.
