DEFINE RECORD CDD$TOP.TK.TK_FOREIGN

        DESCRIPTION IS /*Foreign Key Definition*/.

        TK_FOREIGN_CDD STRUCTURE.

        /* Element =
        Description = Sub-record structure */
        SUBSTRUCT               DATATYPE IS TEXT SIZE IS 50.

        /* Element =
        Description = Field names used in the association */
        FLDNAMES                DATATYPE IS TEXT SIZE IS 60.

        /* Element =
        Description = Record structure */
        STRUCT                  DATATYPE IS TEXT SIZE IS 50.

        /* Element =
        Description = Sub-record structure association */
        SUBASSOCIATE            DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Record Structure association */
        ASSOCIATE               DATATYPE IS TEXT SIZE IS 1.

        END TK_FOREIGN_CDD STRUCTURE.

END TK_FOREIGN.
