DEFINE RECORD CDD$TOP.TK.TK_RELATION

        DESCRIPTION IS /*Module Relation File*/.

        TK_RELATION_CDD STRUCTURE.

        /* Element = MODNAME
        Description = Module name */
        PARENT                  DATATYPE IS TEXT SIZE IS 39.

        /* Element = MODNAME
        Description = Module name */
        CHILD                   DATATYPE IS TEXT SIZE IS 39.

        /* Element =
        Description = Number submodules in the module */
        QUANTITY                DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Defining reference */
        DEFREF                  DATATYPE IS TEXT SIZE IS 1.

        /* Element = DATE
        Description = Creating date */
        CDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Creating time */
        CTIME                   DATATYPE IS TEXT SIZE IS 6.

        END TK_RELATION_CDD STRUCTURE.

END TK_RELATION.
