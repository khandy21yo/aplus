DEFINE RECORD CDD$TOP.PR.PR_OPER

        DESCRIPTION IS /*Operations Table*/.

        PR_OPER_CDD STRUCTURE.

        /* Element = OPERATION
        Description = Operation */
        OPER                    DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = Date */
        EFFDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Piece rate */
        PIECE_RATE              DATATYPE IS G_FLOATING.

        /* Element =
        Description = Hourly rate */
        HOUR_RATE               DATATYPE IS G_FLOATING.

        END PR_OPER_CDD STRUCTURE.

END PR_OPER.
