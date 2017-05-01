DEFINE RECORD CDD$TOP.PR.PR_TRN_PAY

        DESCRIPTION IS /*Payroll Pay Journal*/.

        PR_TRN_PAY_CDD STRUCTURE.

        /* Element = EMPNUM
        Description = Employee number */
        EMPNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = Payroll end Date */
        PR_END_DATE             DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Employee Skill */
        EMP_SKILL               DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Employee Grade */
        EMP_GRADE               DATATYPE IS TEXT SIZE IS 2.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        /* Element = SUBACC
        Description = Sub account (job number) */
        SUBACC                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = OPERATION
        Description = Operation */
        OPER                    DATATYPE IS TEXT SIZE IS 8.

        /* Element = LOCATION
        Description = Location */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Department */
        DEPT                    DATATYPE IS TEXT SIZE IS 6.

        /* Element = WORK_CENTER
        Description = Work Center */
        WORK_CENTER             DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Union code */
        UNION                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Pay type (P-time, O-other earnings) */
        PTYPE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Rate type (H-hourly, S-salary,P-Piece) */
        RTYPE                   DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Earnings code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Piece rate */
        PIECE_RATE              DATATYPE IS G_FLOATING.

        /* Element =
        Description = Hourly rate */
        HOUR_RATE               DATATYPE IS G_FLOATING.

        /* Element =
        Description = Regular hours */
        REG_HR                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Overtime hours */
        OVT_HR                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = Number of piece producted */
        PIECE                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Overtime factor */
        FACTOR                  DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Gross pay */
        GROSS                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = Tax package code */
        TAX_PKG                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Batch entry flag */
        BATCH_ENTRY             DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Update flag */
        UPDATE_FLAG             DATATYPE IS SIGNED WORD.

        /* Element = BATCH
        Description = Seq # for labor performance */
        SEQNUM                  DATATYPE IS TEXT SIZE IS 6.

        /* Element = BATCH
        Description = Batch number used for posting */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element = DATE
        Description = Work Date (YYYYMMDD) */
        WORKDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Regular Time By Day * 100 */
        REGULAR                 ARRAY 0:6 DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Overtime By Day * 100 */
        OVERTIME                ARRAY 0:6 DATATYPE IS SIGNED WORD.

        /* Element = ASSET
        Description = Asset number */
        EQUIPMENT               DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Hours of use on equipment */
        EQUIPHOUR               DATATYPE IS G_FLOATING.

        END PR_TRN_PAY_CDD STRUCTURE.

END PR_TRN_PAY.
