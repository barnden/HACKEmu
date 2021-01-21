! HACK disassembler written in FORTRAN95
! Compiled using GNU Fortran 9.3.0

MODULE EXCEPT
IMPLICIT NONE
    CONTAINS
    SUBROUTINE FAIL(STRING)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN) :: STRING

        WRITE (6, "(A)") TRIM(STRING)
        CALL EXIT(0)
    END SUBROUTINE FAIL
END MODULE EXCEPT

MODULE ARRAY
IMPLICIT NONE

CONTAINS
    SUBROUTINE PUSH_BACK(VEC, INPUT, CUR)
    IMPLICIT NONE
        INTEGER, INTENT(IN) :: INPUT
        INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: VEC
        INTEGER, INTENT(INOUT) :: CUR

        INTEGER, DIMENSION(:), ALLOCATABLE :: COPY
        INTEGER :: CAPACITY, I

        IF (ALLOCATED(VEC)) THEN
            CAPACITY = SIZE(VEC)
            IF (CUR + 1 .GT. CAPACITY) THEN
                ALLOCATE(COPY(CAPACITY * 2))

                DO I = 1, CAPACITY
                    COPY(I) = VEC(I)
                ENDDO

                DEALLOCATE(VEC)
                CALL MOVE_ALLOC(COPY, VEC)
            ENDIF

            CUR = CUR + 1
            VEC(CUR) = INPUT
        ELSE
            ALLOCATE(VEC(1))
            VEC(1) = INPUT
            CUR = 1
        ENDIF
    END SUBROUTINE PUSH_BACK
END MODULE ARRAY

MODULE DISASSEMBLER
USE EXCEPT
IMPLICIT NONE
    CHARACTER(LEN=3), DIMENSION(8) :: DESTS = [CHARACTER(LEN=3) :: "", "M", "D", "MD", "A", "AM", "AD", "AMD"]

    CHARACTER(LEN=3), DIMENSION(8) :: JUMPS = [CHARACTER(LEN=3) :: "", "JGT", "JEQ", "JGE", "JLT", "JNE", "JLE", "JMP"]

    CHARACTER(LEN=3), DIMENSION(28) :: COMPS = [CHARACTER(LEN=3) :: "0", "1", "-1", "D", "A", "M",&
    "!D", "!A", "!M", "-D", "-A", "-M", "D+1", "A+1", "M+1", "D-1", "A-1", "M-1", "D+A", "D+M", &
    "D-A", "D-M", "A-D", "M-D", "D&A", "D&M", "D|A", "D|M"]

    INTEGER, DIMENSION(28) :: COMP_CODES = [INTEGER :: INT(Z"2A"), INT(Z"3F"), INT(Z"3A"),&
    INT(Z"0C"), INT(Z"30"), INT(Z"70"), INT(Z"0D"), INT(Z"31"), INT(Z"71"), INT(Z"0F"), INT(Z"33"),&
    INT(Z"73"), INT(Z"1F"), INT(Z"37"), INT(Z"77"), INT(Z"0E"), INT(Z"32"), INT(Z"72"), INT(Z"02"),&
    INT(Z"42"), INT(Z"13"), INT(Z"53"), INT(Z"07"), INT(Z"47"), INT(Z"00"), INT(Z"40"), INT(Z"15"),&
    INT(Z"55")]

    CONTAINS
    SUBROUTINE DISASSEMBLE(LINES, ASSEMBLY, LINES_SZ)
        IMPLICIT NONE
        INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(IN) :: LINES
        CHARACTER(LEN=32), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: ASSEMBLY
        INTEGER, INTENT(IN) :: LINES_SZ

        CHARACTER(LEN=16) :: BUF
        CHARACTER(LEN=3) :: SEP1 = "", SEP2 = ""
        INTEGER :: I, J, LINE, CMP, JMP, DST
        LOGICAL :: FOUND = .FALSE.

        IF (ALLOCATED(ASSEMBLY)) THEN
            DEALLOCATE(ASSEMBLY)
        ENDIF
        IF (ALLOCATED(LINES)) THEN
            ALLOCATE(ASSEMBLY(LINES_SZ))
        ENDIF

        DO I=1,LINES_SZ
            BUF = ""
            SEP1 = ""
            SEP2 = ""

            LINE = LINES(I)
            IF (RSHIFT(LINE, 13) .EQ. 7) THEN
                CMP = IAND(RSHIFT(LINE, 6), INT(Z"7F"))
                DST = IAND(RSHIFT(LINE, 3), INT(Z"07")) + 1
                JMP = IAND(LINE           , INT(Z"07")) + 1

                DO J=1,28
                    IF (COMP_CODES(J) .EQ. CMP) THEN
                        BUF = COMPS(J)
                        FOUND = .TRUE.
                        EXIT
                    ENDIF
                ENDDO

                IF (.NOT. FOUND) THEN
                    CALL FAIL("ERROR: illegal HACK instruction")
                ENDIF

                IF (DST .GT. 1) THEN
                    SEP1 = "="
                ENDIF

                IF (JMP .GT. 1) THEN
                    SEP2 = ";"
                ENDIF

                WRITE (ASSEMBLY(I), "(A,A,A,A,A)") &
                TRIM(ADJUSTL(DESTS(DST))), TRIM(ADJUSTL(SEP1)), TRIM(ADJUSTL(BUF)),&
                TRIM(ADJUSTL(SEP2)), TRIM(ADJUSTL(JUMPS(JMP)))
            ELSE IF (RSHIFT(LINE, 15) .EQ. 0) THEN
                WRITE (BUF, "(I15)") LINE
                WRITE (ASSEMBLY(I), "(A,A)") "@", TRIM(ADJUSTL(BUF))
            ELSE
                CALL FAIL("ERROR: illegal HACK instruction")
            ENDIF
        ENDDO

        20 FORMAT (A)
    END SUBROUTINE DISASSEMBLE
END MODULE DISASSEMBLER

PROGRAM hack_disassembler
USE EXCEPT
USE ARRAY
USE DISASSEMBLER
IMPLICIT NONE

    INTEGER, DIMENSION(:), ALLOCATABLE :: LINES
    CHARACTER(LEN=32), DIMENSION(:), ALLOCATABLE :: ASSEMBLY
    CHARACTER(LEN=32) :: IN
    INTEGER :: INPUT, I, L, LINES_SZ = 1

    READ (5, 20) IN
    DO WHILE (IN .NE. "EOF")
        IF (LEN_TRIM(IN) .NE. 16) THEN
            CALL FAIL("ERROR: instruction bit size not 16")
        ENDIF

        L = VERIFY(IN(1:16), "01")

        IF (L .EQ. 0) THEN
            READ (IN, 40) INPUT

            CALL PUSH_BACK(LINES, INPUT, LINES_SZ)
            READ (5, 20) IN
        ELSE
            CALL FAIL("ERROR: illegal HACK instruction")
        ENDIF
    ENDDO

    CALL DISASSEMBLE(LINES, ASSEMBLY, LINES_SZ) 

    DO I = 1,LINES_SZ
        WRITE (6, 20) TRIM(ADJUSTL(ASSEMBLY(I)))
    ENDDO

    20 FORMAT (A)
    40 FORMAT (B16)
END PROGRAM hack_disassembler