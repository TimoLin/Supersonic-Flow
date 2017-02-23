	SUBROUTINE  MAIN(Min,TAin,ZETAin,XEin,YEin,XLOC)
	!VB与FORTRAN接口程序
!ms$ ATTRIBUTES DLLEXPORT :: MAIN
!ms$ ATTRIBUTES ALIAS:"MAIN" :: MAIN
!ms$ ATTRIBUTES REFERENCE :: Min,TAin,ZETAin,XEin,YEin,XLOC


        IMPLICIT NONE

        REAL Min,TAin,ZETAin,XEin,YEin
        REAL XLOC

        REAL DELTA,E1,E2,E3,E4,E5,PI,RAD
        INTEGER NI,ICOR
        REAL G,RG,CP,TS,PS,PA,GL
        REAL TA,ZETA,XE,YE
        REAL MU,PU,TU,RU,QU,EPS
        REAL X(400),Y(400),P(400),R(400),Q(400),A(400)
        REAL X1,Y1,U1,V1,Q1,A1,P1,R1,T1,M1,X2,Y2,U2,V2,Q2,A2,P2,R2,T2,
     &    M2,X3,Y3,U3,V3,Q3,A3,P3,R3,T3,M3,X4,Y4,U4,V4,Q4,A4,P4,R4,T4,
     &    M4,LP,LM,LE,L12,L0


        COMMON /CONTRL/ DELTA,NI,ICOR,E1,E2,E3,E4,E5,PI,RAD
        COMMON /PROPTY/ G,RG,CP,TS,PS,PA,GL
        COMMON /CONE/ TA,ZETA,XE,YE
        COMMON /SHCKP/ MU,PU,TU,RU,QU,EPS
        COMMON /ARRAYS/ X,Y,P,R,Q,A
        COMMON /DATA/X1,Y1,U1,V1,Q1,A1,P1,R1,T1,M1,X2,Y2,U2,V2,Q2,A2,P2,
     &R2,T2,M2,X3,Y3,U3,V3,Q3,A3,P3,R3,T3,M3,X4,Y4,U4,V4,Q4,A4,P4,R4,T4,
     &M4,LP,LM,LE,L12,L0
c       NAMELIST /INFO/ DELTA,NI,ICOR,E1,E2,E3,E4,E5,G,RG,MU,PU,
c     &  TU,TA,ZETA,XE,YE,KWRITE

        INTEGER KWRITE
        REAL CU,AP,PP,EPS4,PTEMP,XTEMP

        INTEGER I,I1,IEND,IO,IR,J,J1,J2,K,KW,L,N,checker



10      DELTA=1.0
        NI=5
        ICOR=3
        E1=0.0
        E2=0.0
        E3=0.0
        E4=0.0
        E5=0.0
        G=1.4
        RG=287.04
        TU=300.0
        PU=1.01325E5

        MU=Min
        TA=TAin
        ZETA=ZETAin
        XE=XEin
        YE=YEin

        IEND=0
        KWRITE=1
        checker=0

        OPEN (UNIT=233,FILE='PvsX.dat')
        OPEN (UNIT=244,FILE='PvsX_shock.dat')
        OPEN (UNIT=255,FILE='Output.dat')
35      GL=1.0
        PI=3.1415926
        RAD=57.2957795
        TA=TA/RAD  ! 角度转弧度
        CALL BOUNDY(1)
        PU=PU*GL  !由来流静压计算密度声速及来流速度
        RU=PU/RG/TU
        CU=SQRT(G*RG*TU)
        QU=MU*CU

40      CALL IVLINE !调用超声速初值线计算程序

        DO 80 I=1,NI !从初值线出发计算流场
          J2=I
          WRITE(6,1010)
          DO 80 J=1,J2
          IF (J.GT.1) GOTO 50
          CALL MOVE(4,I)
          IF (I.EQ.1) CALL MOVE (7,0)
          KW=1
          GOTO 70
50        CALL MOVE (1,J-1)
          IF (J.EQ.J2) GOTO 60
          CALL MOVE(2,J)
          CALL INTER
          KW=KWRITE

          GOTO 70

60        CALL MOVE(6,0)
          CALL BODY
          KW=1
70        CALL MOVE(5,J)
          PP=P4/GL
          AP=A4*RAD
          IF (KW.EQ.1) THEN
            IF (J.EQ.1 .AND.X4.LT.XE) THEN
              WRITE(244,*) X4,PP/PU
            END IF
            IF (J.EQ.J1 .AND. X4.LT.XE) then
            !WRITE(233,*)  I,J,X4,Y4,U4,V4,M4,Q4,AP,PP,R4,T4
              WRITE(233,*)  X4,PP/PU
            END IF
            WRITE(255,1020) I,J,X4,Y4,U4,V4,M4,Q4,AP,PP,R4,T4
          END IF
80        CONTINUE
          I1=NI+1
          J1=1

C 计算外激波点单元
        DO 120 I=I1,1000
        CALL MOVE (1,1)
        CALL MOVE (2,2)
        CALL SHOCK(EPS,EPS4)
        EPS=EPS4
        J1=J1+1
        IF(X4.LT.XE) GOTO 85
        IF(IEND.EQ.1) THEN
            WRITE(*,*) '  Calculation Complete!'
            return
        END IF

        IEND=1
85      IR=NI+2*(I-NI-1)
        CALL MOVE (5,1)
        CALL MOVE(3,2)
        DO 120 K=1,2
          J2=J2+1
          WRITE(6,1030)
          DO 120 J=J1,J2
            L=J+3-J1-K
            N=L+K-1
            IF (J.GT.J1) GOTO 90
            CALL MOVE(4,L)
            KW=1
            GOTO 110
90          CALL MOVE(1,L-1)
            IF(J.EQ.J2) GOTO 100
            CALL MOVE(2,N)
            CALL INTER
            KW=KWRITE
            GOTO 110
100         CALL MOVE(6,0)
            CALL BODY
            KW=1
110         CALL MOVE(5,L)
            PP=P4/GL
            AP=A4*RAD
            IO=IR+K
            IF (KW.EQ.1) THEN
                WRITE(255,1020)  IO,J,X4,Y4,U4,V4,M4,Q4,AP,PP,R4,T4
                IF (J.EQ.J1.AND.X4.LT.XE) THEN
                  WRITE(244,*) X4,PP/PU
                END IF
                IF (J.EQ.J2.AND.X4.LT.XE) THEN
                  WRITE(233,*) X4,PP/PU
                  IF((PP/PU).GT.1.0) THEN
                  XTEMP=X4
                  PTEMP=PP
                  END IF
                  IF((PP/PU).LT.1.0 .AND. CHECKER.EQ.0) then
                        XLOC=X4+(XTEMP-X4)*(PU-P4)/(PTEMP-P4)
                        CHECKER=1
                  END IF

                END IF
            END IF

120     CONTINUE
C       GOTO 10

1010   FORMAT(1H1,3X,1HI,4X,1HJ,6X,1HX,9X,1HY,9X,1HU,9X,1HV,9X,1HM,9X,
     &        1HQ,9X,1HA,9X,1HP,12X,1HR,12X,1HT)
1020   FORMAT(2I5,2F10.4,2F10.1,F10.4,F10.1,F10.4,2E13.4,F10.1)
1030   FORMAT(1H2,3X,1HI,4X,1HJ,6X,1HX,9X,1HY,9X,1HU,9X,1HV,9X,1HM,9X,
     &        1HQ,9X,1HA,9X,1HP,12X,1HR,12X,1HT)
       END SUBROUTINE


       SUBROUTINE MOVE(I,J)
        IMPLICIT NONE
C      MOVE TRANSFERS DATA TO AND FROM GRID POINTS AND ARRYS
        REAL X(400),Y(400),P(400),R(400),Q(400),A(400)
        REAL X1,Y1,U1,V1,Q1,A1,P1,R1,T1,M1,X2,Y2,U2,V2,Q2,A2,P2,R2,T2,
     &    M2,X3,Y3,U3,V3,Q3,A3,P3,R3,T3,M3,X4,Y4,U4,V4,Q4,A4,P4,R4,T4,
     &    M4,LP,LM,LE,L12,L0

       COMMON /ARRAYS/ X,Y,P,R,Q,A
       COMMON /DATA/ X1,Y1,U1,V1,Q1,A1,P1,R1,T1,M1,X2,Y2,U2,V2,Q2,A2,P2,
     &R2,T2,M2,X3,Y3,U3,V3,Q3,A3,P3,R3,T3,M3,X4,Y4,U4,V4,Q4,A4,P4,R4,T4,
     &M4,LP,LM,LE,L12,L0

        INTEGER I,J
        REAL C

       GOTO(10,20,30,40,50,60,70),I
10      X1=X(J)
        Y1=Y(J)
        Q1=Q(J)
        A1=A(J)
        P1=P(J)
        R1=R(J)
        RETURN
20      X2=X(J)
        Y2=Y(J)
        Q2=Q(J)
        A2=A(J)
        P2=P(J)
        R2=R(J)
        RETURN
30      X(J)=X3
        Y(J)=Y3
        Q(J)=Q3
        A(J)=A3
        P(J)=P3
        R(J)=R3
        RETURN
40      X4=X(J)
        Y4=Y(J)
        Q4=Q(J)
        A4=A(J)
        P4=P(J)
        R4=R(J)
        RETURN
50      U4=Q4*COS(A4)
        V4=Q4*SIN(A4)
        CALL THERMO (Q4,P4,R4,T4,C,M4)!May have problem
        X(J)=X4
        Y(J)=Y4
        Q(J)=Q4
        A(J)=A4
        P(J)=P4
        R(J)=R4
        RETURN
60      X3=X2
        Y3=Y2
        Q3=Q2
        A3=A2
        P3=P2
        R3=R2
        RETURN
70      X2=X4
        Y2=Y4
        Q2=Q4
        A2=A4
        P2=P4
        R2=R4
        RETURN
      END SUBROUTINE

C supersonic
      SUBROUTINE BODY
C  计算物面上的参数

        IMPLICIT NONE
        REAL DELTA,E1,E2,E3,E4,E5,PI,RAD
        INTEGER NI,ICOR
        REAL G,RG,CP,TS,PS,PA,GL
        REAL X1,Y1,U1,V1,Q1,A1,P1,R1,T1,M1,X2,Y2,U2,V2,Q2,A2,P2,R2,T2,
     &    M2,X3,Y3,U3,V3,Q3,A3,P3,R3,T3,M3,X4,Y4,U4,V4,Q4,A4,P4,R4,T4,
     &    M4,LP,LM,LE,L12,L0

        COMMON /CONTRL/ DELTA,NI,ICOR,E1,E2,E3,E4,E5,PI,RAD
        COMMON /PROPTY/ G,RG,CP,TS,PS,PA,GL
        COMMON /DATA/X1,Y1,U1,V1,Q1,A1,P1,R1,T1,M1,X2,Y2,U2,V2,Q2,A2,P2,
     &R2,T2,M2,X3,Y3,U3,V3,Q3,A3,P3,R3,T3,M3,X4,Y4,U4,V4,Q4,A4,P4,R4,T4,
     &M4,LP,LM,LE,L12,L0

        !局部变量
        REAL M,QM,SM,T,C,P,R,Q,A,Y,R0,A0,T01,T02,TM,PD,RD,QD,AD,XD,YD
        INTEGER ITER

C  CALCULATE THE COEFFICIENTS FOR THE PREDICTOR
        ITER=0
        Q4=Q3
        P4=P3
        R4=R3
        CALL THERMO(Q1,P1,R1,T,C,M) !MAY HAVE PROBLEM
        LM=TAN(A1-ASIN(1.0/M))
        QM=SQRT(M**2-1.0)/(R1*Q1**2)
        SM=DELTA*SIN(A1)/(Y1*M*COS(A1-ASIN(1.0/M)))

C   SOLUTION OF THE FINITE DIFFERENCE EQUATIONSI
10      CALL BOUNDY(2)
        TM=-SM*(X4-X1)+QM*P1-A1
        P=0.5*(P3+P4)
        R=0.5*(R3+R4)
        Q=0.5*(Q3+Q4)
        R0=R*Q
        CALL THERMO(Q,P,R,T,C,M)
        A0=C**2
        T01=R0*Q3+P3
        T02=P3-A0*R3
        P4=(TM+A4)/QM
        Q4=(T01-P4)/R0
        R4=(P4-T02)/A0

C   FOR CONVERGENCE OF COMPLETION OF SPECIFIEC ITERATIONS
        IF (ITER.EQ.ICOR) RETURN
        IF (ITER.EQ.0) GOTO 20
        IF ((ABS(X4-XD).GT.E1).OR.(ABS(Y4-YD).GT.E1))  GOTO 20
        IF ((ABS(P4-PD).GT.E2*PD).OR.(ABS(R4-RD).GT.E3*RD))  GOTO 20
        IF ((ABS(Q4-QD).LT.E4*QD).AND.(ABS(A4-AD).LT.E5*AD)) RETURN

C   CALCULATE THE COEFFICIENTS FOR THE CORRECTOR
20      ITER=ITER+1
        PD=P4
        RD=R4
        QD=Q4
        AD=A4
        XD=X4
        YD=Y4
        P=0.5*(P1+P4)
        R=0.5*(R1+R4)
        Q=0.5*(Q1+Q4)
        A=0.5*(A1+A4)
        Y=0.5*(Y1+Y4)
        CALL THERMO(Q,P,R,T,C,M)
        LM=TAN(A-ASIN(1.0/M))
        SM=DELTA*SIN(A)/(Y*M*COS(A-ASIN(1.0/M)))
        QM=SQRT(M**2-1.0)/(R*Q**2)
        GOTO 10

        END SUBROUTINE

      SUBROUTINE BOUNDY (INITAL)
C  确定右行马赫线与曲线的交点
        !传入变量
        implicit none
        INTEGER INITAL

        REAL DELTA,E1,E2,E3,E4,E5,PI,RAD
        INTEGER NI,ICOR
        REAL TA,ZETA,XE,YE
        REAL X1,Y1,U1,V1,Q1,A1,P1,R1,T1,M1,X2,Y2,U2,V2,Q2,A2,P2,R2,T2,
     &    M2,X3,Y3,U3,V3,Q3,A3,P3,R3,T3,M3,X4,Y4,U4,V4,Q4,A4,P4,R4,T4,
     &    M4,LP,LM,LE,L12,L0

        COMMON /CONTRL/ DELTA,NI,ICOR,E1,E2,E3,E4,E5,PI,RAD
        COMMON /CONE/ TA,ZETA,XE,YE
        COMMON /DATA/X1,Y1,U1,V1,Q1,A1,P1,R1,T1,M1,X2,Y2,U2,V2,Q2,A2,P2,
     &R2,T2,M2,X3,Y3,U3,V3,Q3,A3,P3,R3,T3,M3,X4,Y4,U4,V4,Q4,A4,P4,R4,T4,
     &M4,LP,LM,LE,L12,L0


        !局部变量
        REAL TT,XA,YA,C,B,A,XB
        SAVE C,B,A,XB
        INTEGER IBODY

        GOTO (10,20),INITAL
C   INITAL=1 GOTO 10,计算物面的外形表达式
10      IF (ZETA.EQ.1.0) THEN
        TT=TAN(TA)
        YA=ZETA*YE
        XA=YA/TT
        IBODY=1
        XB=XA
        C=0.0
        B=0.0
        A=0.0

        ELSE
        TT=TAN(TA)
        YA=ZETA*YE
        XA=YA/TT
        IBODY=1
        C=-TT**2/4.0/YE/(1.0-ZETA)
        B=TT-2.0*C*XA
        A=YA-B*XA-C*XA*XA
        XB=-B/2.0/C
        END IF

        RETURN

C   INITAL=2,GOTO 20,计算右行马赫线与物面的交点

20     GOTO (30, 50),IBODY
30     IF(ZETA.EQ.1.0) GOTO 50
       X4=((LM-B)+SQRT((LM-B)**2-4.0*C*(A-Y1+LM*X1)))/(2.0*C)
       IF (X4.LT.XB) GOTO 40
       IBODY=2
       GOTO 50
40     Y4=A+B*X4+C*X4**2
       A4=ATAN(B+2.0*C*X4)
       RETURN
C   LOCATE THE INTERSECTION OF A CHARACTERISTIC AND THE CYLINDER
50     Y4=YE
       X4=X1+(Y4-Y1)/LM
       A4=0.0
       RETURN

      END SUBROUTINE


      SUBROUTINE IVLINE
C     计算超声速流动的初值线
        IMPLICIT NONE
        REAL DELTA,E1,E2,E3,E4,E5,PI,RAD
        INTEGER NI,ICOR
        REAL G,RG,CP,TS,PS,PA,GL
        REAL TA,ZETA,XE,YE
        REAL MU,PU,TU,RU,QU,EPS
        REAL X(400),Y(400),P(400),R(400),Q(400),A(400)
        REAL X1,Y1,U1,V1,Q1,A1,P1,R1,T1,M1,X2,Y2,U2,V2,Q2,A2,P2,R2,T2,
     &    M2,X3,Y3,U3,V3,Q3,A3,P3,R3,T3,M3,X4,Y4,U4,V4,Q4,A4,P4,R4,T4,
     &    M4,LP,LM,LE,L12,L0
        REAL T(400),U(400),V(400)

        COMMON /CONTRL/ DELTA,NI,ICOR,E1,E2,E3,E4,E5,PI,RAD
        COMMON /PROPTY/ G,RG,CP,TS,PS,PA,GL
        COMMON /CONE/ TA,ZETA,XE,YE
        COMMON /SHCKP/ MU,PU,TU,RU,QU,EPS
        COMMON /ARRAYS/ X,Y,P,R,Q,A
        COMMON /DATA/X1,Y1,U1,V1,Q1,A1,P1,R1,T1,M1,X2,Y2,U2,V2,Q2,A2,P2,
     &R2,T2,M2,X3,Y3,U3,V3,Q3,A3,P3,R3,T3,M3,X4,Y4,U4,V4,Q4,A4,P4,R4,T4,
     &M4,LP,LM,LE,L12,L0
        COMMON /TAYMAC/ T,U,V

        !局部变量
        REAL M,MU2,LS,LI,E,B,TANB,TANE,SINE2,FE,FES,ES,SLOPE,DE,DY,DX
        REAL AP,PP,QP,RP,AA,TANA,YA,XA,C,EP,G1,G2,TT

        INTEGER ITER,J,I,K

        IF (DELTA.EQ.1.0) GOTO 60 !轴对称流动跳转到行代码 60 处


C       平面流
        X1=0.0
        Y1=0.0
        TANA=TAN(TA)
        YA=ZETA*YE
        XA=YA/TANA
        ITER=0
        J=1
        G1=2.0/(G+1.0)
        G2=(G-1.0)/2.0
        MU2=MU*MU
        E=ASIN(1.0/MU)+TA
10      B=E-TA
        TANB=TAN(B)
        TANE=TAN(E)
        SINE2=SIN(E)**2
        FE=TANB/TANE-G1*(1.0/MU2/SINE2+G2) !斜激波关系的密度比公式7.91
        IF (ABS(FE).LT.1.0E-8) GOTO 30
        IF (ITER.GT.0) GOTO 20
        FES=FE
        ES=E
        E=0.99*E
        ITER=ITER+1
        GOTO 10
20      SLOPE=(FE-FES)/(E-ES)  !割线法确定第三次及以后所用的激波角
        DE=-FE/SLOPE
        ES=E
        FES=FE
        E=E+DE
        ITER=ITER+1
        GOTO 10
30      CALL SHCK(E) !根据斜激波关系求解激波处参数A4，Q，P4，R4
        CALL THERMO(Q4,P4,R4,T4,C,M)
        EPS=E
        WRITE(6,1030)
        LI=TAN(A4+ASIN(1.0/M))
        EP=E*RAD
        WRITE(6,1010) EP
        !WRITE(6,1000)
        LS=TAN(E)
        X4=(Y1-YA-LS*X1+LI*XA)/(LI-LS)
        Y4=Y1+LS*(X4-X1)
        DY=(Y4-YA)/FLOAT(NI-1)
        DX=DY/LI
        X4=XA-DX
        Y4=YA-DY
        AP=A4*RAD
        PP=P4/GL
        DO 50 I=1,NI
        X4=X4+DX
        Y4=Y4+DY
        CALL MOVE(5,I)
        WRITE(6,1020) I,J,X4,Y4,U4,V4,M4,Q4,AP,PP,R4,T4

50      CONTINUE
        WRITE(6,1030)
        RETURN

C       轴对称流动
60     CALL TM
       DO 70 I=1,NI
        K=I+NI
        Q(K)=Q(I)*QU
        A(K)=A(I)
        P(K)=P(I)*PU
        T(K)=T(I)
70      R(K)=R(I)*RU

        DO 80 I=1,NI
        K=2*NI+1-I
        Q(I)=Q(K)
        A(I)=A(K)
        P(I)=P(K)
        R(I)=R(K)
80      T(I)=T(K)

        Y(1)=ZETA*YE
        X(1)=Y(1)/TAN(TA)
        J=1
        !WRITE(6,1000)
        DO 100 I=1,NI
        IF(I.EQ.1) GOTO 90
        TT=TAN(T(I))
        QP=0.5*(Q(I-1)+Q(I))
        AP=0.5*(A(I-1)+A(I))
        PP=0.5*(P(I-1)+P(I))
        RP=0.5*(R(I-1)+R(I))
        AA=SQRT(G*PP/RP)
        LI=TAN(AP+ASIN(AA/QP))
        X(I)=(LI*X(1)-Y(1))/(LI-TT)
        Y(I)=TT*X(I)
90      CALL THERMO (Q(I),P(I),R(I),TT,C,M)
        AP=A(I)*RAD
        PP=P(I)/GL
        U4=Q(I)*COS(A(I))
        V4=Q(I)*SIN(A(I))
100     WRITE(6,1020) I,J,X(I),Y(I),U4,V4,M,Q(I),AP,PP,R(I),TT !初值线上点的数据
        WRITE(6,1030)
        RETURN

!1000   FORMAT(1H0,4X,1HI,4X,1HJ,6X,1HX,9X,1HY,9X,1HU,9X,1HV,9X,1HM,9X,
!&        11HQ,9X,1HA,9X,1HP,12X,1HR,12X,1HT,1HT,1H)
1010  FORMAT('THE SHOCK WAVE ANGLE EPS',F10.4,1X,'DEGREES')
1020  FORMAT(2I5,2F10.4,2F10.1,F10.4,F10.1,F10.4,2E13.4,F10.1)
1030  FORMAT(1H1)
      END SUBROUTINE

        SUBROUTINE INTER
C  calculate the solution at an interior point
        IMPLICIT NONE
        REAL DELTA,E1,E2,E3,E4,E5,PI,RAD
        INTEGER NI,ICOR
        REAL G,RG,CP,TS,PS,PA,GL
        REAL X1,Y1,U1,V1,Q1,A1,P1,R1,T1,M1,X2,Y2,U2,V2,Q2,A2,P2,R2,T2,
     &    M2,X3,Y3,U3,V3,Q3,A3,P3,R3,T3,M3,X4,Y4,U4,V4,Q4,A4,P4,R4,T4,
     &    M4,LP,LM,LE,L12,L0

        COMMON /CONTRL/ DELTA,NI,ICOR,E1,E2,E3,E4,E5,PI,RAD
        COMMON /PROPTY/ G,RG,CP,TS,PS,PA,GL
        COMMON /DATA/ X1,Y1,U1,V1,Q1,A1,P1,R1,T1,M1,X2,Y2,U2,V2,Q2,A2,
     &  P2,R2,T2,M2,X3,Y3,U3,V3,Q3,A3,P3,R3,T3,M3,X4,Y4,U4,V4,Q4,A4,P4,
     &  R4,T4,M4,LP,LM,LE,L12,L0


        REAL QP,SP,QM,SM,TP,TM,D,XC,YC,P,Q,R,R0,A0,T01,T02,XD,YD,PD,QD
        REAL RD,AD,Y,T,C,M,A
        INTEGER ITER,K
C CALCULATE THE COEFFICIENTS FOR THE PREDICTOR
        ITER=0
        L12=(Y1-Y2)/(X1-X2)
        CALL THERMO(Q2,P2,R2,T,C,M)
        LP=TAN(A2+ASIN(1.0/M))
        QP=SQRT(M**2-1.0)/(R2*Q2**2)
        SP=DELTA/(M*COS(A2+ASIN(1.0/M)))
        IF (Y2.EQ.0.0) THEN
                SP=SP*SIN(A1)/Y1
        END IF
        IF (Y2.GT.0.0) THEN
                SP=SP*SIN(A2)/Y2
        END IF
        CALL THERMO(Q1,P1,R1,T,C,M)
        LM=TAN(A1-ASIN(1.0/M))
        QM=SQRT(M**2-1.0)/(R1*Q1**2)
        SM=DELTA*SIN(A1)/(Y1*M*COS(A1-ASIN(1.0/M)))
        A3=0.5*(A1+A2) !初次预估值
        A4=A3

C SOLUTION OF THE FINITE DIFFERENCE EQUATIONS. LOCATE POINT 4
10      X4=(Y1-Y2-LM*X1+LP*X2)/(LP-LM)
        Y4=Y1+LM*(X4-X1)
        IF (Y4.LT.0.0) RETURN
        TP=-SP*(X4-X2)+QP*P2+A2
        TM=-SM*(X4-X1)+QM*P1-A1
        K=1

C LOCATE POINT 3 AND INTERPOLATE FOR THE PROPERTIES THERE
20      L0=TAN(0.5*(A3+A4)) ! 校正值
        X3=(Y4-Y2-L0*X4+L12*X2)/(L12-L0) !联立方程得到
        Y3=Y4+L0*(X3-X4)
        D=(Y3-Y2)/(Y1-Y2)
        A3=A2+D*(A1-A2)  ! 由A1，A2对A3线性插值
        IF(ITER.EQ.0) THEN
                A4=A3
        END IF
        IF (K.GT.1 .AND. ABS(Y3-YC).LT.0.000001) GOTO 30 !3点坐标求解收敛
        XC=X3
        YC=Y3
        K=K+1
        GOTO 20
30      Q3=Q2+D*(Q1-Q2) !对3点的流动参数由1，2点线性插值
        P3=P2+D*(P1-P2)
        R3=R2+D*(R1-R2)
        IF(ITER.GT.0) GOTO 40 !第二次迭代及以后跳转到40
        Q4=Q3   !初次迭代以3点参数为4点参数赋初值
        P4=P3
        R4=R3
40      P=0.5*(P3+P4)
        R=0.5*(R3+R4)
        Q=0.5*(Q3+Q4)
        R0=R*Q
        CALL THERMO(Q,P,R,T,C,M)
        A0=C**2  !沿流线相容方程的三个系数
        T01=R0*Q3+P3
        T02=P3-A0*R3

C CALCULATE THE PROPERTIES AT POINT 4 AND TEST FOR CONVERGENCE
        P4=(TP+TM)/(QP+QM)  !由P146页计算方程联立得到
        A4=TP-QP*P4
        Q4=(T01-P4)/R0
        R4=(P4-T02)/A0
        IF (ITER.EQ.ICOR) RETURN !ICOR为校正法的次数
        IF (ITER.EQ.0) GOTO 50   !如果为初次计算跳转至50
        IF ((ABS(X4-XD).GT.E1).OR.(ABS(Y4-YD).GT.E1)) GOTO 50
        IF ((ABS(P4-PD).GT.E2*PD).OR.(ABS(R4-RD).GT.E3*RD)) GOTO 50
        IF ((ABS(Q4-QD).LT.E4*QD).AND.(ABS(A4-AD).LT.E5*AD)) RETURN

C CALCULATE THE COEFFICIENTS FOR THE CORRECTOR
50      ITER=ITER+1
        XD=X4 !D为储存的上一次迭代步的值
        YD=Y4
        PD=P4
        RD=R4
        QD=Q4
        AD=A4
        P=0.5*(P2+P4) !左行马赫线的改进值
        R=0.5*(R2+R4)
        Q=0.5*(Q2+Q4)
        A=0.5*(A2+A4)
        Y=0.5*(Y2+Y4)
        CALL THERMO(Q,P,R,T,C,M)
        LP=TAN(A+ASIN(1.0/M))
        QP=SQRT(M**2-1.0)/(R*Q**2)
        SP=DELTA*SIN(A)/(Y*M*COS(A+ASIN(1.0/M)))
        P=0.5*(P1+P4) !右行马赫线的改进值
        R=0.5*(R1+R4)
        Q=0.5*(Q1+Q4)
        A=0.5*(A1+A4)
        Y=0.5*(Y1+Y4)
        CALL THERMO(Q,P,R,T,C,M)
        LM=TAN(A-ASIN(1.0/M))
        QM=SQRT(M**2-1.0)/(R*Q**2)
        SM=DELTA*SIN(A)/(Y*M*COS(A-ASIN(1.0/M)))
        GOTO 10
      END SUBROUTINE

      SUBROUTINE THERMO(Q,P,R,T,C,M)

C SUBROUTINE TO CALCULATES T C M FOR GIVEN Q P R
        REAL Q,P,R  !INTENT IN

        REAL G,RG,CP,TS,PS,PA,GL
        COMMON /PROPTY/ G,RG,CP,TS,PS,PA,GL !物性，全局变量

        REAL T,C,M  !INTENT OUT

        T=P/(RG*R) !温度
        C=SQRT(G*RG*T) !声速方程
        M=Q/C !马赫数计算
        RETURN
      END SUBROUTINE

      SUBROUTINE SHOCK(EPS1,EPS4)
        IMPLICIT NONE
        REAL DELTA,E1,E2,E3,E4,E5,PI,RAD
        INTEGER NI,ICOR
        REAL G,RG,CP,TS,PS,PA,GL
        REAL MU,PU,TU,RU,QU,EPS
        REAL X1,Y1,U1,V1,Q1,A1,P1,R1,T1,M1,X2,Y2,U2,V2,Q2,A2,P2,R2,T2,
     &    M2,X3,Y3,U3,V3,Q3,A3,P3,R3,T3,M3,X4,Y4,U4,V4,Q4,A4,P4,R4,T4,
     &    M4,LP,LM,LE,L12,L0

        COMMON /CONTRL/ DELTA,NI,ICOR,E1,E2,E3,E4,E5,PI,RAD
        COMMON /PROPTY/ G,RG,CP,TS,PS,PA,GL
        COMMON /SHCKP/ MU,PU,TU,RU,QU,EPS
        COMMON /DATA/ X1,Y1,U1,V1,Q1,A1,P1,R1,T1,M1,X2,Y2,U2,V2,Q2,A2,
     &  P2,R2,T2,M2,X3,Y3,U3,V3,Q3,A3,P3,R3,T3,M3,X4,Y4,U4,V4,Q4,A4,P4,
     &  R4,T4,M4,LP,LM,LE,L12,L0

        REAL EPS1,EPS4 !INTENT IN AND OUT
        ! 局部变量
        REAL M,LS,G1,G2,P4S,P,Q,R,A,Y,QP,SP,TP,P4LRC,DP,ES,DPS,SLOPE
        REAL DE,DEL,C,T
        INTEGER ITER



C CALCULATE THE COEFFICIENTS FOR THE PREDICTOR
        ITER=0
        EPS4=EPS1  !第一次试算4点的激波角等于1点的激波角
        G1=(G+1.0)/2.0/G
        G2=(G-1.0)/2.0/G
10      CALL SHCK(EPS4) !由子程序SHCK得到4点的P4，Q4，R4，A4
        P4S=P4 !P4S为斜激波分析所得到的值
        LS=TAN(0.5*(EPS1+EPS4))
        Q=0.5*(Q2+Q4)
        P=0.5*(P2+P4)
        R=0.5*(R2+R4)
        A=0.5*(A2+A4)
        CALL THERMO(Q,P,R,T,C,M)
        LP=TAN(A+ASIN(1.0/M))
        X4=(Y1-Y2-LS*X1+LP*X2)/(LP-LS) !由2点4点的坐标关系求4点坐标
        Y4=Y1+LS*(X4-X1)
        Y=0.5*(Y2+Y4)
        QP=SQRT(M**2-1.0)/(R*Q**2)  !由2点4点的沿马赫线相容方程求P4
        SP=DELTA*SIN(A)/(Y*M*COS(A+ASIN(1.0/M)))
        TP=-SP*(X4-X2)+QP*P2+A2
        P4LRC=(TP-A4)/QP
        DP=P4LRC-P4S  !由相容关系求得的P4与由斜激波关系求得的P4的差值

C TEST FOR CONVERGENCE
        IF(ABS(DP).LT.E2) GOTO 30
        IF (ITER.EQ.ICOR) GOTO 30
        IF (ITER.GT.0) GOTO 20
        ES=EPS4 !初次迭代赋值
        DPS=DP
        EPS4=ASIN(SQRT(G1*P4LRC/PU+G2)/MU) !根据相容关系求得的P4来求出4点激波角，用来作为第二次试算的4点激波角
        ITER=ITER+1
        GOTO 10
20      SLOPE=(DP-DPS)/(EPS4-ES) !割线法确定第三次试算所用的激波角
        DE=-DP/SLOPE
        ES=EPS4
        DPS=DP
        EPS4=EPS4+DE
        ITER=ITER+1
        GOTO 10
!结束之后求解出了4点的激波角
C 确定2，4点中间位置的3点坐标
30      DEL=0.5
        X3=X2+DEL*(X4-X2)
        Y3=Y2+DEL*(Y4-Y2)
        Q3=Q2+DEL*(Q4-Q2)
        A3=A2+DEL*(A4-A2)
        P3=P2+DEL*(P4-P2)
        R3=R2+DEL*(R4-R2)
        RETURN
      END SUBROUTINE

      SUBROUTINE SHCK(E)
        IMPLICIT NONE

C 由斜激波关系求解激波处参数 A4,P4,Q4,R4
        REAL E !INTENT IN

        REAL DELTA,E1,E2,E3,E4,E5,PI,RAD
        INTEGER NI,ICOR
        REAL G,RG,CP,TS,PS,PA,GL
        REAL MU,PU,TU,RU,QU,EPS
        REAL X1,Y1,U1,V1,Q1,A1,P1,R1,T1,M1,X2,Y2,U2,V2,Q2,A2,P2,R2,T2,
     &    M2,X3,Y3,U3,V3,Q3,A3,P3,R3,T3,M3,X4,Y4,U4,V4,Q4,A4,P4,R4,T4,
     &    M4,LP,LM,LE,L12,L0

        COMMON /CONTRL/ DELTA,NI,ICOR,E1,E2,E3,E4,E5,PI,RAD
        COMMON /PROPTY/ G,RG,CP,TS,PS,PA,GL
        COMMON /SHCKP/ MU,PU,TU,RU,QU,EPS
        COMMON /DATA/ X1,Y1,U1,V1,Q1,A1,P1,R1,T1,M1,X2,Y2,U2,V2,Q2,A2,
     &  P2,R2,T2,M2,X3,Y3,U3,V3,Q3,A3,P3,R3,T3,M3,X4,Y4,U4,V4,Q4,A4,P4,
     &  R4,T4,M4,LP,LM,LE,L12,L0

        REAL SINE,TANE,MN,TANB,BETA,SINB

        SINE=SIN(E)
        TANE=TAN(E)
        MN=(MU*SINE)**2
        TANB=TANE*2.0/(G+1.0)*(1.0/MN+(G-1.0)/2.0) !式7.91
        BETA=ATAN(TANB)
        SINB=SIN(BETA)
        A4=E-BETA
        Q4=QU*SINE/SINB*(2.0/(G+1.0)/MN+(G-1.0)/(G+1.0))
        P4=PU*2.0*G/(G+1.0)*(MN-(G-1.0)/2.0/G)
        R4=RU*TANE/TANB
        RETURN
      END SUBROUTINE

       SUBROUTINE TM

            IMPLICIT NONE
C SUBROUTINE TM CALCULATES TAYLOR-MACCOLL FLOW OVER A CONE

            REAL DELTA,E1,E2,E3,E4,E5,PI,RAD
            INTEGER NI,ICOR
            REAL X(400),Y(400),P(400),R(400),Q(400),A(400)
            REAL G,RG,CP,TSS,PS,PA,GL
            REAL T(400),U(400),V(400)
            REAL MU,PU,TU,RU,QU,EPS
            REAL TA,ZETA,XE,YE


            COMMON /CONTRL/ DELTA,NI,ICOR,E1,E2,E3,E4,E5,PI,RAD
            COMMON /ARRAYS/ X,Y,P,R,Q,A
            COMMON /PROPTY/ G,RG,CP,TSS,PS,PA,GL
            COMMON /TAYMAC/ T,U,V
            COMMON /SHCKP/ MU,PU,TU,RU,QU,EPS
            COMMON /CONE/ TA,ZETA,XE,YE

            REAL AC

            REAL M1,M1S,K1,K2,K3,K4,L1,L2,L3,L4
            REAL AP,AS,RR,PR,B,DDT,DE,DT,EP,SLOPE,TP,TS,VS,DTS,QS,ES
            INTEGER I,K,L,NILOCAL

10          M1=MU
            NILOCAL=NI
            AC=TA
            EPS=AC+0.5*ASIN(1.0/M1)
            L=0
C SOLVE FOR THE FLOW PROPERTIES BEHIND THE SHOCK WAVE

40          CALL SHK (EPS,M1,M1S,G,QS,AS,PR,RR)
            PR=PR/PRR(QS)
            RR=RR/RRR(QS)
            U(1)=QS*COS(EPS-AS)
            V(1)=-QS*SIN(EPS-AS)
            T(1)=EPS
            K=0
            I=1
            DT=-(EPS-AC)/FLOAT(NILOCAL-1)

C INTEGRATE FROM THE SHOCK WAVE TO THE CONE

50          K1=V(I)*DT
            L1=F(U(I),V(I),T(I))*DT
            K2=(V(I)+0.5*L1)*DT
            L2=F(U(I)+0.5*K1,V(I)+0.5*L1,T(I)+0.5*DT)*DT
            K3=(V(I)+0.5*L2)*DT
            L3=F(U(I)+0.5*K2,V(I)+0.5*L2,T(I)+0.5*DT)*DT
            K4=(V(I)+L3)*DT
            L4=F(U(I)+K3,V(I)+L3,T(I)+DT)*DT
            U(I+1)=U(I)+(K1+2.0*K2+2.0*K3+K4)/6.0
            V(I+1)=V(I)+(L1+2.0*L2+2.0*L3+K4)/6.0
            T(I+1)=T(I)+DT
            IF(K.GT.0) GOTO 70
            IF (V(I+1).GT.0.0) GOTO 60
            I=I+1
            GOTO 50
60          VS=V(I+1)
            DTS=DT
            DT=DT*V(I)/(V(I)-V(I+1))
            K=1
            GOTO 50
70          IF (ABS(V(I+1)).LT.1.0E-8) GOTO 80
            SLOPE=(V(I+1)-VS)/(DT-DTS)
            DDT=-V(I+1)/SLOPE
            DTS=DT
            VS=V(I+1)
            DT=DT+DDT
            GOTO 50

C ITERATE ON THE SHOCK WAVE ANGLE

80          IF (ABS(T(I+1)-AC).LT.1.0E-6) GOTO 100
            IF (L.GT.0) GOTO 90
            TS=T(I+1)
            ES=EPS
            EPS=EPS+(AC-TS)
            L=1
            GOTO 40
90          SLOPE=(T(I+1)-TS)/(EPS-ES)
            DE=(AC-T(I+1))/SLOPE
            ES=EPS
            TS=T(I+1)
            EPS=EPS+DE
            GOTO 40

c SOLUTION HAS CONVERGED

100         EP=EPS*RAD
            WRITE (6,1000) EP
            DO 110 I=1,NILOCAL
            Q(I)=SQRT(U(I)**2+V(I)**2)
            B=T(I)
         A(I)=ATAN((U(I)*SIN(B)+V(I)*COS(B))/(U(I)*COS(B)-V(I)*SIN(B)))
            P(I)=PR*PRR(Q(I))
            R(I)=RR*RRR(Q(I))
            TP=T(I)*RAD
            Q(I)=Q(I)/M1S
            AP=A(I)*RAD
110         WRITE (6,1010) I,TP,Q(I),AP,P(I),R(I)
            RETURN

1000        FORMAT ('THE SHOCK WAVE ANGLE EPS=',F10.4,1X,'DEGREES') !MAY HAVE PROBLEM

1010        FORMAT (I6,F12.3,F10.5,F11.3,2F11.4)

            CONTAINS
                FUNCTION PRR(XX)
                    REAL XX
                    REAL PRR
                    PRR=(1.0-(G-1.0)/(G+1.0)*XX*XX)**(G/(G-1.0))
                    RETURN
                END FUNCTION
                FUNCTION RRR(XX)
                    REAL XX
                    REAL RRR
                    RRR=(1.0-(G-1.0)/(G+1.0)*XX*XX)**(1.0/(G-1.0))
                    RETURN
                END FUNCTION
                FUNCTION F(XX,YY,ZZ)
                    REAL XX,YY,ZZ
                    REAL F,AA
                    AA=((G+1.0)-(G-1.0)*(XX*XX+YY*YY))/2.0
                    F=-XX+AA*(XX+YY/TAN(ZZ))/(YY*YY-AA)
                    RETURN
                END	FUNCTION

        END
        SUBROUTINE SHK (EPS,M1,M1S,G,Q,A,P,R)
            IMPLICIT NONE

C     SUBROUTINE SHK CALCULATES PROPERTY RATIOS FOR OBLIQUE SHOCK WAVE
            REAL EPS,M1,M1S,G,Q,A,P,R

            REAL MN,SINE,TANE,TANB,BETA,SINB
            M1S=M1*SQRT((G+1.0)/(2.0+(G-1.0)*M1*M1))
            SINE=SIN(EPS)
            TANE=TAN(EPS)
            MN=(M1*SINE)**2
            TANB=TANE*2.0/(G+1.0)*(1.0/MN+(G-1.0)/2.0)
            BETA=ATAN(TANB)
            SINB=SIN(BETA)
            A=EPS-BETA
            Q=SINE/SINB*(2.0/(G+1.0)/MN+(G-1.0)/(G+1.0))*M1S
            P=2.0*G/(G+1.0)*(MN-(G-1.0)/2.0/G)
            R=TANE/TANB
            RETURN
        END

