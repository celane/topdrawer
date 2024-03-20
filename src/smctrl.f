C        MORTRAN 2.79 (BRACKETED KEYWORD MACROS OF 09/28/81)          
C THIS IS A COLLECTION OF SUBROUTINES -WRITTEN IN FORTRAN-                      
C THAT WILL SMOOTH A GIVEN SEQUENCE OF DATA POINTS.                             
C THE ALGORITHM IS BY J W TUKEY AND ALBERTO TUBILLIO, AND THE                   
C CODING IS MOSTLY BY ROGER CHAFFEE.                                            
C TO USE IT MAKE THE FOLLOWING CALL :                                           
C                                                                               
C             CALL SMCTRL (SMY,Y,N,LEVEL,F,E)                                   
C WHERE                                                                         
C SMY,Y,F,E ARE ONE-DIMENSIONAL ARRAYS OF BINS N AND                            
C LEVEL IS A SMOOTHING LEVEL, FROM 0 TO 5.                                      
C Y SHOULD CONTAIN THE SEQUENCE TO BE SMOOTHED,                                 
C AT THE COMPLETION OF THE CALL  Y IS LEFT UNMODIFIED AND                       
C SMY GIVES THE SMOOTHED SEQUENCE.                                              
C F AND E ARE DUMMY ARRAYS NEEDED IN THE COMPUTATION.                           
C SMOOTH CALLS IN TURN THE SUBROUTINES:                                         
C              SMRMED                                                           
C              SMRMEN                                                           
C              SMQHAN                                                           
C              SMSORT                                                           
C THESE FORM THE REST OF THE COLLECTION.                                        
C        ***********NOTES***************                                        
C Y IS ASSUMED TO BE THE SEQUENCE OF OBSERVED VALUES OF A                       
C FUNCTION  AT EQUALLY SPACED INTERVALS.                                        
C THE SMOOTHER PROCEDURE IS A NON-LINEAR ONE WHICH SO FAR                       
C HAS PROVED TO BE THE BEST AMONG SEVERAL ONES CURRENTLY                        
C STUDIED  AT SLAC.                                                             
C AS OPPOSED TO LINEAR PROCEDURES ITS PERFORMANCE SHOULDN'T                     
C BE IMPAIRED IF THE DATA IS NOT WELL BEHAVED.                                  
      SUBROUTINE SMCTRL (G,D,K,LEVEL,F,E)                                       
      DIMENSION G(K),D(K),F(K),E(K)                                             
      DIMENSION N1LEV(6),N2LEV(6)                                               
      DATA N1LEV(1),N1LEV(2),N1LEV(3),N1LEV(4),                                 
     X   N1LEV(5),N1LEV(6) /1,3,5,7,9,11/                                       
      DATA N2LEV(1),N2LEV(2),N2LEV(3),N2LEV(4),                                 
     X   N2LEV(5),N2LEV(6) /1,1,3,3,5,5/                                        
C                                                                               
      IF (K.LE.0) RETURN                                                        
      L = MAX0(0,MIN0(5,LEVEL)) + 2                                             
    1 L = L-1                                                                   
      N1 = N1LEV(L)                                                             
      N2 = N2LEV(L)                                                             
      IF (N1.GT.K) GO TO 1                                                      
ccc      CALL SMRMED (G,D,K,N1)			! INcorrect for sparse data JC                
c                                                                               
c	The problem comes in with data that has counts in some                        
c	channels but none in adjacent channels.   The non zero data is deleted!       
c                                                                               
      CALL SMRMEN (G,D,K,N1)                                                    
         IF (K.LT.3) RETURN                                                     
C                 ENDPOINTS ARE SPECIAL                                         
C                 USE G(1)=MEDIAN(D(1),G(2),3G(2)-2G(3))                        
ccc         E(1)=D(1)                                                           
ccc         E(2)=G(2)                                                           
ccc         E(3)=3.*G(2)-2.*G(3)                                                
ccc         CALL SMSORT(E,3)                                                    
ccc         G(1)=E(2)                                                           
ccc         E(1)=D(K)                                                           
ccc         E(2)=G(K-1)                                                         
ccc         E(3)=3.*G(K-1)-2.*G(K-2)                                            
ccc         CALL SMSORT(E,3)                                                    
ccc         G(K)=E(2)                                                           
c                                                                               
c	Extrapolate a 3 point straight line to end point                              
c                                                                               
	G(1)=(D(1)+D(2)+D(3))/3+(D(1)-D(3))/2                                          
	G(K)=(D(K)+D(K-1)+D(K-2))/3+(D(K)-D(K-2))/2                                    
      CALL SMRMED (F,G,K,N2)                                                    
      CALL SMQHAN(G,F,K)                                                        
      DO 2 KR=1,K                                                               
      F(KR)=D(KR)-G(KR)                                                         
    2 E(KR) = F(KR)                                                             
C                 G HAS SMOOTH. E AND F HAVE RESIDUALS                          
C                            GET 25-PERCENTILE POINTS                           
      CALL SMSORT(E,K)                                                          
      KM = MAX0(1,K/4)                                                          
      DQU = E(KM)                                                               
      I=K+1-KM                                                                  
      UQU = E(I)                                                                
      STEP = AMAX1(0.2,1.5*(UQU-DQU))                                           
      STEP2=STEP+STEP                                                           
C              CHANGE LARGE RESIDUALS                                           
      DO 5 I=1,K                                                                
         IF (ABS(F(I)).GT.STEP) F(I)=SIGN(DIM(STEP2,ABS(F(I))),F(I))            
    5 CONTINUE                                                                  
C              SMOOTH THE CHANGED RESIDUALS                                     
      CALL SMRMEN (E,F,K,N1)                                                    
      CALL SMRMED (F,E,K,N2)                                                    
      CALL SMQHAN  (E,F,K)                                                      
C              ADD THEM BACK TO THE FIRST SMOOTHED VALUES                       
      DO 6 KR=1,K                                                               
    6 G(KR)=G(KR)+E(KR)                                                         
C                 END OF SMOOTH.  DO ANOTHER, W/O CHANGING RESIDUALS            
      CALL SMRMEN (E,G,K,N1)                                                    
      CALL SMRMED (F,E,K,N2)                                                    
      CALL SMQHAN (E,F,K)                                                       
      DO 7 KR=1,K                                                               
    7 F(KR)=G(KR)-E(KR)                                                         
      CALL SMRMEN (G,F,K,N1)                                                    
      CALL SMRMED (F,G,K,N2)                                                    
      CALL SMQHAN (G,F,K)                                                       
      DO 8 KR=1,K                                                               
    8 G(KR)=E(KR)+G(KR)                                                         
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SMQHAN (G,F,K)                                                 
C  QUADRATIC INTERPOLATION AND HANNING                                          
      DIMENSION G(K),F(K)                                                       
      IF (K.LE.4) GO TO 10                                                      
      G(1)=F(1)                                                                 
      G(2)=(F(1)+F(2)+F(2)+F(3))*0.25                                           
      G(3)=(F(2)+F(3)+F(3)+F(4))*0.25                                           
      K1=K-1                                                                    
      DO 2 I=4,K1                                                               
      IF (F(I-2).NE.F(I-1)) GOTO 1                                              
      IF (F(I-1).NE.F(I))   GOTO 1                                              
      IF ((F(I-2).GE.F(I-3)).AND.(F(I+1).GE.F(I))) GOTO 1                       
      IF ((F(I-2).LE.F(I-3)).AND.(F(I+1).LE.F(I))) GOTO 1                       
C          SPECIAL CASE.  F(I-3)<=F(I-2)=F(I-1)=F(I)<=F(I+1)  (OR >=)           
C          RECALCULATE SMOOTHED VALUES USING CUBIC FIT                          
      G(I-2)=F(I-2)                                                             
      G(I-1)=(4.0*(F(I-2)+F(I))-F(I-3)-F(I+1))/6.0                              
      G(I)=F(I)                                                                 
      GOTO 2                                                                    
C          NORMAL CASE (ALL OTHERS)  CALCULATE USING QUADRATIC FIT              
    1 G(I)=(F(I-1)+F(I)+F(I)+F(I+1))*0.25                                       
    2 CONTINUE                                                                  
      G(K)=F(K)                                                                 
      RETURN                                                                    
   10 DO 11 I=1,K                                                               
   11 G(I) = F(I)                                                               
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SMSORT(A,N)                                                    
C        PUTS A IN ASCENDING ORDER                                              
      INTEGER J,M,N,IDONE,IPASS                                                 
      REAL A(N),T                                                               
      IF (N.LE.1) GO TO 3                                                       
         M = N                                                                  
         DO 2 IPASS=2,N                                                         
            M = M - 1                                                           
            IDONE = 1                                                           
            DO 1 J=1,M                                                          
               IF (A(J) .LE. A(J+1)) GO TO 1                                    
                  IDONE = 0                                                     
                  T = A(J)                                                      
                  A(J) = A(J+1)                                                 
                  A(J+1) = T                                                    
 1          CONTINUE                                                            
            IF (IDONE.NE.0) GO TO 3                                             
 2       CONTINUE                                                               
 3    CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SMRMED (G,F,K,L)                                               
C     THIS SUBROUTINE CALCULATES RUNNING MEDIANS OF LENGTH LEN (LE 11)          
C     FROM THE ARRAY F AND PUTS THEM IN G.  F AND G MAY NOT                     
C     OCCUPY THE SAME CORE LOCATIONS.                                           
C       A GENERAL ROUTINE WOULD REQUIRE TESTS THAT 0.LT.L.LE.K                  
C       AND L.LT.(DIMENSION OF B)  AND L=ODD.                                   
C       THIS IS ASSURED IN SMCTRL, AND NOT TESTED HERE.                         
      DIMENSION   B(12),G(K),F(K)                                               
      M=L/2                                                                     
      MP1 = M+1                                                                 
      MP2 = MP1+1                                                               
C                            FIRST M+1 VALUES                                   
      DO 10 I=1,L                                                               
   10 B(I)=F(I)                                                                 
      KM = -1                                                                   
      DO 20 I=1,MP1                                                             
         KM=KM+2                                                                
         CALL SMSORT(B,KM)                                                      
         G(I) = B(I)                                                            
   20 CONTINUE                                                                  
C                            VALUES M+2....K-M                                  
      K1=K-L                                                                    
      IF (K1.EQ.0) GO TO 65                                                     
      DO 60 KR=1,K1                                                             
         TEMP = F(KR)                                                           
         DO 40 J=1,L                                                            
            IF (B(J).NE.TEMP) GO TO 40                                          
               I=J                                                              
               GO TO 50                                                         
   40    CONTINUE                                                               
C                            IMPOSSIBLE SITUATION........                       
         CALL T2STOP('SUBR SMRMED')                                             
C              B(I) IS F(KR).  MOVE B(1) TO B(I-1) INTO B(2) TO B(I)            
C              (AND LEAVE B(1) VACANT)                                          
   50    IF (I.EQ.1) GO TO 54                                                   
   53       B(I)=B(I-1)                                                         
            I=I-1                                                               
            IF (I.NE.1) GO TO 53                                                
C              NOW PUT F(KR+L) INTO THE LIST WHERE IT BELONGS                   
   54    J=0                                                                    
C              PUT A VALUE AT THE END OF THE LIST, TO MAKE SURE                 
C              IT DOESNT RUN OVER.                                              
         KRX=KR+L                                                               
         B(L+1) = F(KRX)                                                        
   55    J=J+1                                                                  
         B(J) = B(J+1)                                                          
         IF (B(J).LT.B(L+1)) GO TO 55                                           
         B(J)=B(L+1)                                                            
         KRX=KR+MP1                                                             
         G(KRX)=B(MP1)                                                          
   60 CONTINUE                                                                  
C                            LAST M+1 VALUES                                    
   65 DO 70 I=1,L                                                               
         KM=K+1-I                                                               
         B(I)=F(KM)                                                             
   70 CONTINUE                                                                  
      I = 1                                                                     
      J = K                                                                     
      DO 80 KM = 1,L,2                                                          
         CALL SMSORT(B,KM)                                                      
         G(J) = B(I)                                                            
         J = J - 1                                                              
         I = I + 1                                                              
   80 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SMRMEN (G,F,N,L)                                               
C     THIS SUBROUTINE CALCULATES RUNNING MEANS OF LENGTH L FROM                 
C     THE ARRAY F AND PUTS THE RESULT IN G,NO CALLS ARE MADE                    
C       A GENERAL ROUTINE WOULD REQUIRE TESTS THAT 0.LT.L.LE.N                  
C       AND L=ODD.                                                              
C       THIS IS ASSURED IN SMCTRL, AND NOT TESTED HERE.                         
      DIMENSION F(N),G(N)                                                       
      M=L/2                                                                     
C     THE FIRST AND LAST M POSITIONS OF G ARE FILLED WITH THE                   
C     MEANS OF THE FIRST (LAST) 1,3,...L-2 ELEMENTS OF Y                        
      G(1) = F(1)                                                               
      TEMP1 = F(1)                                                              
      G(N) = F(N)                                                               
      TEMP2 = F(N)                                                              
      K = 1                                                                     
      IF (M.EQ.0) GO TO 5                                                       
      DO 2 I=1,M                                                                
         III = N-K                                                              
         TEMP2 = TEMP2 + F(III) + F(III-1)                                      
         K = K + 2                                                              
         TEMP1 = TEMP1 + F(K-1) + F(K)                                          
         RECIP = 1./FLOAT(K)                                                    
         G(I+1) = TEMP1*RECIP                                                   
         III = N-I                                                              
         G(III) = TEMP2*RECIP                                                   
    2 CONTINUE                                                                  
C                                                                               
    5 NMID = N-L-1                                                              
      IF (NMID.LE.0) GO TO 20                                                   
      DO 10 I=1,NMID                                                            
         III = I+K                                                              
         TEMP1 = TEMP1 + F(III) - F(I)                                          
         III=I+M                                                                
         G(III+1) = TEMP1*RECIP                                                 
   10 CONTINUE                                                                  
   20 RETURN                                                                    
      END                                                                       
